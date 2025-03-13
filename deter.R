
dico_df <- read.csv("dico_mode_dep_simpl.csv", sep = ",", stringsAsFactors = FALSE)
df_domtrav_deter <- domtrav %>%
  left_join(dico_df, by = c("MODP" = "CODE"))

df2 <- donnees_tcu_agglo

df2$dat_date.x <- as.Date(df2$dat_date.x, format = "%d/%m/%Y")


# Ajouter une colonne "ANNEE" à df2 pour extraire l'année de dat_date.x
df2 <- df2 %>%
  mutate(ANNEE = format(dat_date.x, "%Y") %>% as.numeric())

df1 <- df_domtrav_deter

df1 <- df1 %>%
  mutate(ANNEE_ENQUETE = as.numeric(ANNEE_ENQUETE))  # S'assurer qu'elle est bien numérique


# Trouver la date la plus proche pour chaque AGGLO et ANNEE_ENQUETE
df_closest <- df2 %>%
  left_join(df1, by = "AGGLO") %>%  # Joindre pour comparer les années
  mutate(diff = abs(ANNEE - ANNEE_ENQUETE)) %>%  # Calculer la différence absolue d'années
  group_by(AGGLO, ANNEE_ENQUETE) %>%
  slice_min(order_by = diff) %>%  # Garder la date avec la différence minimale
  ungroup()

data_deter <- merge(x = df_closest, y = data_men, by.x = c("AGGLO","ANNEE_ENQUETE","ZFD", "ECH"), by.y = c("AGGLO","ANNEE_ENQUETE","ZFM", "ECH"))



# Sélectionner les variables utiles (y compris identifiants individuels)
data_deter_utile <- data_deter[, c("ZFD", "ECH", "PER", "lc_par_hab", "D11", "P2", "P4", "PCSC", "P7", "MODP_LIB")]

# Convertir MODP_LIB en facteur
data_deter_utile$MODP_LIB <- as.factor(data_deter_utile$MODP_LIB)

# Convertir certaines variables en facteurs
data_deter_utile$P2 <- as.factor(data_deter_utile$P2)
data_deter_utile$PCSC <- as.factor(data_deter_utile$PCSC)
data_deter_utile$P7 <- as.factor(data_deter_utile$P7)

# Créer un identifiant unique par individu
data_deter_utile <- data_deter_utile %>%
  mutate(ID_personne = paste(ZFD, ECH, PER, sep = "_"))

# Extraire les individus uniques
personnes_uniques <- unique(data_deter_utile$ID_personne)

# Séparer en train/test (80% train, 20% test)
set.seed(123)
personnes_train <- sample(personnes_uniques, size = round(0.8 * length(personnes_uniques)))

# Filtrer les déplacements selon les individus sélectionnés
train_set <- data_deter_utile %>% filter(ID_personne %in% personnes_train)
test_set  <- data_deter_utile %>% filter(!ID_personne %in% personnes_train)

# Supprimer l'ID_personne
train_set <- train_set %>% select(-ID_personne, -ZFD, -ECH, -PER)
test_set  <- test_set %>% select(-ID_personne, -ZFD, -ECH, -PER)

# One-Hot Encoding pour P2, PCSC et P7
dummies <- dummyVars(~ P2 + PCSC + P7 + lc_par_hab + D11 + P4, data = train_set, fullRank = TRUE)
train_encoded <- predict(dummies, newdata = train_set) %>% as.data.frame()
test_encoded  <- predict(dummies, newdata = test_set) %>% as.data.frame()

# Ajouter MODP_LIB séparément
train_encoded$MODP_LIB <- train_set$MODP_LIB
test_encoded$MODP_LIB  <- test_set$MODP_LIB

# Gérer les NA
train_encoded <- na.omit(train_encoded)
test_encoded  <- na.omit(test_encoded)

# Séparer X et Y
X_train <- train_encoded %>% select(-MODP_LIB)
X_test  <- test_encoded %>% select(-MODP_LIB)
Y_train <- train_encoded$MODP_LIB
Y_test  <- test_encoded$MODP_LIB

# Normalisation des variables numériques
normalisation <- preProcess(X_train, method = c("center", "scale"))
X_train_norm <- predict(normalisation, X_train)
X_test_norm  <- predict(normalisation, X_test)

# Modèle KNN classification
knn_pred <- knn(
  train = X_train_norm, 
  test  = X_test_norm, 
  cl    = Y_train,  
  k     = 10
)

# Calcul de l'accuracy
accuracy <- sum(knn_pred == Y_test) / length(Y_test) * 100
print(paste("Accuracy (%) :", round(accuracy, 2)))

# Matrice de confusion
conf_matrix <- confusionMatrix(knn_pred, Y_test)
print(conf_matrix)
