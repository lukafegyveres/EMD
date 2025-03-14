
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


#, "P2", "P4", "PCSC", "arr_par_sup", "P7", "lc_par_hab"


# Sélection des variables utiles
data_deter_utile <- data_deter[, c("ZFD", "ECH", "PER", "D11", "PCSC", "P7", "arr_par_sup", "MODP_LIB")]

# Suppression des doublons sur les identifiants individuels
data_deter_utile <- data_deter_utile %>% distinct(ZFD, ECH, PER, .keep_all = TRUE)

# Suppression des identifiants individuels
data_deter_utile <- data_deter_utile %>% select(-ZFD, -ECH, -PER)

data_deter_utile$MODP_LIB <- as.factor(data_deter_utile$MODP_LIB)

# Convertir certaines variables en facteurs
data_deter_utile$P7 <- as.factor(data_deter_utile$P7)
data_deter_utile <- data_deter_utile %>%
  mutate(
    #PCSC_1 = ifelse(PCSC == 1, 1, 0),
    #PCSC_2 = ifelse(PCSC == 2, 1, 0),
    #PCSC_7 = ifelse(PCSC == 7, 1, 0), 
    P7 = ifelse(P7 == 1, 1, 0),
    
  ) %>%
  select(-PCSC) 

# Séparation en train/test (80% train, 20% test) en respectant la répartition des classes
set.seed(123)
trainIndex <- createDataPartition(data_deter_utile$MODP_LIB, p = 0.8, list = FALSE)

train_set <- data_deter_utile[trainIndex, ]
test_set  <- data_deter_utile[-trainIndex, ]

# Ajouter MODP_LIB (binaire)
train_encoded <- train_set
test_encoded  <- test_set

# Gérer les valeurs manquantes
train_encoded <- na.omit(train_encoded)
test_encoded  <- na.omit(test_encoded)

# Séparation des variables explicatives et de la variable cible
X_train <- train_encoded %>% select(-MODP_LIB)
X_test  <- test_encoded %>% select(-MODP_LIB)
Y_train <- train_encoded$MODP_LIB
Y_test  <- test_encoded$MODP_LIB

# Normalisation des variables numériques
normalisation <- preProcess(X_train, method = c("center", "scale"))
X_train_norm <- predict(normalisation, X_train)
X_test_norm  <- predict(normalisation, X_test)

# Modèle KNN classification binaire
knn_pred <- knn(
  train = X_train_norm, 
  test  = X_test_norm, 
  cl    = Y_train,  
  k     = 7
)

# Calcul de l'accuracy
accuracy <- sum(knn_pred == Y_test) / length(Y_test) * 100
print(paste("Accuracy (%) :", round(accuracy, 2)))

# Matrice de confusion
conf_matrix <- confusionMatrix(knn_pred, Y_test, positive = "1")  # On définit "1" (TCU) comme la classe positive
print(conf_matrix)



