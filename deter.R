
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




# Sélectionner les variables à inclure (y compris les nouvelles)
data_deter_utile <- data_deter[, c("lc_par_hab", "D11", "P2", "P4", "PCSC", "P7", "POP", "MODP_LIB")]

# Convertir MODP_LIB en facteur (si ce n'est pas déjà fait)
data_deter_utile$MODP_LIB <- as.factor(data_deter_utile$MODP_LIB)

# Convertir P2 (sexe) et PCSC (catégorie socio-professionnelle) en facteurs
data_deter_utile$P2 <- as.factor(data_deter_utile$P2)
data_deter_utile$PCSC <- as.factor(data_deter_utile$PCSC)

# Créer des variables binaires (One-Hot Encoding) pour P2 et PCSC seulement
dummies <- dummyVars(~ P2 + PCSC + P7 + lc_par_hab + D11 + P4 + POP, data = data_deter_utile, fullRank = TRUE)
data_encoded <- predict(dummies, newdata = data_deter_utile) %>% as.data.frame()

# Ajouter MODP_LIB de façon séparée (sans transformation)
data_encoded$MODP_LIB <- data_deter_utile$MODP_LIB


# Séparer les données en train/test (80% train, 20% test)
set.seed(123)
trainIndex <- createDataPartition(data_encoded$MODP_LIB, p = 0.8, list = FALSE)
train_data <- data_encoded[trainIndex, ]
test_data <- data_encoded[-trainIndex, ]

# Gérer les NA
train_data <- na.omit(train_data)
test_data  <- na.omit(test_data)

X_train <- train_data %>% select(-"MODP_LIB")
X_test <- test_data %>% select(-"MODP_LIB")
Y_train <- train_data %>% select("MODP_LIB")
Y_test <- test_data %>% select("MODP_LIB")

# Normalisation des variables numériques
normalisation <- preProcess(X_train, method = c("center", "scale"))
X_train_norm <- predict(normalisation, X_train)
X_test_norm  <- predict(normalisation, X_test)

# Modèle KNN classification
knn_pred <- knn(
  train = X_train_norm, 
  test  = X_test_norm, 
  cl = Y_train$MODP_LIB,  
  k = 11
)

# Calcul de l'accuracy
accuracy <- sum(knn_pred == Y_test$MODP_LIB) / length(Y_test$MODP_LIB) * 100
print(paste("Accuracy (%) :", round(accuracy, 2)))

# Matrice de confusion pour voir les erreurs
conf_matrix <- confusionMatrix(knn_pred, Y_test$MODP_LIB)
print(conf_matrix)
