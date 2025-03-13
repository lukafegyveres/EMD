# Sélection des variables utiles
data_deter_utile <- data_deter[, c("ZFD", "ECH", "PER", "D11", "P7", "D4", "PCSC", "P2", "P4", "lc_par_hab", "arr_par_sup", "MODP_LIB", "MOIS")]

# Suppression des doublons sur les identifiants individuels
data_deter_utile <- data_deter_utile %>% distinct(ZFD, ECH, PER, .keep_all = TRUE)

# Assurez-vous que MODP_LIB est un facteur (si ce n'est pas déjà le cas)
data_deter_utile$MODP_LIB <- as.factor(data_deter_utile$MODP_LIB)

data_deter_utile <- subset(data_deter_utile, data_deter_utile$arr_par_sup > 0)


# Créer le box plot avec échelle logarithmique sur l'axe y
ggplot(data_deter_utile, aes(x = MODP_LIB, y = D11)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  scale_y_log10() +  # Applique une échelle logarithmique
  labs(title = "Distances en fonction du mode de déplacement",
       x = "Mode de déplacement",
       y = "Distance (échelle logarithmique)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pour mieux visualiser les labels


# Créer le box plot avec échelle logarithmique sur l'axe y
ggplot(data_deter_utile, aes(x = MODP_LIB, y = D4)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Heure de départ en fonction du mode de déplacement",
       x = "Mode de déplacement",
       y = "Heure de départ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pour mieux visualiser les labels

# Créer le box plot avec échelle logarithmique sur l'axe y
ggplot(data_deter_utile, aes(x = MODP_LIB, y = P4)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Age en fonction du mode de déplacement",
       x = "Mode de déplacement",
       y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pour mieux visualiser les labels

# Créer le box plot avec échelle logarithmique sur l'axe y
ggplot(data_deter_utile, aes(x = MODP_LIB, y = arr_par_sup)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Densité des arrêts de TCU en fonction du mode de déplacement",
       x = "Mode de déplacement",
       y = "Nombre d'arrêts par km2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pour mieux visualiser les labels


# Créer une table de correspondance entre les codes de PCSC (0 à 9) et leurs intitulés
pcsc_labels <- c(
  "0" = "Non réponse",
  "1" = "Agriculteurs exploitants",
  "2" = "Artisans, commerçants et chefs d'entreprise",
  "3" = "Cadres et professions intellectuelles supérieures",
  "4" = "Professions intermédiaires",
  "5" = "Employés",
  "6" = "Ouvriers",
  "7" = "Élèves, étudiants",
  "8" = "Chômeurs n'ayant jamais travaillé",
  "9" = "Autres inactifs n'ayant jamais travaillé"
)

data_deter_utile$PCSC <- as.character(data_deter_utile$PCSC)  # Assurer que PCSC est un caractère


# S'assurer que PCSC est un facteur et appliquer les labels correspondants
data_deter_utile$PCSC <- factor(data_deter_utile$PCSC, levels = names(pcsc_labels))


# Créer un bar plot empilé et normalisé avec les intitulés des catégories socio-professionnelles
ggplot(data_deter_utile, aes(x = as.factor(PCSC), fill = MODP_LIB)) +
  geom_bar(position = "fill") +  # "fill" pour empiler et normaliser à 1
  labs(title = "Proportion d'Utilisation du Mode de Déplacement en fonction de la PCSC",
       x = "Catégorie Socio-Professionnelle (PCSC)",
       y = "Proportion d'Utilisation") +
  scale_y_continuous(labels = scales::percent) +  # Afficher les y en pourcentage
  scale_x_discrete(labels = pcsc_labels) +  # Remplacer les codes PCSC par leurs intitulés
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pour améliorer la lisibilité des labels


permis_labels <- c(
  "1" = "Oui",
  "2" = "Non", 
  "3" = "En cours"
)

data_deter_utile$P7 <- as.character(data_deter_utile$P7)  # Assurer que PCSC est un caractère

data_deter_utile$P7 <- factor(data_deter_utile$P7, levels = names(permis_labels))

# Créer un bar plot empilé et normalisé avec les intitulés des catégories socio-professionnelles
ggplot(data_deter_utile, aes(x = as.factor(P7), fill = MODP_LIB)) +
  geom_bar(position = "fill") +  # "fill" pour empiler et normaliser à 1
  labs(title = "Proportion d'Utilisation du Mode de Déplacement en fonction de la PCSC",
       x = "Possession du permis de conduire",
       y = "Proportion d'Utilisation") +
  scale_y_continuous(labels = scales::percent) +  # Afficher les y en pourcentage
  scale_x_discrete(labels = permis_labels) +  # Remplacer les codes PCSC par leurs intitulés
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pour améliorer la lisibilité des labels

sexe_labels <- c(
  "1" = "Masculin",
  "2" = "Féminin"
)

data_deter_utile$P2 <- as.character(data_deter_utile$P2)  # Assurer que PCSC est un caractère

data_deter_utile$P2 <- factor(data_deter_utile$P2, levels = names(sexe_labels))

# Créer un bar plot empilé et normalisé avec les intitulés des catégories socio-professionnelles
ggplot(data_deter_utile, aes(x = as.factor(P2), fill = MODP_LIB)) +
  geom_bar(position = "fill") +  # "fill" pour empiler et normaliser à 1
  labs(title = "Proportion d'Utilisation du Mode de Déplacement en fonction de la PCSC",
       x = "Possession du permis de conduire",
       y = "Proportion d'Utilisation") +
  scale_y_continuous(labels = scales::percent) +  # Afficher les y en pourcentage
  scale_x_discrete(labels = sexe_labels) +  # Remplacer les codes PCSC par leurs intitulés
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pour améliorer la lisibilité des labels

# Créer un bar plot empilé et normalisé avec les intitulés des catégories socio-professionnelles
ggplot(data_deter_utile, aes(x = as.factor(MOIS), fill = MODP_LIB)) +
  geom_bar(position = "fill") +  # "fill" pour empiler et normaliser à 1
  labs(title = "Proportion d'Utilisation du Mode de Déplacement en fonction de la PCSC",
       x = "Possession du permis de conduire",
       y = "Proportion d'Utilisation") +
  scale_y_continuous(labels = scales::percent) +  # Afficher les y en pourcentage
  scale_x_discrete() +  # Remplacer les codes PCSC par leurs intitulés
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pour améliorer la lisibilité des labels


