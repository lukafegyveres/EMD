setwd("C:/Users/lukaf/OneDrive - Ecole Polytechnique/Bureau/Polytechnique/3A/P2/HSS Données et méthodes quantitatives/EMD")

library(dplyr)
library(ggplot2)
library(forcats)
library(FactoMineR)
library(factoextra)
library(ggrepel)


load("data/domtrav.RData")

#certaines villes enquêtées deux fois

domtrav %>%
  distinct(AGGLO, ANNEE_ENQUETE) %>%
  group_by(AGGLO) %>%
  filter(n_distinct(ANNEE_ENQUETE) > 1) %>%
  arrange(AGGLO)

#conservation des enquêtes de moins de 10 ans pour unicité et pertinence
domtrav_recents <- subset(domtrav, domtrav$ANNEE_ENQUETE > 2015)

#31 agglomérations
domtrav_recents %>% distinct(AGGLO, ANNEE_ENQUETE)

#distance domicile-travail par agglo

distance_domtrav_recents <- domtrav_recents[,c("AGGLO","D11","D12")]

#Affichage sous forme de boxplot

ggplot(data=distance_domtrav_recents, aes(x=fct_reorder(AGGLO, D11,
  .fun = median, .desc = FALSE), y=D11/1000)) +
  geom_boxplot(fill = "skyblue", outlier.shape = NA) +
  coord_cartesian(ylim=c(0,30)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution des distances domicile-travail dans différentes agglomérations",
       x = "Agglomération",
       y = "Distance domicile-travail à vol d'oiseau (km)")

#Analyse des modes de déplacement par agglo
modp_domtrav_recents <- domtrav_recents[,c("AGGLO","MODP")]

#On remplace les modes numériques par le nom du mode
dico_df <- read.csv("dico_mode_dep_simpl.csv", sep = ",", stringsAsFactors = FALSE)
modp_domtrav_recents <- modp_domtrav_recents %>%
  left_join(dico_df, by = c("MODP" = "CODE")) %>%
  select(-MODP) %>%
  rename(MODP = MODP_LIB)

#comptage des déplacements par type et par agglo 
modp_domtrav_recents_count <- modp_domtrav_recents %>%
  group_by(AGGLO, MODP) %>%
  summarise(COUNT = n(), .groups = "drop")

##Affichage sous forme de bar plot

# Calculer la proportion maximale pour chaque agglomération
modp_domtrav_recents_count %>%
  group_by(AGGLO) %>%
  mutate(max_proportion = max(COUNT / sum(COUNT))) %>%
  ungroup() %>%
  # Classer les agglomérations par la proportion maximale
  mutate(AGGLO = fct_reorder(AGGLO, max_proportion, .desc = FALSE)) %>%
  ggplot(aes(x = AGGLO, y = COUNT, fill = MODP)) +
  geom_bar(stat = "identity", position = "fill") +  # "fill" pour normaliser les proportions
  scale_y_continuous(labels = scales::percent) +    # Affichage en pourcentage
  labs(title = "Répartition des modes de déplacement dans différentes agglomération",
       x = "Agglomération",
       y = "Proportion",
       fill = "Mode de déplacement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# pondération : même poids pour chaque trajet

# médiane et dispersion pour chaque agglo

variables_agglo <- distance_domtrav_recents %>%
  group_by(AGGLO) %>%
  summarize(
    mediane_ddt = median(D11, na.rm = TRUE),
    Q1_ddt = quantile(D11, 0.25, na.rm = TRUE), 
    Q3_ddt = quantile(D11, 0.75, na.rm = TRUE),
    interquartile_ddt = Q3_ddt - Q1_ddt,
    dispersion_relative_ddt = (Q3_ddt - Q1_ddt)/mediane_ddt
  ) %>%
  arrange(mediane_ddt)  # Trier selon la médiane

# Proportion des déplacements en voiture par agglomération
proportion_voiture <- modp_domtrav_recents_count %>%
  group_by(AGGLO) %>%
  summarize(proportion_voiture = max(COUNT / sum(COUNT), na.rm = TRUE))  # On agrège pour éviter les doublons

variables_agglo <- variables_agglo %>%
  left_join(proportion_voiture, by = "AGGLO")

# longueur commerciale par habitant de chaque agglo

load("data/donnees_agglo.RData")

variables_agglo <- variables_agglo %>%
  mutate(
    lc_par_hab = case_when(
      AGGLO == "gap" ~ 5.062216,
      AGGLO == "ajaccio" ~ 4.999940,
      AGGLO == "sables-dolonne" ~ 2.079867,
      AGGLO == "reims" ~ 1.1014166,
      AGGLO == "dijon" ~ 2.141226,
      AGGLO == "annecy" ~ 11.074030,
      AGGLO == "besancon" ~ 0.2846113,
      AGGLO == "niort" ~ 10.318583,
      AGGLO == "evreux" ~ 2.118967,
      AGGLO == "cherbourg" ~ 2.667542,
      AGGLO == "alencon" ~ 0.8728821,
      AGGLO == "lille" ~ 1.195198,
      AGGLO == "grenoble" ~ 1.0516832,
      AGGLO == "lannion" ~ 2.132257,
      AGGLO == "bourg-en-bresse" ~ 8.454405,
      AGGLO == "brest" ~ 2.473732,
      AGGLO == "rouen" ~ 1.695053,
      AGGLO == "saintes" ~ 3.9544141,
      AGGLO == "marseille" ~ 0.8868225,
      AGGLO == "valenciennes" ~ 1.4795444,
      AGGLO == "saint-etienne" ~ 1.566994,
      AGGLO == "metz" ~ 2.972130,
      AGGLO == "angers" ~ 3.926778,
      AGGLO == "poitiers" ~ 5.754658,
      AGGLO == "vendee" ~ 2.079867,
      AGGLO == "rennes" ~ 4.443215,
      AGGLO == "annemasse" ~ 1.844378,
      AGGLO == "creil" ~ 1.3897629,
      TRUE ~ 0
    ),
    arr_par_sup = case_when(
      AGGLO == "gap" ~ 2.888416,
      AGGLO == "ajaccio" ~ 0.9674419,
      AGGLO == "sables-dolonne" ~ 1.0793816,
      AGGLO == "reims" ~ 4.275654,
      AGGLO == "dijon" ~ 3.788131,
      AGGLO == "annecy" ~ 6.458170,
      AGGLO == "besancon" ~ 0.4550051,
      AGGLO == "niort" ~ 1.159011,
      AGGLO == "evreux" ~ 1.222812,
      AGGLO == "cherbourg" ~ 12.416107,
      AGGLO == "alencon" ~ 0.4103857,
      AGGLO == "lille" ~ 10.832025,
      AGGLO == "grenoble" ~ 1.825714,
      AGGLO == "lannion" ~ 1.554174,
      AGGLO == "bourg-en-bresse" ~ 1.865540,
      AGGLO == "brest" ~ 3.924532,
      AGGLO == "rouen" ~ 3.183168,
      AGGLO == "saintes" ~ 1.7673420,
      AGGLO == "marseille" ~ 4.513734,
      AGGLO == "valenciennes" ~ 1.2921135,
      AGGLO == "saint-etienne" ~ 3.346498,
      AGGLO == "metz" ~ 3.601072,
      AGGLO == "angers" ~ 3.282200,
      AGGLO == "poitiers" ~ 6.505372,
      AGGLO == "vendee" ~ 1.0793816,
      AGGLO == "rennes" ~ 3.055189,
      AGGLO == "annemasse" ~ 4.118700,
      AGGLO == "creil" ~ 6.268307,
      AGGLO == "niort" ~ 10.318583,
      AGGLO == "niort" ~ 10.318583,
      AGGLO == "niort" ~ 10.318583,
      AGGLO == "niort" ~ 10.318583,
      AGGLO == "niort" ~ 10.318583,
      AGGLO == "niort" ~ 10.318583,
      TRUE ~ 0
    )
  )

ggplot(variables_agglo, aes(x = reorder(AGGLO, lc_par_hab), y = lc_par_hab)) +
  geom_bar(stat = "identity", fill="darkturquoise") +
  labs(title = "Longueur commerciale par habitant par agglomération",
       x = "Agglomération",
       y = "Longueur commerciale par habitant (km/hab)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

# ACP

data_PCA <- variables_agglo[,c("AGGLO", "mediane_ddt",
            "dispersion_relative_ddt","proportion_voiture","lc_par_hab")]

res.PCA <- PCA(data_PCA[, -1], scale.unit=TRUE, ncp=2, graph=FALSE)

summary(res.PCA)

ind <- as.data.frame(res.PCA$ind$coord)  # Récupère les coordonnées des individus
ind$AGGLO <- as.factor(data_PCA$AGGLO)  # Associe les agglomérations

ggplot(ind, aes(x = Dim.1, y = Dim.2, label = AGGLO)) +
  geom_point(size = 2, color = "black") +  # Afficher les points
  geom_text_repel(size = 2.5) +  # Ajouter les noms sans chevauchement
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.5) +
  labs(title = "Projection des agglomérations sur le plan PCA",
       x = "Axe 1",
       y = "Axe 2") +
  theme_minimal()

fviz_pca_ind(res.PCA, 
             label = "all",  # Afficher les noms
             repel = TRUE)  # Éviter le chevauchement des labels
