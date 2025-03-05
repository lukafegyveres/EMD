setwd("C:/Users/paulr/OneDrive/Documents/X 3A/Methodes quantitatives/EMD")


deplacements_personnes <- merge(x=data_depl, y=data_pers, by.x = c("AGGLO","ANNEE_ENQUETE","ZFD", "ECH", "PER"), by.y = c("AGGLO","ANNEE_ENQUETE","ZFP", "ECH", "PER"))

domtrav <- subset(deplacements_personnes, (deplacements_personnes$D5A == 1 & deplacements_personnes$D2A == 11) | (deplacements_personnes$D5A == 11 & deplacements_personnes$D2A == 1))

#save(domtrav, file = "data/domtrav.RData")

load("data/domtrav.RData")

#Analyse des modes de déplacement par agglo
df <- subset(domtrav)
df <- df[,c("AGGLO","MODP")]


#On remplace les modes numériques par le nom du mode
dico_df <- read.csv("dico_mode_dep_simpl.csv", sep = ",", stringsAsFactors = FALSE)
df <- df %>%
  left_join(dico_df, by = c("MODP" = "CODE")) %>%
  select(-MODP) %>%
  rename(MODP = MODP_LIB)

#comptage des déplacements par type et par agglo 
df_count <- df %>%
  group_by(AGGLO, MODP) %>%
  summarise(COUNT = n(), .groups = "drop")


##Affichage sous forme de bar plot

ggplot(df_count, aes(x = AGGLO, y = COUNT, fill = MODP)) +
  geom_bar(stat = "identity", position = "fill") +  # "fill" pour normaliser les proportions
  scale_y_continuous(labels = scales::percent) +    # Affichage en pourcentage
  scale_fill_brewer(palette="Set3") +
  labs(title = "Répartition des modes de déplacement par agglomération",
       x = "Agglomération",
       y = "Proportion",
       fill = "Mode de déplacement") +
  theme_minimal()


##Affichage sous forme de tableau

# Normalisation des fréquences
df_freq <- df_count %>%
  group_by(AGGLO) %>%
  mutate(Frequence = COUNT / sum(COUNT)) %>%
  select(-COUNT)

# Transformation en tableau large
tableau_freq <- df_freq %>%
  pivot_wider(names_from = MODP, values_from = Frequence, values_fill = list(Frequence = 0))

# Affichage avec gt
tableau_freq %>%
  gt() %>%
  fmt_percent(columns = everything(), decimals = 1) %>%  # Afficher en pourcentage
  tab_options(table.font.size = "large")
