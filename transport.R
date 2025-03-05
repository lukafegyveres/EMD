setwd("C:/Users/paulr/OneDrive/Documents/X 3A/Methodes quantitatives/EMD")


deplacements_personnes <- merge(x=data_depl, y=data_pers, by.x = c("AGGLO","ANNEE_ENQUETE","ZFD", "ECH", "PER"), by.y = c("AGGLO","ANNEE_ENQUETE","ZFP", "ECH", "PER"))

deplacements_personnes <- deplacements_personnes[,c("AGGLO","ANNEE_ENQUETE","ZFD","ECH","PER","NDEP","PCSC","D2A","D5A","MODP")]

domtrav <- subset(deplacements_personnes, (deplacements_personnes$D5A == 1 & deplacements_personnes$D2A == 11) | (deplacements_personnes$D5A == 11 & deplacements_personnes$D2A == 1))


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


################################################################################
#                              Agglo                                           #
################################################################################

setwd("C:/Users/paulr/OneDrive/Documents/X 3A/Methodes quantitatives/Données TCU/")
full_data_tcu <- read.csv("open-data-tcu-dataenq.csv", sep = ";")

data_tcu <- subset(full_data_tcu, full_data_tcu$var_id==5722) #On récup les longueurs commerciales
data_tcu <- rename(data_tcu, LC=dat_valeur)

data_pm <- read.csv("tcu-persmorale.csv", sep = ";") #On récup les infos de PM

data_tcu <- merge(x=data_tcu, y=data_pm, by = "pm_id")

data_tcu_tempo <- subset(full_data_tcu, full_data_tcu$var_id==181) #On récup les populations
data_tcu <- merge(x=data_tcu, y=data_tcu_tempo, by = c("pm_id", "enq_id"))
data_tcu <- rename(data_tcu, POP=dat_valeur)

data_tcu <- data_tcu[,c("enq_id","pm_id","LC","POP","dat_date.x","pm_nom","pm_rang","pm_sigle")]

data_tcu$LC <- as.numeric(data_tcu$LC)
data_tcu$POP <- as.numeric(data_tcu$POP)

data_tcu$lc_par_hab <-1000* data_tcu$LC / data_tcu$POP

