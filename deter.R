donnees_tcu_agglo <- inner_join(data_tcu, exploitants, by="pm_id")

dico_df <- read.csv("dico_mode_dep_simpl.csv", sep = ",", stringsAsFactors = FALSE)
df_domtrav_deter <- domtrav %>%
  left_join(dico_df, by = c("MODP" = "CODE")) %>%
  select(-MODP) %>%
  rename(MODP = MODP_LIB)

df2 <- donnees_tcu_agglo

df2$dat_date.x <- as.Date(df2$dat_date.x, format = "%d/%m/%Y")


# Ajouter une colonne "ANNEE" à df2 pour extraire l'année de dat_date.x
df2 <- df2 %>%
  mutate(ANNEE = format(dat_date.x, "%Y") %>% as.numeric())

df1 <- df_domtrav_deter

# Trouver la date la plus proche pour chaque AGGLO et ANNEE_ENQUETE
df_closest <- df2 %>%
  left_join(df1, by = "AGGLO") %>%  # Joindre pour comparer les années
  mutate(diff = abs(ANNEE - ANNEE_ENQUETE)) %>%  # Calculer la différence absolue d'années
  group_by(AGGLO, ANNEE_ENQUETE) %>%
  slice_min(order_by = diff) %>%  # Garder la date avec la différence minimale
  ungroup() %>%
  select(AGGLO, ANNEE_ENQUETE, dat_date.x)  # Garder seulement les colonnes utiles

result <- df1 %>%
  inner_join(df_closest, by = c("AGGLO", "ANNEE_ENQUETE"))
