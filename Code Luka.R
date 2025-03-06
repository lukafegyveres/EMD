setwd("C:/Users/lukaf/OneDrive - Ecole Polytechnique/Bureau/Polytechnique/3A/P2/HSS Données et méthodes quantitatives/EMD")

#distance domtravail par agglo

df <- subset(domtrav, domtrav$ANNEE_ENQUETE > 2018)
df <- df[,c("AGGLO","D11","D12")]


##Affichage sous forme de bar plot

boxplot(df$D11 ~ df$AGGLO)
