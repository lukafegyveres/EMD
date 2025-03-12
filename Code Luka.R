setwd("C:/Users/lukaf/OneDrive - Ecole Polytechnique/Bureau/Polytechnique/3A/P2/HSS Données et méthodes quantitatives/EMD")

#distance domtravail par agglo

df <- subset(domtrav, domtrav$ANNEE_ENQUETE > 2015)
df <- df[,c("AGGLO","D11","D12")]


##Affichage sous forme de boxplot

ggplot(data=df, aes(x=AGGLO, y=D11)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0,30000))

## s'assurer que les agglos sont en un seul exemplaire
## reprendre le pourcentage de trajets en voiture dans transport.R
## calculer médiane et la dispersion (Q3-Q1/Q2) de chaque agglo
## récupérer longueur commerciale de chaque agglo dans deter.R
## faire l'ACP (diapos 42 et 43)