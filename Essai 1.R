setwd("C:/Users/lukaf/OneDrive - Ecole Polytechnique/Bureau/Polytechnique/3A/P2/HSS Données et méthodes quantitatives/EMD")

setwd("Données/lil-0921/lil-0921.csv/Csv/Fichiers_Standard_Face_a_face/")

library("openxlsx")

menage <- read.csv("marseille_2009_std_faf_men.csv", sep = ";")
personne <- read.csv("marseille_2009_std_faf_pers.csv", sep = ";")
deplacement <- read.csv("marseille_2009_std_faf_depl.csv", sep = ";")
trajet <- read.csv("marseille_2009_std_faf_traj.csv", sep = ";")
opinion <- read.csv("marseille_2009_std_faf_opi.csv", sep = ";")

View(menage)
View(personne)
View(deplacement)
View(trajet)
View(opinion)

?merge

merge_deplacement_personne <- merge(x = deplacement, y = personne, by.x = c("ZFD", "ECH", "PER"), by.y = c("ZFP", "ECH", "PER"))  

reduction <- merge_deplacement_personne[,c("ZFD", "ECH", "PER", "NDEP", "D2A", "D5A", "D9", "PCSC")]

domicile_travail_PCS <- subset(reduction, (reduction$D2A == 1 & reduction$D5A == 11) | (reduction$D2A == 11 & reduction$D5A == 1))

boxplot(domicile_travail_PCS$D9 ~ domicile_travail_PCS$PCSC)
