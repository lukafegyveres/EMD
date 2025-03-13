setwd("C:/Users/paulr/OneDrive/Documents/X 3A/Methodes quantitatives/EMD/")



setwd("Données TCU")


full_data_tcu <- read.csv("open-data-tcu-dataenq.csv", sep = ";")

data_tcu <- subset(full_data_tcu, full_data_tcu$var_id==5722) #On récup les longueurs commerciales
data_tcu <- rename(data_tcu, LC=dat_valeur)

data_pm <- read.csv("tcu-persmorale.csv", sep = ";") #On récup les infos de PM

data_tcu <- merge(x=data_tcu, y=data_pm, by = "pm_id")

data_tcu_tempo <- subset(full_data_tcu, full_data_tcu$var_id==181) #On récup les populations
data_tcu <- merge(x=data_tcu, y=data_tcu_tempo, by = c("pm_id", "enq_id"))
data_tcu <- rename(data_tcu, POP=dat_valeur)

data_tcu_tempo <- subset(full_data_tcu, full_data_tcu$var_id==5724) #On récup le nombre d'arret
data_tcu <- merge(x=data_tcu, y=data_tcu_tempo, by = c("pm_id", "enq_id"))
data_tcu <- rename(data_tcu, NB_ARR=dat_valeur)

data_tcu <- data_tcu[,c("enq_id","pm_id","LC","POP", "NB_ARR","dat_date.x","pm_nom","pm_rang","pm_sigle")]


data_tcu_tempo <- subset(full_data_tcu, full_data_tcu$var_id==1810) #On récup la superficie
data_tcu <- merge(x=data_tcu, y=data_tcu_tempo, by = c("pm_id", "enq_id"))
data_tcu <- rename(data_tcu, SUP=dat_valeur)

data_tcu <- data_tcu[,c("enq_id","pm_id","LC","POP", "NB_ARR", "SUP","dat_date.x","pm_nom","pm_rang","pm_sigle")]

data_tcu$LC <- as.numeric(data_tcu$LC)
data_tcu$POP <- as.numeric(data_tcu$POP)
data_tcu$SUP <- as.numeric(data_tcu$SUP)
data_tcu$NB_ARR <- as.numeric(data_tcu$NB_ARR)

data_tcu$lc_par_hab <-1000* data_tcu$LC / data_tcu$POP
data_tcu$arr_par_sup <- data_tcu$NB_ARR / data_tcu$SUP


exploitants <- read.csv("exploitants.csv", sep = ",", stringsAsFactors = FALSE)
donnees_tcu_agglo <- inner_join(data_tcu, exploitants, by="pm_id")

save(donnees_tcu_agglo, file = "data/donnees_agglo.RData")
