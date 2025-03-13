library(openxlsx)
library(FactoMineR)
library(cluster)
library(mclust)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(ggmosaic)
library(FNN)
library(class)
library(caret)
library(pROC)


#Extraction des csv standards
setwd("Données/")
liste_csv<-list.files(recursive = TRUE, pattern="std.+.csv")
all_csv <- sapply(liste_csv, read.csv, sep=";", USE.NAMES = TRUE)


#Concatenation dans un jeu de dataframes
data_men <- data.frame()
data_pers <- data.frame()
data_traj <- data.frame()
data_depl <- data.frame()

for(dir_enquete in names(all_csv)){
  nom_enquete <- tail(strsplit(dir_enquete, "/")[[1]],1)
  infos_enquete <- strsplit(nom_enquete, "_")[[1]]
  nom_ville <- infos_enquete[1]
  i = 2
  #On recherche le nom de la ville
  while(is.na(as.numeric(infos_enquete[i]))){
    nom_ville <- paste(nom_ville,infos_enquete[i], sep="-")
    i = i + 1
  }
  annee = infos_enquete[i]
  
  type = substr(infos_enquete[length(infos_enquete)], 1, nchar(infos_enquete[length(infos_enquete)])-4)
  
  #ajouter deux colonnes ville et année et concatener dans des grandes df
  data_tempo = all_csv[[dir_enquete]]
  data_tempo['AGGLO']=nom_ville
  data_tempo['ANNEE_ENQUETE']=annee 
  
  #Renommer les pbs de annee au lieu de an
  if('ANNEE' %in% names(data_tempo)){
    data_tempo = rename(data_tempo, "AN"="ANNEE")
  }
  
  if(type == "men"){
    if('METH' %in% names(data_tempo)){
      data_tempo <- data_tempo[,-which(colnames(data_tempo)=="METH")]
    }
    data_men = rbind(data_men,data_tempo)
  }else if(type == "pers") {
    if('PMET' %in% names(data_tempo)){
      data_tempo <- data_tempo[,-which(colnames(data_tempo)=="PMET")]
    }
    if('P17' %in% names(data_tempo)){
      data_tempo <- data_tempo[,-which(colnames(data_tempo)=="P17")]
    }
    if('P13B' %in% names(data_tempo)){
      data_tempo <- data_tempo[,-which(colnames(data_tempo)=="P13B")]
    }
    data_pers = rbind(data_pers,data_tempo)
  }else if(type == "traj") {
    if('TMET' %in% names(data_tempo)){
      data_tempo <- data_tempo[,-which(colnames(data_tempo)=="TMET")]
    }
    if('T3A' %in% names(data_tempo)){
      data_tempo <- data_tempo[,-which(colnames(data_tempo)=="T3A")]
    }
    data_traj = rbind(data_traj,data_tempo)
  }else if(type == "depl") {
    if('DMET' %in% names(data_tempo)){
      data_tempo <- data_tempo[,-which(colnames(data_tempo)=="DMET")]
    }
    data_depl = rbind(data_depl,data_tempo)
  }
}

#save(data_men, file = "data/data_men.RData")
#save(data_pers, file = "data/data_pers.RData")
#save(data_depl, file = "data/data_depl.RData")
#save(data_traj, file = "data/data_traj.RData")

load("data/data_depl.RData")
load("data/data_men.RData")
load("data/data_pers.RData")
load("data/data_traj.RData")

