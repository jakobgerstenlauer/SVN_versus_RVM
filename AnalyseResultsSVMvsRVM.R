#remove old objects for safety resons
rm(list=ls(all=TRUE))

#utility function
glue<-function(...){paste(...,sep="")}

#TODO Adapt to working dir or remove!
#setwd("D:/Documents/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
setwd("E:/Documents/Mis Documentos/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
#setwd("J:/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code")

#define path of standard directories
source("workingDir.R")

#this region dinamically loads the file
#opens a dialog box to input the file for svm
data.file <- file.choose()
d.svm  <- read.csv(data.file,sep=";")
str(d.svm)

#opens a dialog box to input the file for rvm
data.file <- file.choose()
library(tcltk2)
tk_choose.files()
d.rvm  <- read.csv(data.file,sep=";")
str(d.rvm)

#this region is for static loading
#setwd(dataDir)
#d.svm<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_08.csv",sep=";")
#d.rvm<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_08.csv",sep=";")

setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")
