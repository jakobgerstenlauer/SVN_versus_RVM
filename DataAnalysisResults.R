#remove old objects for safety resons
rm(list=ls(all=TRUE))

#utility function
glue<-function(...){paste(...,sep="")}

#TODO Adapt to working dir or remove!
#setwd('..')
#setwd(glue(getwd(),'/code'))
#setwd("J:/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code")
#setwd("E:/Documents/Mis Documentos/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
#setwd("D:/Documents/MIRI/Semestre 2/ProyectoFinal/KernelFinalProject/code")

source("properties.R")
#define path of standard directories
source("workingDir.R")

setwd(dataDir)
#get current date and replace hyphens by underline
Date<-gsub(pattern="-", replacement="_",Sys.Date())
#TODO: Check if the date is correct! #the file does not exist
#paste new filename
fileName<-paste("Results_Simulation_KMLMM_term_project_",Date,".csv",sep="")
d<-read.table(d.results, file=fileName, append=FALSE, row.names = FALSE)
#static file name
d<-read.table("Results_Simulation_SVM_KMLMM_term_project_2016_12_09.csv")
#d<-read.table("Results_Simulation_RVM_KMLMM_term_project_2016_12_08.csv")

#Was the correct polynomial degree chosen?
plot(polynomial.degree.grid,polynomial_degree_setting)

#How do the chosen C and Epsilon relate to signal-to-noise ratio,
#N and D?

m1_c.lm<-lm(c_setting ~ signal.to.noise.ratio.grid + num.vars.grid + num.observations.grid)
m1_epsilon.lm<-lm(epsilon_setting ~ signal.to.noise.ratio.grid + num.vars.grid + num.observations.grid)

#How does the performance of the SVM depend on
#signal-to-noise ratio,
#N and D?

m1_cv_mean.lm<-lm(cv.mean ~ signal.to.noise.ratio.grid + num.vars.grid + num.observations.grid)
m1_sparsity.lm<-lm(sparsity ~ signal.to.noise.ratio.grid + num.vars.grid + num.observations.grid)