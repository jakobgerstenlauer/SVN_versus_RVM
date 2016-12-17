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
# data.file <- file.choose()
# d.svm  <- read.csv(data.file,sep=";")
# str(d.svm)

#opens a dialog box to input the file for rvm
# data.file <- file.choose()
# library(tcltk2)
# tk_choose.files()
# d.rvm  <- read.csv(data.file,sep=";")
# str(d.rvm)

#this region is for static loading
setwd(dataDir)
d.svm<-read.csv("SVM_results_table_KMLMM_term_project_2016_12_17.csv",sep=";")
str(d.svm)
d.rvm<-read.csv("RVM_results_table_KMLMM_term_project_2016_12_17.csv",sep=";")
str(d.rvm)

setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")

#get the table analysing the results, in here we use a pearson covariance
row.attributes<-c("d$c_setting","d$epsilon_setting","d$polynomial.degree-d$polynomial_degree_setting","d$sparsity","d$cv.mean","d$compu.time")
column.attributes<-c("d$signal.to.noise.ratio","d$num.observations/d$num.vars","d$polynomial.degree")
result.table.svm <- populate.table(row.attributes,column.attributes,"spearman")
colnames(result.table.svm) <- c("Signal to noise ratio", "Number of observations/Number of variables", "Polynomial degree")
rownames(result.table.svm) <- c("C","Epsilon","Error in estimating poly","Sparsity","CV Mean","Computational Time")
#save to a new file

Date<-gsub(pattern="-", replacement="_",Sys.Date())
fileName<-paste("SVM_results_table_KMLMM_term_project_",Date,".csv",sep="")
result.table.svm
setwd(dataDir)
write.table(result.table.svm, file=fileName, append=FALSE, row.names = TRUE, col.names = NA, sep = ";")
