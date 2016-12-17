#Test results for rvm
#remove old objects for safety resons
rm(list=ls(all=TRUE))

#utility function
glue<-function(...){paste(...,sep="")}

#TODO Adapt to working dir or remove!
#setwd("D:/Documents/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
#setwd("E:/Documents/Mis Documentos/MIRI/Semestre 2/APRENDIZAJE AUTOMATICO BASADO EN KERNEL Y MODELADO MULTIVARIANTE/ProyectoFinal/KernelFinalProject/code")
#setwd("J:/UPC/2016/02/KMLMM/KernelMethods/practicals/term_project/code")

#define path of standard directories
source("workingDir.R")
setwd(dataDir)
d<-read.csv("Results_Simulation_RVM_KMLMM_term_project_2016_12_15.csv",sep=";") #<-set to static
#this region dynamically loads the file
#opens a dialog box to input the file
#data.file <- file.choose()
#d  <- read.csv(data.file,sep=";")

setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")
str(d)

with(d,plot(signal.to.noise.ratio, cv.mean, pch="+")) 
#There are some extreme outliers with very low cv.mean (very bad models)
#Let´s ignore these very bad models to be able to plot the relationship between cv.mean and signal to noise ratio.

with(subset(d,cv.mean>-0.1),plot(signal.to.noise.ratio, cv.mean, pch="+")) 
with(subset(d,cv.mean>-0.1),cor.test(signal.to.noise.ratio, cv.mean)) 
# Pearson's product-moment correlation
# 
# data:  signal.to.noise.ratio and cv.mean
# t = 1.8427, df = 703, p-value = 0.06579
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.004529434  0.142443383
# sample estimates:
# cor 
# 0.06933319 
#Conclusion: The positive correlation is marginally not significant.

#test for non-linear effect with Kendall's tau:
with(subset(d,cv.mean>-0.1),cor.test(signal.to.noise.ratio, cv.mean, method="kendall"))
#Kendall's rank correlation tau
# data:  signal.to.noise.ratio and cv.mean
# z = 1.6964, p-value = 0.08981
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
# tau 
# 0.04276472 

#get the table analysing the results, in here we use a pearson covariance
row.attributes<-c("d$polynomial.degree-d$polynomial_degree_setting","d$sparsity","d$cv.mean","d$compu.time")
column.attributes<-c("d$signal.to.noise.ratio","d$num.observations/d$num.vars","d$polynomial.degree")
result.table.rvm <- populate.table(row.attributes,column.attributes,"spearman")
colnames(result.table.rvm) <- c("Signal to noise ratio", "Number of observations/Number of variables", "Polynomial degree")
rownames(result.table.rvm) <- c("Error in estimating poly","Sparcity","CV Mean","Computational Time")
#save to a new file
Date<-gsub(pattern="-", replacement="_",Sys.Date())
fileName<-paste("RVM_results_table_KMLMM_term_project_",Date,".csv",sep="")
result.table.rvm
setwd(dataDir)
write.table(result.table.rvm, file=fileName, append=FALSE, row.names = TRUE, col.names = NA, sep = ";")

#P.P. copy from tests done in isolated environment#
#install.packages("ggplot2")
library(ggplot2)