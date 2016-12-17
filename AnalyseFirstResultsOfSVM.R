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

#this region is for static loading
setwd(dataDir)
d<-read.csv("Results_Simulation_SVM_KMLMM_term_project_2016_12_16.csv",sep=";")

#this region dinamically loads the file
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
# 0.04123481
#test for non-linear effect with Kendall's tau:

with(subset(d,cv.mean>-0.1),cor.test(signal.to.noise.ratio, cv.mean, method="kendall"))
# Kendall's rank correlation tau
# 
# data:  signal.to.noise.ratio and cv.mean
# z = 0.94221, p-value = 0.3461
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
# tau 
# 0.02297758 
#Conclusion: There is no significant correlation between signal to noise ratio and cv.mean!
#The results are quite evenly distributed in the space!
with(subset(d,cv.mean>-0.1),hist(cv.mean))
with(subset(d,cv.mean>-0.1),hist(sparsity))
with(d,hist(polynomial.degree-polynomial_degree_setting))
with(d,table(polynomial.degree-polynomial_degree_setting))
#-7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7 
#1   6   9  29  28  50  28 155 108 230 103 101  31  18   3

#correct value for cv.mean
d$cv.mean.corrected <- ifelse(d$cv.mean < 0.0,0.0,d$cv.mean)

#Conclusion: The SVM has a tendency to underestimates the polynomial degree.

#get the table analysing the results, in here we use a pearson covariance
row.attributes<-c("d$c_setting","d$epsilon_setting","d$polynomial.degree-d$polynomial_degree_setting","d$sparsity","d$cv.mean.corrected","d$compu.time")
column.attributes<-c("d$signal.to.noise.ratio","d$num.observations/d$num.vars","d$polynomial.degree")
result.table.svm <- populate.table(row.attributes,column.attributes,"spearman")
colnames(result.table.svm) <- c("Signal to noise ratio", "Number of observations/Number of variables", "Polynomial degree")
rownames(result.table.svm) <- c("C","Epsilon","Error in estimating poly","Sparsity","CV Mean","Computational Time")

#save to a new file
Date<-gsub(pattern="-", replacement="_",Sys.Date())
fileName<-paste("SVM_results_table_KMLMM_term_project_",Date,".csv",sep="")
result.table.svm
setwd(dataDir)
#install.packages("xtable")
library(xtable)
latex.table.svm<-xtable(result.table.svm)
print.xtable(latex.table.svm, type="latex", file="result_table_svm.tex")
write.table(result.table.svm, file=fileName, append=FALSE, row.names = TRUE, col.names = NA, sep = ";")


#define more relevant graphic i.e. most and best descriptive graphic
#pag. 28 <- reorder bars
#pag. 122 <- multiple histograms with different color
#pag. 159 <- error bars
#boxplots? ts?

#todo: is relevant to know the relation between str, cv.mean and sparcity?

#P.P. copy from tests done in isolated environment# values to consider: cv.mean, sparcity, sd.sparsity, 
#signal to noise ratio, comput time
#
#install.packages("ggplot2")
# library(ggplot2)
#signal to noise ratio vs cv.mean
# plot(d_low_model_quality$signal.to.noise.ratio, d_low_model_quality$cv.mean)
# ggplot(d,aes(x=d$cv.mean,y=d$signal.to.noise.ratio,fill=d$id_parameter_combination))+geom_bar(position = "dodge",stat="identity")+labs(title="Comparison for CV Mean and STNR",x="CV Mean",y="Signal to noise ratio")
# ggplot(d,aes(x=d$cv.mean,y=d$sparsity,fill=d$id_parameter_combination))+geom_bar(position = "identity",stat="identity")+labs(title="Comparison for CV Mean and STNR",x="CV Mean",y="Sparcity")
# ggplot(d,aes(x=d$sparsity,y=d$signal.to.noise.ratio,fill=d$id_parameter_combination))+geom_bar(position = "identity",stat="identity")+labs(title="Comparison for CV Mean and STNR",x="Sparcity",y="Signal to noise ratio") 

#shows the same concentration, most of them tend to be more grouped in 1 (100 in the graph)
#ggplot(d_low_model_quality,aes(x=cv.mean,y=signal.to.noise.ratio,fill=id_parameter_combination))+geom_bar(position = "dodge",stat="identity")+labs(title="Comparison for CV Mean and STNR",x="CV Mean",y="Signal to noise ratio") 

#get the computation time for each parameter, in total 100 combinations,3 each
# hist(tapply(d$compu.time,d$id_parameter_combination,sum))