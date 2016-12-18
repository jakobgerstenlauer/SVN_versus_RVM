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
d<-read.csv("Results_Simulation_SVM_KMLMM_term_project_2016_12_16.csv",sep=";")

#opens a dialog box to input the file
#data.file <- file.choose()
#d  <- read.csv(data.file,sep=";")

#correct value for cv.mean, force minimum to zero
d$cv.mean.corrected <- ifelse(d$cv.mean < 0.0,0.0,d$cv.mean)

setwd(codeDir)
source("KMLMM_term_project_GERSTENLAUER_utility_functions.R")
str(d)

#First of all let´s aggregate the recorded values for each sample (each sample has three replicates)

#a dummy data set
d.svm<-data.frame(as.numeric(unique(d$id_parameter_combination)))
names(d.svm)<-"id_parameter_combination"

#variables to average
variables.numeric<-names(d)[c(1,7:14)]

for(var in variables.numeric){
  command<-glue("d.svm$",var,"<-as.numeric(tapply(d$",var,",d$id_parameter_combination,mean))")
  eval(parse(text=command))
  
  command<-glue("d.svm$",var,".sd<-as.numeric(tapply(d$",var,",d$id_parameter_combination,sd))")
  eval(parse(text=command))
}

#variables specific for the sample, for these variables it is sufficient to record one value (always the same by definition!)
variables.sample<-names(d)[-c(1,6:14)]

for(var in variables.sample){
  command<-glue("d.svm$",var,"<-as.numeric(tapply(d$",var,",d$id_parameter_combination,unique))")
  eval(parse(text=command))
}

setwd(dataDir)
write.table(x=d.svm, file="Results_Simulation_SVM_KMLMM_term_project_2016_12_16_aggregated.csv",row.names = FALSE)

with(d.svm,plot(signal.to.noise.ratio, cv.mean, pch="+")) 
#There are some extreme outliers with very low cv.mean (very bad models)
#Let´s ignore these very bad models to be able to plot the relationship between cv.mean and signal to noise ratio.

with(subset(d.svm,cv.mean>-0.1),plot(signal.to.noise.ratio, cv.mean, pch="+")) 
with(subset(d.svm,cv.mean>-0.1),cor.test(signal.to.noise.ratio, cv.mean)) 
# Pearson's product-moment correlation
# 
# data:  signal.to.noise.ratio and cv.mean
# t = 1.0803, df = 238, p-value = 0.2811
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.05728004  0.19476362
# sample estimates:
# cor 
# 0.06985657 

with(subset(d.svm,cv.mean>-0.1),cor.test(signal.to.noise.ratio, cv.mean, method="kendall"))
# Kendall's rank correlation tau
# 
# data:  signal.to.noise.ratio and cv.mean
# z = 0.85345, p-value = 0.3934
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
# tau 
# 0.0370041
#Conclusion: There is no significant correlation between signal to noise ratio and cv.mean!
#The results are quite evenly distributed in the space!

with(subset(d.svm,cv.mean>-0.1),hist(cv.mean))
with(subset(d.svm,cv.mean>-0.1),hist(sparsity))
with(d.svm,hist(polynomial.degree-polynomial_degree_setting))
with(d.svm,table(polynomial.degree-polynomial_degree_setting))
#-7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7 
#1   6   9  29  28  50  28 155 108 230 103 101  31  18   3


#Conclusion: The SVM has a tendency to underestimates the polynomial degree.

#get the table analysing the results, in here we use a pearson covariance
row.attributes<-c("d.svm$c_setting","d.svm$epsilon_setting","d.svm$polynomial.degree-d.svm$polynomial_degree_setting","d.svm$sparsity","d.svm$cv.mean.corrected","d.svm$compu.time")
column.attributes<-c("d.svm$signal.to.noise.ratio","d.svm$num.observations/d.svm$num.vars","d.svm$polynomial.degree")
result.table.svm <- populate.table(row.attributes,column.attributes,"spearman")
colnames(result.table.svm) <- c("signal/noise","cases/inputs","degree")
rownames(result.table.svm) <- c("C","$epsilon$","$delta$ degree","sparsity","$lambda$","time[s]")

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
