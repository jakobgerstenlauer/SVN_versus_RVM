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

with(d,plot(signal.to.noise.ratio, cv.mean, pch="+")) 
#There are some extreme outliers with very low cv.mean (very bad models)
#Let´s ignore these very bad models to be able to plot the relationship between cv.mean and signal to noise ratio.
with(subset(d,cv.mean>-0.1),plot(signal.to.noise.ratio, cv.mean, pch="+")) 
with(subset(d,cv.mean>-0.1),cor.test(signal.to.noise.ratio, cv.mean)) 

#correct value for cv.mean
d$cv.mean.corrected <- ifelse(d$cv.mean < 0.0,0.0,d$cv.mean)

#First of all let´s aggregate the recorded values for each sample (each sample has three replicates)

#a dummy data set
d.rvm<-data.frame(as.numeric(unique(d$id_parameter_combination)))
names(d.rvm)<-"id_parameter_combination"

#variables to average
variables.numeric<-names(d)[c(1,7:12)]

for(var in variables.numeric){
  command<-glue("d.rvm$",var,"<-as.numeric(tapply(d$",var,",d$id_parameter_combination,mean))")
  eval(parse(text=command))
  
  command<-glue("d.rvm$",var,".sd<-as.numeric(tapply(d$",var,",d$id_parameter_combination,sd))")
  eval(parse(text=command))
}

#variables specific for the sample, for these variables it is sufficient to record one value (always the same by definition!)
variables.sample<-names(d)[-c(1,6:12)]

for(var in variables.sample){
  command<-glue("d.rvm$",var,"<-as.numeric(tapply(d$",var,",d$id_parameter_combination,unique))")
  eval(parse(text=command))
}

setwd(dataDir)
write.table(x=d.rvm, file="Results_Simulation_RVM_KMLMM_term_project_2016_12_15_aggregated.csv",row.names = FALSE)

with(d.rvm,plot(signal.to.noise.ratio, cv.mean.corrected, pch="+")) 

with(d.rvm,plot(signal.to.noise.ratio, cv.mean.corrected, pch="+")) 
with(d.rvm,cor.test(signal.to.noise.ratio, cv.mean.corrected)) 
# Pearson's product-moment correlation
# 
# data:  signal.to.noise.ratio and cv.mean.corrected
# t = 1.3339, df = 298, p-value = 0.1833
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.03651899  0.18863560
# sample estimates:
# cor 
# 0.07704044 

#test for non-linear effect with Kendall's tau:
with(d.rvm,cor.test(signal.to.noise.ratio, cv.mean.corrected, method="kendall"))
with(d.rvm,hist(cv.mean.corrected))
with(d.rvm,hist(sparsity))
with(d.rvm,hist(polynomial.degree-polynomial_degree_setting))
with(d.rvm,table(polynomial.degree-polynomial_degree_setting))


#get the table analysing the results, in here we use a pearson covariance
row.attributes<-c("d.rvm$polynomial.degree-d.rvm$polynomial_degree_setting","d.rvm$sparsity","d.rvm$cv.mean.corrected","d.rvm$compu.time")
column.attributes<-c("d.rvm$signal.to.noise.ratio","d.rvm$num.observations/d.rvm$num.vars","d.rvm$polynomial.degree")
result.table.rvm <- populate.table(row.attributes,column.attributes,"spearman")
colnames(result.table.rvm) <- c("signal/noise","cases/inputs","degree")
rownames(result.table.rvm) <- c("$delta$ degree","sparsity","$lambda$","time[s]")

#save to a new file
Date<-gsub(pattern="-", replacement="_",Sys.Date())
fileName<-paste("RVM_results_table_KMLMM_term_project_",Date,".csv",sep="")
result.table.rvm
setwd(dataDir)
#install.packages("xtable")
library(xtable)
latex.table.rvm<-xtable(result.table.rvm)
print.xtable(latex.table.rvm, type="latex", file="result_table_rvm.tex")
write.table(result.table.rvm, file=fileName, append=FALSE, row.names = TRUE, col.names = NA, sep = ";")
