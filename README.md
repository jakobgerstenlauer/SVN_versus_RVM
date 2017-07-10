Kernel-Based Learning & Multivariate Modeling DMKM Master - MIRI Master
Lecturer: Lluıs A. Belanche, belanche@cs.upc.edu
Term project
October-December 2016
"Compare the Relevance Vector machine and the SVM for regression in terms of predictive accuracy
and sparsity."

The following folders are available:

report:
Report as pdf.

data: 
In this folder, for each method (SVM and RVM) there is a separate output file 
storing the results of the replicated Latin hypercube simulation implemented in "KMLMM_term_project_GERSTENLAUER.R".

code: 
properties.R: Defines parameters of the Latin hypercube sampling scheme.
KMLMM_term_project_Perez_GERSTENLAUER.R: Code for running the simulations.
KMLMM_term_project_Perez_GERSTENLAUER_utilities.R: Utility functions used in other scripts.
AnalyseFirstResultsOfSVM.R: 
Inputfile: "Results_Simulation_SVM_KMLMM_term_project_2016_12_16.csv"
Outputfile: "Results_Simulation_SVM_KMLMM_term_project_2016_12_16_aggregated.csv"
Creates Table 4, aggregates data and writes it to outputfile 
AnalyseFirstResultsOfRVM.R: 
Inputfile: "Results_Simulation_RVM_KMLMM_term_project_2016_12_16.csv"
Outputfile: "Results_Simulation_RVM_KMLMM_term_project_2016_12_16_aggregated.csv"
Create Table 5, aggregates data and writes it to outputfile
AnalyseResultsSVMvsRVM.R:
Reads aggregated outputfiles from AnalyseFirstResultsOfSVM.R / AnalyseFirstResultsOfRVM.R.
Analyses results and creates figures and tables.

Date: 21.12.2016
Jakob Gerstenlauer
jakob.gerstenlauer@gjakob.de
Francisco Pèrez
pacogppl@gmail.com