## Kernel-Based Learning & Multivariate Modeling DMKM Master - MIRI Master
Lecturer: Lluıs A. Belanche, belanche@cs.upc.edu
Term project
October-Dezember 2016

## "Compare the Relevance Vector machine and the SVM for regression in terms of predictive accuracy and sparsity."

- First, an instance generater is set-up.
- Second, we define a latin-hypercube (LH) scheme for efficient sampling of parameter space.
- Third, for each parameter combination of the LH sampling scheme, one instance (data set) is generated.
- Fourth, for each instance we tune the parameters of the SVM and the kernel.
  Based on the optimum parameters (both kernel and SVM parameters, criteria: cv error)
  we record generalization error, sparsity, and total computation time (including tuning) for the selected model.
- Fifth, for each instance we tune the parameters of the RVM and the kernel.
  Based on the optimum parameters (both kernel and RVM parameters, criteria: cv error)
  we record generalization error, sparsity, and total computation time (including tuning) for the selected model.
- Sixth, we analyse the results of step 4 and 5 comparing SVM and RVM across the following dimensions:
  dim 1: signal-to-noise ratio (between 0: no signal only noise and 1: only signal no noise),
  dim 2: number of observations N,
  dim 3: number of parameters D,
  dim 4: polynomial degree of the inputs.
- Seven, we compare the two models asking the following questions:
  Question 1: How often do the models choose the appropriate ploynomial degree of the kernel?
  Question 2: How does the predictive accuracy of both models depend on N, D, and signal-to-noise ratio?
