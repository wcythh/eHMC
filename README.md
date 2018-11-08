# eHMC
R code for eHMC

folder "UniversalFunctions": consisting of leapfrog integrator, longestbatch, empirical distribution of steps, eHMC sampler, eHMCq sampler and eHMCu sampler.

folder "Model": consisting of 5 models: Logit, IRT, MVN, SIR, SV
                -- Metric: the covariance matrix of the momentum, the identity and adapted diagonal matrix
                -- ModelSpecificFunctions: consisting of the observation data (if existing), the U and grad_U and the function        returning summary statistics
                -- SimulationResult: show the comparison amongst NUTS, eHMC, eHMCq and eHMCu over 8 targetted accept probabilities. 

