require(drake)
source("src/models.R")
source("lib/cumulative_logit.R")

## Make analysis plan to run all models
make(analysis_plan, jobs = 2)

## Load cached models
loadd(ol_m)
loadd(gaussian_m)

## Expose functions for loo
expose_functions(ol_m, vectorize=FALSE)

## Run loo
loo(ol_m, gaussian_m)
