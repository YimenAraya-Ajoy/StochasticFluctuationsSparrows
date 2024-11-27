library(AGHmatrix)
library(hglm)
require(rstan)

source("~/Dropbox/StochasticitySparrows/Morphology/Data_Manipulation_AnimalModel_stan.R")

inits <- function() list(sigma_G = runif(1, 0.01, 1),
                         sigma_R = runif(1, 0.01, 1),
                         sigma_I = runif(1, 0.01, 1))

ni <- 25000
nt <- 5
nb <- 5000
nc <- 3
params<-c("mu1", "B_sex", "sigma2_G", "sigma2_I", "sigma2_R", "sigma2_IY",
          "sigma2_AIY", "sigma2_AI", "sigma2_AY","p_sigma2_AI", "p_sigma2_AY",
          "h2", "cv", "drift_all", "drift_g", "p_drift", "meanAIY", "delta")

## Call Stan from R
md_animal_model <- stan("/home/yi/Dropbox/StochasticitySparrows/Morphology/AnimalModel.stan", data = stan_data, init = inits, pars = params,
                        chains = nc, iter = ni, warmup = nb, thin = nt, cores=3)

round(summary(md_animal_model)$summary[,c(6,4,8)],3)
saveRDS(md_animal_model, "/home/yi/Dropbox/StochasticitySparrows/Results/md_animal_model.rds")

