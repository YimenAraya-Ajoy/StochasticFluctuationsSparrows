require(rstan)
source("Fitness/Data_Manipulation_Morphology_selection_stan.R")

stan_data = list(n_obs_m = nrow(m),
                 n_obs_f = nrow(f),
                 n_ind=n_ind_m,
                 n_yi=n_yi_m,
                 n_y=n_y_m,
                 n_isl=n_isl_m,
                 n_observ=n_observ,
                 n_m=max(m$Month),
                 individual_m=m$ID,
                 individual_f=f$ID,
                 island_f=f$Island_ID,
                 
                 year_isl_m=m$IslandYear_ID,
                 year_isl_f=f$IslandYear_ID,
                 year_m=m$Year,
                 year_f=f$Year,
                 observer=m$observer_ID,
                 sex_m=m$sex,
                 sex_f=f$sex,
                 sex2=m$sex+1,
                 m=m$vekt,
                 w=f$r2s,
                 r=f$N_Recruits,
                 s=f$Own_survival,
                 month=m$Month,
                 age=f$Experienced_breeder,
                 n=f$RelPop)




ni <-15000 ##Number of iterations
nc <- 3  ##Number of chains
nt <- 10 ## Thinning interval
nb <- 5000 ## Number of iterations to discard

params<-c("B_m", "B_f", "sigma_I", "sigma2_YI","sigma2_Y", "sigma2_Isl", "phi", "cov_Isl",  "cov_IY", "cov_Y",
           "CV", "optim", "var_optim", "sel")
mod_selection<-stan_model("Fitness/RR_selection.stan")
md_selection <- sampling(mod_selection, data = stan_data, pars = params, chains = nc, iter = ni,  warmup = nb, thin = nt, cores=3)
saveRDS(md_selection, "Results/md_selection.rds")

