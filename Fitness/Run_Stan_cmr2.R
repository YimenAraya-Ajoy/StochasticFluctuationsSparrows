require(rstan)
stan_data<-readRDS(file="GeneralData/CMR_standata.rds")

ni <- 15000 ##Number of iterations
nc <- 3  ##Number of chains
nt <- 10 ## Thinning interval
nb <- 5000 ## Number of iterations to discard

params<-c("mu_p", "mean_p", "mu_phi", "mean_phi", "Sigma2_YI_p", "Sigma2_YI_phi", "Sigma2_Y_phi", "p")

mod1<-stan_model("CMR2.stan")
md_cmr<- sampling(mod1, data = stan_data, pars = params, chains = nc, iter = ni,  warmup = nb, thin = nt, cores=3)
round(summary(md_cmr)$summary[,c(6,4,8)],3)
saveRDS(md_cmr, "~/Dropbox/StochasticitySparrows/Fitness/md_cmr2.rds")


