require(rstan)
source("Fitness/Data_manipulation_fitness_stan.r")

stan_data = list(n_obs = n_obs,
                 n_ind=n_ind,
                 n_yi=n_yi,
                 n_y=n_y,
                 n_isl=n_isl,
                 individual=f$ID,
                 island=f$Island_ID,
                 year_isl=f$IslandYear_ID,
                 year=f$Year2,
                 n=f$RelPop,
                 age=f$Experienced_breeder-mean(f$Experienced_breeder),
                 sex=f$sex-mean(f$sex),
                 r=f$N_Recruits,
                 s=f$Own_survival,
                 v_n= var(f$logPop),
                 n_ind_isl_year=dFlokYear$estimated_pop_size,
                 IslandPopSize=IslandPopSize-mean(IslandPopSize),
                 Island_Ind=round(Island_Ind),
                 Isl_islyear=Isl_islyear)


ni <- 30000 ##Number of iterations
nc <- 3  ##Number of chains
nt <- 20 ## Thinning interval
nb <- 10000 ## Number of iterations to discard

params<-c("B_R", "B_S", "sigma2_ID","cov_ID","sigma2_YI", "cov_YI","sigma2_Y", "cov_Y",
          "sigma2_Isl_R","cov_Isl_R","sigma2_Isl_S","cov_Isl_S", "phi")

mod1<-stan_model("RR_Both.stan")
md_fitness <- sampling(mod1, data = stan_data, pars = params, chains = nc, iter = ni,  warmup = nb, thin = nt, cores=2)
round(summary(md_fitness)$summary[,c(6,4,8)],3)

saveRDS(md_fitness, "Results/md_recruits.rds")
