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
                 w=f$r2s,
                 v_n= var(f$logPop),
                 n_ind_isl_year=dFlokYear$estimated_pop_size,
                 IslandPopSize=IslandPopSize-mean(IslandPopSize),
                 Island_Ind=round(Island_Ind),
                 Isl_islyear=Isl_islyear)


ni <- 25000 ##Number of iterations
nc <- 3  ##Number of chains
nt <- 20 ## Thinning interval
nb <- 5000 ## Number of iterations to discard

params<-c("B", "sigma", "sigma_isl", "cov_Isl", "phi", "temp_v", "ind_v" ,"spatial_v" , "spatio_temp_v",  
          "dem_v" , "dem_v_eff", "den_v", "p_temp_v",  "p_spatial_v", "p_spatio_temp_v",
          "p_dem_v_eff",  "p_den_v", "p_ind_v", "E_lambda")

mod1<-stan_model("Fitness/RR_fitness.stan")
md_fitness <- sampling(mod1, data = stan_data, pars = params, chains = nc, iter = ni,  warmup = nb, thin = nt, cores=3)
round(summary(md_fitness)$summary[,c(6,4,8)],3)

saveRDS(md_fitness, "Results/md_fitness.rds")
