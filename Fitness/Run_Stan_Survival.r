require(rstan)
source("Fitness\Data_manipulation_fitness_stan.r")

year2_d<-f[unique(f$flokyear),c("Year2", "IslandYear_ID")]
year2_d$IslandYear_ID<-as.numeric(year2_d$IslandYear_ID)
year2<-year2_d[order(year2_d$IslandYear_ID),1]

stan_data = list(n_obs = n_obs,
                 n_ind=n_ind,
                 n_yi=n_yi,
                 n_y=n_y,
                 n_isl=n_isl,
                 individual=f$ID,
                 island=f$Island_ID,
                 year_isl=f$IslandYear_ID,
                 year=year2,
                 n=f$RelPop,
                 age=f$Experienced_breeder-mean(f$Experienced_breeder),
                 sex=f$sex-mean(f$sex),
                 s=f$Own_survival,
                 v_n= var(f$logPop),
                 n_ind_isl_year=dFlokYear$estimated_pop_size,
                 IslandPopSize=IslandPopSize-mean(IslandPopSize),
                 Island_Ind=round(Island_Ind),
                 Isl_islyear=Isl_islyear)


ni <- 15000 ##Number of iterations
nc <- 3  ##Number of chains
nt <- 10 ## Thinning interval
nb <- 5000 ## Number of iterations to discard

params<-c("sigma2_YI",  
          "sigma2_Y",  
          "mean_phi", 
          "mu_phi",
          "v")

mod1<-stan_model("~/Dropbox/StochasticitySparrows/Fitness/RR_survival2.stan")
md_survival <- sampling(mod1, data = stan_data, pars = params, chains = nc, iter = ni,  warmup = nb, thin = nt, cores=3)
round(summary(md_survival)$summary[,c(6,4,8)],3)

saveRDS(md_survival, "~/Dropbox/StochasticitySparrows/Fitness/md_survival.rds")
