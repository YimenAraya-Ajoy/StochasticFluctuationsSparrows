require(ggplot2)
require(rstan)
require(arm)
source("Functions/SimulationFunctions.R")
md_cmr<-readRDS(file="Results/md_cmr_2b.rds")

md_fitness<-readRDS(file="Results/md_recruits.rds")
ests_fitness<-summary(md_fitness)$summary[,6]
ests_cmr<-summary(md_cmr)$summary[,6]

u_s<-ests_fitness["B_S[1]"]
gamma_s<-ests_fitness["B_S[2]"]

u_r<-ests_fitness["B_R[1]"]
gamma_r<-ests_fitness["B_R[2]"]

mID<-mIY<-mY<-diag(2)

mIY[2,1]<-mIY[1,2]<-ests_fitness["cov_YI"] 
mIY[1,1]<-ests_fitness["sigma2_YI[1]"]
mIY[2,2]<-ests_fitness["sigma2_YI[2]"]
mID[1,2]<-mID[2,1]<-ests_fitness["cov_ID"] 
mID[1,1]<-ests_fitness["sigma2_ID[1]"]
mID[2,2]<-ests_fitness["sigma2_ID[2]"]

mY[1,2]<-mY[2,1]<-ests_fitness["cov_Y"] 
mY[1,1]<-ests_fitness["sigma2_Y[1]"]
mY[2,2]<-ests_fitness["sigma2_Y[2]"]
phi<-ests_fitness["phi"]

clones<-200
sf<-f[,c("ID", "Year2", "Island_ID", "IslandYear_ID", "RelPop", "Experienced_breeder")]
ssf<-sf[match(unique(sf$IslandYear_ID), sf$IslandYear_ID),]
sfd<-do.call("rbind", replicate(clones, ssf, simplify = FALSE))
sfd$ID2<-sample(sf$ID, replace=TRUE, nrow(sfd))
sfd$ID<-as.numeric(as.factor(sfd$ID2))

ssf<-sf[sf$IslandYear_ID %in% unique(sf$IslandYear_ID),]


lsf_Imp=lsf_D=lsf_Y=lsf_IY=list()

for(i in 1:100){
  lsf_IY[[i]]<-sim_sparrows_fitness(sfd, u_s=u_s, u_r=u_r, mIY=mIY, phi=phi, m_RR=ests_cmr["mu_p"], v_RR=ests_cmr["sigmaFY_p"]^2) 
  lsf_Y[[i]]<-sim_sparrows_fitness(sfd, u_s=u_s, u_r=u_r, mY=mY, phi=phi, m_RR=ests_cmr["mu_p"], v_RR=ests_cmr["Sigma2_Y_p"], LevelRR="Year2") 
  lsf_D[[i]]<-sim_sparrows_fitness(sf, u_s=u_s, u_r=u_r, phi=phi, m_RR=ests_cmr["mu_p"], v_RR=ests_cmr["sigmaFY_p"]^2) 
  }

obs_sim_IY<-do.call(rbind.data.frame,mclapply(lsf_IY,ML_var, "IslandYear_ID", mc.cores = 5))
obs_obs_IY<-do.call(rbind.data.frame,mclapply(lsf_IY,ML_var_obs, "IslandYear_ID", mc.cores = 5))
obs_corr_IY<-do.call(rbind.data.frame,mclapply(lsf_IY,ML_var_Corr2, "IslandYear_ID", mc.cores=5, m_RR=ests_cmr["mu_p"], v_RR=ests_cmr["sigmaFY_p"]^2))

data_sim_IY<-data.frame(value=apply(obs_sim_IY, 2,median), CIl=apply(obs_sim_IY, 2,quantile, 0.025),
           CIu=apply(obs_sim_IY, 2,quantile, 0.975))

data_obs_IY<-data.frame(value=apply(obs_obs_IY, 2,median), CIl=apply(obs_obs_IY, 2,quantile, 0.025),
                    CIu=apply(obs_obs_IY, 2,quantile, 0.975))

data_corr_IY<-data.frame(value=apply(obs_corr_IY, 2,median), CIl=apply(obs_corr_IY, 2,quantile, 0.025),
                   CIu=apply(obs_corr_IY, 2,quantile, 0.975))


dataIY<-rbind(data_obs_IY, data_sim_IY, data_corr_IY)

dataIY$simulation<-rep(c("B) Observed", "A) Simulated", "C) Corrected"),each=4)
dataIY$name<-as.factor(1:4)
dataIY$level<-"Local"

obs_sim_Y<-do.call(rbind.data.frame,mclapply(lsf_Y,ML_var, level="Year2", mc.cores = 5))
obs_obs_Y<-do.call(rbind.data.frame,mclapply(lsf_Y,ML_var_obs, level="Year2", mc.cores = 5))
obs_corr_Y<-do.call(rbind.data.frame,mclapply(lsf_Y,ML_var_Corr2, level="Year2", mc.cores=5, m_RR=ests_cmr["mu_p"], v_RR=ests_cmr["Sigma2_Y_p"]))

data_sim_Y<-data.frame(value=apply(obs_sim_Y, 2,median), CIl=apply(obs_sim_Y, 2,quantile, 0.025),
                        CIu=apply(obs_sim_Y, 2,quantile, 0.975))

data_obs_Y<-data.frame(value=apply(obs_obs_Y, 2,median), CIl=apply(obs_obs_Y, 2,quantile, 0.025),
                        CIu=apply(obs_obs_Y, 2,quantile, 0.975))

data_corr_Y<-data.frame(value=apply(obs_corr_Y, 2,median), CIl=apply(obs_corr_Y, 2,quantile, 0.025),
                         CIu=apply(obs_corr_Y, 2,quantile, 0.975))


dataY<-rbind(data_obs_Y, data_sim_Y, data_corr_Y)

dataY$simulation<-rep(c("B) Observed", "A) Simulated", "C) Corrected"),each=4)
dataY$name<-as.factor(1:4)
dataY$level<-"Regional"


obs_sim_D<-do.call(rbind.data.frame,mclapply(lsf_D,ML_var, "IslandYear_ID", mc.cores = 5))
obs_obs_D<-do.call(rbind.data.frame,mclapply(lsf_D,ML_var_obs, "IslandYear_ID", mc.cores = 5))
obs_corr_D<-do.call(rbind.data.frame,mclapply(lsf_D,ML_var_Corr2, "IslandYear_ID", mc.cores=5, m_RR=ests_cmr["mu_p"], v_RR=ests_cmr["sigmaFY_p"]^2))

data_sim_D<-data.frame(value=apply(obs_sim_D, 2,median), CIl=apply(obs_sim_D, 2,quantile, 0.025),
                       CIu=apply(obs_sim_D, 2,quantile, 0.975))

data_obs_D<-data.frame(value=apply(obs_obs_D, 2,median), CIl=apply(obs_obs_D, 2,quantile, 0.025),
                       CIu=apply(obs_obs_D, 2,quantile, 0.975))

data_corr_D<-data.frame(value=apply(obs_corr_D, 2,median), CIl=apply(obs_corr_D, 2,quantile, 0.025),
                        CIu=apply(obs_corr_D, 2,quantile, 0.975))

data_corr_D[,]<-0
dataD<-rbind(data_obs_D, data_sim_D, data_corr_D)

dataD$simulation<-rep(c("B) Observed", "A) Simulated", "C) Corrected"),each=4)
dataD$name<-as.factor(1:4)
dataD$level<-"Demographic"


dataRecap<-rbind(dataY, dataIY, dataD)


