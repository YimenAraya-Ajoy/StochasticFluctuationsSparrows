require(rstan)
require(ggplot2)
source("Functions/SimulationFunctions.R")

md_fitness<-readRDS(file="Results/md_recruits.rds")
md_cmr<-readRDS(file="Results/md_cmr_3.rds")

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

clones<-100
sf<-f[,c("ID", "Year2", "Island_ID", "IslandYear_ID", "RelPop", "Experienced_breeder")]
sfd<-do.call("rbind", replicate(clones, sf[unique(sf$IslandYear_ID),], simplify = FALSE))
sfd$ID2<-sample(sf$ID, replace=TRUE, nrow(sfd))
sfd$ID<-as.numeric(as.factor(sfd$ID2))


lsf_Imp=lsf_D=lsf_Y=lsf_IY=list()

for(i in 1:100){
  lsf_IY[[i]]<-sim_sparrows_fitness(sfd, u_s=u_s, u_r=u_r, mIY=mIY, phi=phi, m_RR=ests_cmr["mu_p"], v_RR=ests_cmr["Sigma2_YI_p"]) 
  lsf_Y[[i]]<-sim_sparrows_fitness(sfd, u_s=u_s, u_r=u_r, mY=mY, phi=phi, m_RR=ests_cmr["mu_p"], v_RR=ests_cmr["Sigma2_Y_p"], LevelRR="Year2") 
  lsf_D[[i]]<-sim_sparrows_fitness(sf, u_s=u_s, u_r=u_r, phi=phi)
}

obs_IY<-do.call(rbind.data.frame,mclapply(lsf_IY, ML_var, level="IslandYear_ID", mc.cores = 5))

obs_Y<-do.call(rbind.data.frame,mclapply(lsf_Y, ML_var, level="Year2", mc.cores = 5))

obs_D<-do.call(rbind.data.frame,mclapply(lsf_D,ML_var, "IslandYear_ID", mc.cores=5))


data_IY<-data.frame(value=apply(obs_IY, 2,median), CIl=apply(obs_IY, 2,quantile, 0.025),
           CIu=apply(obs_IY, 2,quantile, 0.975))

data_Y<-data.frame(value=apply(obs_Y, 2,median), CIl=apply(obs_Y, 2,quantile, 0.025),
                    CIu=apply(obs_Y, 2,quantile, 0.975))

data_D<-data.frame(value=apply(obs_D, 2,median), CIl=apply(obs_D, 2,quantile, 0.025),
                   CIu=apply(obs_D, 2,quantile, 0.975))



data_D["covariance",]<-0

data<-rbind(data_Y, data_IY, data_D)


data$level<-rep(c("Regional", "Local", "Demographic"),each=4)
data$name<-as.factor(1:4)
saveRDS(data, "Results/sim_variance_uncorrected.rds")

