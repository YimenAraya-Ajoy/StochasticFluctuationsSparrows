require(MASS)
require(lme4)
require(parallel)
require(arm)
require(tidyr)
source("~/Dropbox/StochasticitySparrows/Fitness/Data_manipulation_fitness_stan.r")
an_obs<-function(sf){
  modG <-lmer(obs_w ~ RelPop + (1|IslandYear_ID) + (1|ID), data=sf)
  modNB <-glmer.nb(obs_r2s ~ RelPop +  (1|IslandYear_ID) +  (1|ID), data=sf)
  spat_temp_V_l <-VarCorr(modNB)$IslandYear_ID[1,1]
  spat_temp_V <-(exp(spat_temp_V_l )-1)* exp(2*(fixef(modNB)[1]-log(2)) + spat_temp_V_l)
  data.frame(Obs_estG=VarCorr(modG)$IslandYear_ID[1,1], Obs_estNB=spat_temp_V)
}

an<-function(sf){
  modG <-lmer(w ~ RelPop + (1|IslandYear_ID) + (1|ID), data=sf)
  modNB <-glmer.nb(r2s ~ RelPop +  (1|IslandYear_ID) +  (1|ID), data=sf)
  spat_temp_V_l <-VarCorr(modNB)$IslandYear_ID[1,1]
  spat_temp_V <-(exp(spat_temp_V_l )-1)* exp(2*(fixef(modNB)[1]-log(2)) + spat_temp_V_l)
  data.frame(estG=VarCorr(modG)$IslandYear_ID[1,1], estNB=spat_temp_V)
}

sim_sparrows_fitness<-function(sf=0, u_s=0, gamma_s=0, u_r=0, gamma_r=0, mY=matrix(0,2,2), mIY=matrix(0,2,2), mID=matrix(0,2,2), phi=0, m_RR=1, v_RR=0, LevelRR="IslandYear_ID"){
rr<-invlogit(rnorm(max(sf[,LevelRR]),m_RR, sqrt(v_RR)))

uIY<-mvrnorm(max(sf$IslandYear_ID), c(0,0), mIY)
uY<-mvrnorm(max(sf$Year2), c(0,0), mY)
uID<-mvrnorm(max(sf$ID)*clones, c(0,0), mID)

sf$v_R<- u_r  + gamma_r*sf$RelPop  + uIY[sf$IslandYear_ID,1] +  uID[sf$ID,1] + uY[sf$Year2,1]
sf$v_S<- u_s  + gamma_s*sf$RelPop  + uIY[sf$IslandYear_ID,2] +  uID[sf$ID,2] + uY[sf$Year2,2] 

sf$r<-rnbinom(n=length(sf$v_R), mu=exp(sf$v_R), size=phi)  
sf$surv<-rbinom(length(sf$v_S),1,invlogit(sf$v_S))

sf$rr<-rr[sf[,LevelRR]]
sf$obs_surv<-rbinom(length(sf$surv),  sf$surv, sf$rr)
sf$obs_r<-rbinom(length(sf$r), sf$r, sf$rr)

sf$r2s<-2*sf$surv + sf$r
sf$w<-sf$surv + sf$r/2

sf$obs_r2s<-2*sf$obs_surv + sf$obs_r
sf$obs_w <-sf$obs_surv + sf$obs_r/2
sf
}


ML_var<-function(sf, level){
var_w=var(tapply(sf$w, sf[,level], mean))
s<-tapply(sf$s, sf[,level], mean)
r<-tapply(sf$r, sf[,level], mean)
var_s<-var(s)
var_r<-var(r/2)
cov_sr<-2*cov(s, r/2)
data.frame(fitness=var_w, survival=var_s, recritment = var_r, covariance=cov_sr)
}

ML_var_obs<-function(sf, level){
  var_w=var(tapply(sf$obs_w, sf[,level], mean))
  s<-tapply(sf$obs_s, sf[,level], mean)
  r<-tapply(sf$obs_r, sf[,level], mean)
  var_s<-var(s)
  var_r<-var(r/2)
  cov_sr<-2*cov(s, r/2)
  data.frame(fitness=var_w, survival=var_s, recritment = var_r, covariance=cov_sr)
}



ML_var_Corr<-function(sf, level){
  rr<-tapply(sf$rr, sf[,level], mean)
  m_rr<-mean(rr)
  V_rr=var(rr)
  w=tapply(sf$w, sf[,level], mean)
  s<-tapply(sf$s, sf[,level], mean)
  r<-tapply(sf$r, sf[,level], mean)
  var_s<-(var(s)-(V_rr*(mean(sf$s)/m_rr)^2))/(m_rr^2+V_rr)
  var_r<-((var(r/2)-(V_rr*(mean(sf$r/2)/m_rr)^2))/(m_rr^2+V_rr))
  var_w<-((var(w)-(V_rr*(mean(sf$w)/m_rr)^2))/(m_rr^2+V_rr))
  cov_sr<-2*((cov(s,r/2)-((V_rr+m_rr^2)*mean(s)/m_rr*mean(r)/2/m_rr-mean(s)/m_rr*mean(r)/2/m_rr*m_rr^2))/(V_rr+m_rr^2))
  data.frame(fitness=var_w, survival=var_s, recritment = var_r, covariance=cov_sr)
}

ML_var_Corr2<-function(sf, level, m_RR, v_RR){
  rr<-invlogit(rnorm(100000, m_RR, sqrt(v_RR)))
  V_rr=var(rr)
  m_rr<-mean(rr)
  w=tapply(sf$obs_w, sf[,level], mean)
  s<-tapply(sf$obs_s, sf[,level], mean)
  r<-tapply(sf$obs_r, sf[,level], mean)
  var_s<-(var(s)-(V_rr*(mean(s)/mean(rr))^2))/(m_rr^2+V_rr)
  var_r<-((var(r/2)-(V_rr*(mean(r)/2/mean(rr))^2))/(m_rr^2+V_rr))
  var_w<-((var(w)-(V_rr*(mean(w)/mean(rr))^2))/(m_rr^2+V_rr))
  cov_sr<-2*((cov(s,r/2)-((V_rr+m_rr^2)*mean(s)/mean(rr)*mean(r)/2/mean(rr)-mean(s)/mean(rr)*mean(r)/2/mean(rr)*m_rr^2))/(V_rr+m_rr^2))
  data.frame(fitness=var_w, survival=var_s, recritment = var_r, covariance=cov_sr)
}

ML_var_Corr3<-function(sf, level){
  rr<-tapply(sf$rr, sf[,level], mean)
  w=tapply(sf$w, sf[,level], mean)/rr
  s<-tapply(sf$s, sf[,level], mean)/rr
  r<-tapply(sf$r, sf[,level], mean)/rr
  var_s<-var(s)
  var_r<-var(r/2)
  var_w<-var(w)
  cov_sr<-2*cov(s,r/2)
  data.frame(fitness=var_w, survival=var_s, recritment = var_r, covariance=cov_sr)
}

ML_var_Corr4<-function(sf, level, m_RR, v_RR){
  rr<-invlogit(rnorm(max(sf[,level]), m_RR, sqrt(v_RR)))
  w=tapply(sf$obs_w, sf[,level], mean)/rr
  s<-tapply(sf$obs_s, sf[,level], mean)/rr
  r<-tapply(sf$obs_r, sf[,level], mean)/rr
  var_s<-var(s)
  var_r<-var(r/2)
  var_w<-var(w)
  cov_sr<-2*cov(s,r/2)
  data.frame(fitness=var_w, survival=var_s, recritment = var_r, covariance=cov_sr)
}

