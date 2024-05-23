require(rstan)
require(arm)
source("Fitness/Data_Manipulation_Morphology_selection_stan.R")

md_selection<-readRDS(file="Results/md_selection.rds")
ests_selection<-format(round(summary(md_selection)$summary[1:30,],2),2)

md_selection_recruits<-readRDS(file="Results/md_selection_recruits.rds")
ests_selection_recruits<-format(round(summary(md_selection_recruits)$summary[1:30,],2),2)

md_selection_survival<-readRDS(file="Results/md_selection_Survival.rds")
ests_selection_survival<-format(round(summary(md_selection_survival)$summary[1:30,],2),2)

m$z<-scale(m$vekt)
mod<-lmer(z~sex + (1|ID) + (1|Month) + (1|init), data=m)

f$m<-ranef(mod)$ID[f$ID,]
f<-f[complete.cases(f$m) ,]

z<-seq(min(f$m, na.rm = TRUE), max(f$m, na.rm = TRUE), by=0.01)

postscript("Figures/Figure_4.eps", width=3, height=6, horizontal = FALSE, onefile = FALSE, paper = "special")

par(mfrow=c(3,1), mar=c(4,4,2,2))
##Recruits
Bo<-summary(md_selection_recruits)$summary[3,6]
Bl<-summary(md_selection_recruits)$summary[4,6]
Bq<-summary(md_selection_recruits)$summary[5,6]
Ba<-summary(md_selection_recruits)$summary[7,6]
Bs<-summary(md_selection_recruits)$summary[6,6]

w=exp(Bo + Bl*z  + Bq*z^2 + Ba*0.5 + Bs*0.5)

plot(w~z, type="l", ylim=c(0,1), ylab="Recruits", xlab="Body mass (sd)")
mtext("A)", 3, adj=0)

Bls<-rnorm(1000,Bl,summary(md_selection_recruits)$summary["sigma2_YI[2]",6]
)
opts<-Bls/(-2*Bq)
abline(v=opts, col="gray")
abline(v=Bl/-(2*Bq), lty=2)

f$zc<-round(f$m)

for(i in 1:max(f$Island_ID)){
  tf<-f[f$Island_ID==i,]
  points(tapply(tf$N_Recruits, tf$zc,  mean)~ 
           jitter(as.numeric(names(tapply(tf$w, tf$zc,  mean))),1), cex=1.2)
  
}


##Adult survival
Bo<-summary(md_selection_survival)$summary[3,6]
Bl<-summary(md_selection_survival)$summary[4,6]
Bq<-summary(md_selection_survival)$summary[5,6]
Ba<-summary(md_selection_survival)$summary[7,6]
Bs<-summary(md_selection_survival)$summary[6,6]

w=invlogit(Bo + Bl*z  + Bq*z^2 + Ba*0.5 + Bs*0.5)

plot(w~z, type="l", ylim=c(0,1), ylab="Adult survival", xlab="Body mass (sd)")
mtext("B)", 3, adj=0)

Bls<-rnorm(1000,Bl,summary(md_selection_survival)$summary["sigma2_YI[2]",6]
)
opts<-Bls/(-2*Bq)
abline(v=opts, col="gray")
abline(v=Bl/-(2*Bq), lty=2)

f$zc<-round(f$m)

for(i in 1:max(f$Island_ID)){
  tf<-f[f$Island_ID==i,]
  points(tapply(tf$Own_survival, tf$zc,  mean)~ 
           jitter(as.numeric(names(tapply(tf$w, tf$zc,  mean))),1), cex=1.2)
  
}


##Fitness
Bo<-summary(md_selection)$summary[3,6]
Bl<-summary(md_selection)$summary[4,6]
Bq<-summary(md_selection)$summary[5,6]
Ba<-summary(md_selection)$summary[7,6]

w=exp(Bo + Bl*z  + Bq*z^2 + Ba*0.5-log(2))

plot(w~z, type="l", ylim=c(0,2), ylab="Fitness", xlab="Body mass (sd)")
mtext("C)", 3, adj=0)

Bls<-rnorm(1000,Bl,summary(md_selection)$summary["sigma2_YI[2]",6]
)
opts<-Bls/(-2*Bq)
abline(v=opts, col="gray")
abline(v=Bl/-(2*Bq), lty=2)

f$zc<-round(f$m)

for(i in 1:max(f$Island_ID)){
  tf<-f[f$Island_ID==i,]
  points(tapply(tf$w, tf$zc,  mean)~ 
           jitter(as.numeric(names(tapply(tf$w, tf$zc,  mean))),1), cex=1.2)
  
}

dev.off()


file.copy(from="Figures/Figure_4.eps", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Figures/Figure_4.eps", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
