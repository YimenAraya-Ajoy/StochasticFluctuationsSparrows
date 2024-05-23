require(xtable)
ests<-function(a,x){paste(a[x,6], " (", a[x,4] , ", ", a[x,8], ")", sep="")}

sim_obs<-function(md,sigma,n){
  v<-rep(NA,1000)
  list_of_draws <- extract(md)
  
  for(i in 1:1000){
    sigma2<-sample(unlist(list_of_draws[sigma]),1)
    mu_phi<-sample(unlist(list_of_draws$mu_phi),2)
    p<-rep(invlogit(rnorm(n,mu_phi, sqrt(sigma2))),each=500)
    ID<-rep(1:n, each=500)
    s<-rbinom(length(p),1,p)
    v[i]<-var(tapply(s,ID, mean))
  }
  sim<-format(round(c(mean(v), quantile(v, c(0.025,0.975))),3),3)
  ests_sim_sigma<-paste(sim[1], " (", sim[2] , ", ", sim[3], ")", sep="")
  ests_sim_sigma
  }


source("Functions/SimulationFunctions.R")
md_surv<-readRDS(file="Results/md_survival.rds")
md_cmr2<-readRDS(file="Results/md_cmr_2.rds")
md_cmr3<-readRDS(file="Results/md_cmr_3.rds")

ests_cmr3<-format(round(summary(md_cmr3)$summary[,],2),2)
ests_cmr2<-format(round(summary(md_cmr2)$summary[,],2),2)
ests_surv<-format(round(summary(md_surv)$summary[,],2),2)

par<-c( "Mean survival",
        "Mean recapture",
        "Var island-year log odds surv", "Var year log odds surv",
        "Var island-year surv", "Var year surv",
        "Var island-year log odds p", "Var year log odds p"
)

###Model 1


Model_1<-c( ests(ests_surv, "mean_phi"), 
           "",
           ests(ests_surv, "sigma2_YI"), 
           ests(ests_surv, "sigma2_Y"),
           sim_obs(md_surv,"sigma2_YI", 238),
           sim_obs(md_surv,"sigma2_Y", 21),
           "", 
           "")


Model_2<-c( ests(ests_cmr2, "mean_phi"),
            ests(ests_cmr2, "mean_p"), 
            ests(ests_cmr2, "Sigma2_YI_phi"), 
            ests(ests_cmr2, "Sigma2_Y_phi"),
            sim_obs(md_cmr2,"Sigma2_YI_phi", 238),
            sim_obs(md_cmr2,"Sigma2_Y_phi", 21),
            ests(ests_cmr2, "Sigma2_YI_p"), 
            "") 

Model_3<-c(ests(ests_cmr3, "mean_phi"), 
           ests(ests_cmr3, "mean_p"),
           ests(ests_cmr3, "Sigma2_YI_phi"), 
           ests(ests_cmr3, "Sigma2_Y_phi"),
           sim_obs(md_cmr3,"Sigma2_YI_phi", 238),
           sim_obs(md_cmr3,"Sigma2_Y_phi", 21),
           ests(ests_cmr3, "Sigma2_YI_p"), 
           ests(ests_cmr3, "Sigma2_Y_p")) 

res<-cbind(par, Model_1, Model_2, Model_3)
colnames(res)<-c(" ", "Model 1", "Model 2", "Model 3")

print(xtable(res, type = "latex", caption = "", align=c("c","l","c","c", "c")), 
             floating=FALSE, file="Results/Table_A2.tex",include.rownames=FALSE,  
      sanitize.text.function = identity, sanitize.colnames.function=function(x){paste0("\\multicolumn{1}{c}{",x,"}")})

file.copy(from="Results/Table_A2.tex", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Tables/Table_A2.tex", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

