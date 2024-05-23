require(ggplot2)
require(rstan)
require(arm)
require(xtable)
source("Functions/SimulationFunctions.R")

md_fitness<-readRDS(file="Results/md_fitness.rds")
md_fitness_gaussian<-readRDS(file="Results/md_fitness_gaussian.rds")
sim_variance_uncorrected<-readRDS(file="Results/sim_variance_uncorrected.rds")
sim_variance_corrected<-readRDS(file="Results/sim_variance.rds")

source<-c("Regional","Local", "Demographic")
ests_nb<-round(as.vector(summary(md_fitness)$summary[c("temp_v","spatio_temp_v","dem_v_eff"),"50%"]),3)
ests_gaussian<-round(as.vector(summary(md_fitness_gaussian)$summary[c("temp_v","spatio_temp_v","dem_v_eff"),"50%"]),2)
ests_sim_uncorrected<-round(sim_variance_uncorrected[c(1,5,9),1],2)
ests_sim_corrected<-round(sim_variance_corrected[c(1,5,9),1],2)

res1<-data.frame(source,ests_nb, ests_gaussian, ests_sim_uncorrected, ests_sim_corrected)
res<-rbind(c("Stocahsticity ($\\sigma^2$)","","", "", ""), res1)
colnames(res)<-c("", "\\textbf{Negative binomial}", "\\textbf{Guassian}", "\\textbf{Simulation}", "\\textbf{Detection corrected}")

print(xtable(res, type = "latex", caption = "", align=c("c","l","c","c","c","c")), 
             floating=FALSE, file="Results/Table_A1.tex",include.rownames=FALSE,  
      sanitize.text.function = identity, sanitize.colnames.function=function(x){paste0("\\multicolumn{1}{c}{",x,"}")})

file.copy(from="Results/Table_A1.tex", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Tables/Table_A1.tex", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
