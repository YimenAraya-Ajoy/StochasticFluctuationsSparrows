require(rstan)
require(xtable)
source("Morphology/Data_Manipulation_AnimalModel_stan.R")
md_animal_model<-readRDS(file="Results/md_animal_model.rds")
ests_animal<-round(summary(md_animal_model)$summary[,],2)


parnames<-c('\\textbf{Fixed effects} ($\\beta$)', 'Intercept', 
            'Sex','\\textbf{Random effects} ($\\sigma^2$)', 'Additive genetic', 'Permanent environment', 'Island Year', "Drift", 'Residual',
            "\\textbf{Proportions}",'Heritability', "Drift") 


ests<-function(a,x){paste(a[x,6], " (", a[x,4] , ", ", a[x,8], ")", sep="")}

Intercept<-ests(ests_animal, "mu1")
Sex<-ests(ests_animal, "B_sex")
AdditiveGenetic<-ests(ests_animal, "sigma2_G")
PermanentEnvironment<-ests(ests_animal, "sigma2_I")
Drift_g<-ests(ests_animal, "drift_g")
Residual<-ests(ests_animal, "sigma2_R")

Drift_p<- ests(ests_animal, "p_drift")
IslandYear<-ests(ests_animal, "sigma2_IY")
Residual<-ests(ests_animal, "sigma2_R")
Heritability<-ests(ests_animal, "h2")
Evolvability<-ests(ests_animal, "cv")

ests<-c("", Intercept, Sex, "", AdditiveGenetic, PermanentEnvironment, IslandYear, Drift_g, Residual, "", Heritability, Drift_p)

res<-cbind(parnames,ests)
res
colnames(res)<-c("Parameters", "Animal Model")

print(xtable(res, type = "latex"), floating=FALSE, file="Results/Table_A3.tex",include.rownames=FALSE,  
      sanitize.text.function = identity, sanitize.colnames.function=function(x){paste0("\\multicolumn{1}{c}{",x,"}")})

file.copy(from="Results/Table_A3.tex", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Tables/Table_A3.tex", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

