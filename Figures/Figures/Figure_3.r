require(rstan)

postscript("Figures/Figure_3.eps", width=7, height=5, horizontal = FALSE, onefile = FALSE, paper = "special")


source("Fitness/Data_manipulation_fitness_stan.r")
md_fitness<-readRDS(file="Results/md_fitness.rds")

par(mfrow=c(2,2), mar=c(4,4,2,2))
IY_mf<-tapply(f$w, f$IslandYear_ID, mean)
ests<-summary(md_fitness)$summary[,6]
dFlokYear$E_lambda<- exp(ests[match(grep('E_lambda', names(ests), value=TRUE), names(ests))] + ests[4]*mean(f$Experienced_breeder) + ests[2]*mean(f$sex))

plot(IY_mf~dFlokYear$estimated_pop_size, cex=1.5, xlab="Population size", ylab="Expected growth rate", col="gray")
points(dFlokYear$E_lambda ~dFlokYear$estimated_pop_size, cex=1.5)
abline(h=mean(dFlokYear$E_lambda))
mtext("A)", 3, adj=0)

source("Morphology/Data_Manipulation_Morphology_stan.R")
md_morphology<-readRDS(file="Results/md_morphology.rds")

dFlokYear$IYM<-tapply(scale(m$vekt), m$IslandYear_ID, mean, na.rm=TRUE)
plot(dFlokYear$IYM~dFlokYear$estimated_pop_size, cex=1.5, xlab="Population size", ylab="Body mass", col="gray")
mtext("B)", 3, adj=0)

ests<-summary(md_morphology)$summary[,6]
dFlokYear$IY<- ests[match(grep('YI', names(ests), value=TRUE), names(ests))]
points(dFlokYear$IY~dFlokYear$estimated_pop_size, cex=1.5)
abline(h=0)


source("Morphology/Data_Manipulation_AnimalModel_stan.R")
md_animal_model<-readRDS(file="Results/md_animal_model.rds")
ests<-summary(md_animal_model)$summary[,6]

dFY_animal<-pop[pop$flokyear %in% unique(m$flokyear),]
dFY_animal$IslandYear_ID<-as.numeric(as.factor(dFY_animal$flokyear))
dFY_animal$island_ID<- as.numeric(as.factor(dFY_animal$Island))
ests<-summary(md_animal_model)$summary[,6]
dFY_animal$meanAIY<- ests[match(grep('meanAIY', names(ests), value=TRUE), names(ests))]
delta<-ests[match(grep('delta', names(ests), value=TRUE), names(ests))]
dFY_animal$delta<-delta[match(dFY_animal$IslandYear_ID,t)]
plot(dFY_animal$delta~ dFY_animal$estimated_pop_size, cex=1.5, xlab="Population size", ylab="Change in mean breeding value")
abline(h=0)
mtext("C)", 3, adj=0)

source("Fitness/Data_Manipulation_Morphology_selection_stan.R")

require(arm)
m$z<-scale(m$vekt)
mod<-lmer(z~sex + (1|ID) + (1|Month) + (1|init), data=m)

f$m<-ranef(mod)$ID[f$ID,]
f<-f[complete.cases(f$m) ,]

summary(f$r2s)
summary(f$m)

fs<-c(1:n_yi_f)[n_ind_IslandYear_f>2]
dFY_animal$c<-NA
dFY_animal$s<-NA


for(i in 1:length(fs)){
  tmp.f<-f[f$IslandYear_ID==fs[i],]
  dFY_animal$c[fs[i]]<-cov(cbind(tmp.f$w/mean(tmp.f$w), tmp.f$m))[1,2]
  s<-coef(glm.nb(r2s~m, data=tmp.f))[2]
  dFY_animal$s[fs[i]]<-s
  }

plot(dFY_animal$s~dFY_animal$estimated_pop_size, ylim=c(-1,1), cex=1.5, xlab="Population size", ylab="Selection gradient", col="gray")
mtext("D)", 3, adj=0)

md_selection<-readRDS(file="Results/md_selection.rds")
sel<-summary(md_selection)$summary[,6]
dFY_animal$sel<-sel[match(grep('sel', names(sel), value=TRUE), names(sel))]
points(dFY_animal$sel~dFY_animal$estimated_pop_size, cex=1.5)
abline(h=sel[4])

dev.off()

file.copy(from="Figures/Figure_3.eps", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Figures/Figure_3.eps", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)



