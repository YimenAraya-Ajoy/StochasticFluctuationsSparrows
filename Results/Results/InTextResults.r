#First section
require(rstan)
source("Fitness/Data_manipulation_fitness_stan.r")
md_fitness<-readRDS(file="Fitness/md_fitness.rds")

##Number of individuals
max(f$ID)

##Number of individual breeding seasons
nrow(f)

###Percentage of individuals present in x breeding seasons
round(table(table(f$ID))/sum(table(table(f$ID)))*100)

##Mean mean fitness
round(mean(tapply(f$w, f$IslandYear_ID, mean)),2)

##Variance in mean fitness
round(var(tapply(f$w, f$IslandYear_ID, mean)),2)

##Percentage of variance explained in growth rates
round(summary(md_fitness)$summary[20:25,c(1,5,8)],2)*100


#Second section
source("Morphology/Data_Manipulation_Morphology_stan.R")
md_morphology<-readRDS(file="Morphology/md_morphology.rds")

##Number of individuals with adult measurements of mass
max(m$ID)

##Percentage of individuals measured x number of times
round(table(table(m$ID))/sum(table(table(m$ID)))*100)

###Island year combinations
max(m$IslandYear_ID)

###Mean mass
mean(tapply(m$vekt, m$IslandYear_ID, mean))
var(tapply(m$vekt, m$IslandYear_ID, mean))


#Percentage of variance explained in growth rates
round(summary(md_morphology)$summary[20:24,c(1,5,8)],2)*100

#Third section
source("Morphology/Data_Manipulation_AnimalModel_stan.R")
md_animal_model<-readRDS(file="Morphology/md_animal_model.rds")


round(summary(md_animal_model)$summary[1:20,c(1,5,8)],2)


summary(md_animal_model)$summary[1:24,c(1,5,8)]


#Fourth section
source("Fitness/Data_Manipulation_Morphology_selection_stan.R")
md_selection<-readRDS(file="Fitness/md_selection.rds")
ests_selection<-summary(md_selection)$summary[,]


##Number of individuals analyzed
max(m$ID)

##Number of individual breeding seasons
nrow(f)

##Number of island year combinations
max(f$IslandYear_ID)

##Mean body mass
mean(m$vekt)


##Optimal body mass$
round(ests_selection["optim", c(6,4,8)],1)


##Spatio-temporal variance in selection
round(ests_selection["sigma2_YI[2]", c(6,4,8)],3)

##Variance in the optimum
round(ests_selection["CV", c(6,4,8)],2)

linearsel<-extract(md_selection)$sel
quadsel2<-(-2*extract(md_selection)$B_f[,3])

opt<-matrix(NA, nrow(linearsel), ncol(linearsel))
var_opt<-rep(NA, nrow(linearsel))

for(i in 1: ncol(linearsel)){
  opt[,i]<-linearsel[,i]/quadsel2 
}

var(apply(opt,2,median))*(sd(m$vekt))^2

var(tapply(m$vekt, m$IslandYear_ID, mean))
