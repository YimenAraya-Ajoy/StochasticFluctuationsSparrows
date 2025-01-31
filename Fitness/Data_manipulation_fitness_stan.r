f<-read.table('GeneralData/fitness_data_newest_pedigree.txt', sep=";", header=TRUE)
f<-f[complete.cases(f$Sex),]
f$ringnr<-f$ID
coord<-read.table("GeneralData/island_coord.csv", sep=",", header =   TRUE)
pop<-read.table("GeneralData/demography.txt", sep=";", header = TRUE)

f$estimated_pop_size[f$estimated_pop_size==0]<-1

f$Location[f$Location=="aldra"]<-"Aldra"
f$Location[f$Location=="gjer\xf8y" ]<-"Gjerøy"
f$Location[f$Location=="hestmann\xf8y"] <-"Hestmannøy"
f$Location[f$Location=="indre kvar\xf8y"]<- "Indre-Kvarøy"
f$Location[f$Location=="lovund"]<- "Lovund"
f$Location[f$Location=="lur\xf8y-on\xf8y"]<- "Lurøy-Onøy"
f$Location[f$Location=="myken"]<- "Myken"
f$Location[f$Location=="nes\xf8y"]<- "Nesøy"
f$Location[f$Location=="selv\xe6r"]<- "Selvær"
f$Location[f$Location=="sleneset"]<- "Sleneset"
f$Location[f$Location=="tr\xe6na"]<- "Træna"

##Making factors numeric values
for(i in 1:nrow(f)){
  f$flok[i]<-coord$Code[f$Location[i]==coord$Island]
}

f$flokyear<-as.factor(paste(f$flok, f$Year))
f$Island_ID<-as.numeric(as.factor(f$Location))
f$Year2<-as.numeric(as.factor(f$Year))
f$IslandYear_ID=as.numeric(as.factor(paste(f$Island_ID, f$Year2)))
f$ID<-as.numeric(as.factor(f$ID))
f$ob<-1:nrow(f)
f$ringnr_year<-as.factor(paste(f$ringnr, f$Year))
f$Experienced_breeder = f$Least_age-1
f$Experienced_breeder[f$Experienced_breeder>0]<-1
f$sex<-as.numeric(as.factor(f$Sex))-1

pop$flokyear<-paste(pop$Island, pop$Year)
dFlokYear<-pop[pop$flokyear %in% unique(f$flokyear),]
dFlokYear$IslandYear_ID<-as.numeric(as.factor(dFlokYear$flokyear))
dFlokYear$estimated_pop_size[dFlokYear$estimated_pop_size==0]<-1

##Number of levels
n_obs<-nrow(f)
n_ind<-max(f$ID)
n_isl<-max(f$Island_ID)
n_y<-max(f$Year2)
n_yi=max(f$IslandYear_ID, na.rm=TRUE)
n_ind_IslandYear=as.vector(table(f$flokyear))

##Estimating level averages
IslandFitness <-tapply(f$r2s/2, f$Island_ID, mean, rm.na=TRUE)
IslandPopSize <-tapply(f$estimated_pop_size, f$Island_ID, mean, na.rm = TRUE)
IslandYearPopSize<-tapply(f$estimated_pop_size, f$IslandYear_ID , mean, rm.na=TRUE)
IslandYearFitness<-tapply(f$r2s/2, f$IslandYear_ID, mean, rm.na=TRUE)
IslandYearSigma_w<-tapply(f$r2s/2, f$IslandYear_ID, var, na.rm=TRUE)

##Adding level averages to the whole data set
f$IslandFitness<-IslandFitness[f$Island_ID]
f$IslandYearFitness<-IslandYearFitness[f$IslandYear_ID]
f$IslandYearSigma_w<-IslandYearSigma_w[f$IslandYear_ID]

##Derived quantities
f$RelPop <- as.vector((f$estimated_pop_size-IslandPopSize[f$Island_ID])/sd(f$estimated_pop_size))
f$logPop <- log(f$estimated_pop_size)-mean(log(f$estimated_pop_size), na.rm=TRUE)
f$VarIslandYearFitness<-(f$IslandYearFitness-f$IslandFitness)^2
IslandYearRelPopSize<-tapply(f$RelPop, f$IslandYear_ID , mean, rm.na=TRUE)
Island_Ind<-tapply(f$Island_ID, f$ID , mean, rm.na=TRUE)
Isl_islyear<-tapply(f$Island_ID, f$IslandYear_ID , mean, rm.na=TRUE)

##Sort data set
f<-f[order(f$IslandYear_ID),]
f$w<-f$N_Recruits/2 + f$Own_survival


