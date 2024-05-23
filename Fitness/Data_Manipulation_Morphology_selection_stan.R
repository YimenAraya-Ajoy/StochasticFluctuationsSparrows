m3<-read.table("GeneralData/morphology.txt", sep=";", header=TRUE)
source("Fitness/Data_manipulation_fitness_stan.r")
pop<-read.table("GeneralData/demography.txt", sep=";", header = TRUE)

pop$flokyear<-paste(pop$Island, pop$Year)
m3$y<-as.numeric(substring(m3$dato,1,4))
m3$flokyear<-as.factor(paste(m3$flok, m3$y))
m<-m3[complete.cases(m3$vekt),]

rnm<-data.frame(ringnr=unique(m$ringnr))
rnf<-data.frame(ringnr=unique(f$ringnr))

fym<-data.frame(flokyear=unique(m$flokyear))
fyf<-data.frame(flokyear=unique(f$flokyear))

rn<-merge(rnf, rnm)
fy<-merge(fyf, fym)

m1<-droplevels(m[m$ringnr %in% rn$ringnr & m$flokyear %in% fy$flokyear,])
f1<-droplevels(f[f$ringnr %in% rn$ringnr & f$flokyear %in% fy$flokyear,])

f<-f1[f1$ringnr %in% unique(m1$ringnr) & f1$flokyear %in% unique(m1$flokyear),]
m<-m1[m1$ringnr %in% unique(f$ringnr) & m1$flokyear %in% unique(f$flokyear),]

dFlokYear<-pop[pop$flokyear %in% unique(m$flokyear),]
dFlokYear$ID<-as.numeric(as.factor(dFlokYear$flokyear))

##Makingfactors numeric values
m$flok<-droplevels(as.factor(m$flok))
f$flok<-droplevels(as.factor(f$flok))

m$Island_ID<-as.numeric(as.factor(m$flok))
f$Island_ID<-as.numeric(as.factor(f$flok))

m$flokyear<-as.factor(paste(m$flokyear))
f$flokyear<-as.factor(paste(f$flokyear))

m$Year<-as.numeric(as.factor(m$y))
f$Year<-as.numeric(as.factor(f$Year))

m$IslandYear_ID=as.numeric(droplevels(as.factor(m$flokyear)))
f$IslandYear_ID=as.numeric(droplevels(as.factor(f$flokyear)))

m$ID<-as.numeric(droplevels(as.factor(m$ringnr)))
f$ID<-as.numeric(droplevels(as.factor(f$ringnr)))

m$IDYear=as.numeric(as.factor(paste(m$ID, m$Year)))
f$IDYear=as.numeric(as.factor(paste(f$ID, f$Year)))

m$observer_ID=as.numeric(as.factor(m$init))
m$observer_ID[is.na(m$observer_ID)]<-max(m$observer_ID, na.rm=TRUE)+1

m$Month<-as.numeric(as.factor(substring(m$dato,6,7)))

m$sex<-as.numeric(as.factor(m$scriptsex))
f$sex<-as.numeric(as.factor(f$sex))

m$sex<-f$sex[match(m$ringnr, f$ringnr)]
m$sex[is.na(m$sex)]<-0

m$sex[m$sex==2]<-0
f$sex[f$sex==2]<-0
m$logPop<-log(IslandYearPopSize[m$IslandYear_ID])-mean(log(IslandYearPopSize[m$IslandYear_ID]))

##Number of levels
n_isl_m<-max(m$Island_ID, na.rm=TRUE)
n_isl_f<-max(f$Island_ID, na.rm=TRUE)

n_y_m<-max(m$Year, na.rm=TRUE)
n_y_f<-max(f$Year, na.rm=TRUE)

n_yi_m=max(m$IslandYear_ID, na.rm=TRUE)
n_yi_f=max(f$IslandYear_ID, na.rm=TRUE)

n_ind_m<-max(m$ID, na.rm = TRUE)
n_ind_f<-max(f$ID, na.rm = TRUE)

n_observ=max(m$observer_ID, na.rm=TRUE)
n_m_m=max(m$Month, na.rm=TRUE)

n_ind_IslandYear_m=as.vector(table(m$IslandYear_ID))
n_ind_IslandYear_f=as.vector(table(f$IslandYear_ID))

