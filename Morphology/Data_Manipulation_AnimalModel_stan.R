library(AGHmatrix)
library(hglm)
require(rstan)

ped<-read.table("/home/yi/Dropbox/data/New pedigree/helge_ped_adj_04-03-2024_est_birthyears.txt", header=TRUE)
setwd("~/Dropbox/StochasticitySparrows")
source("Morphology/Data_Manipulation_Morphology_stan.R")
p<-ped[,1:3]

p<-data.frame(id=substring(ped$id,1,7), dam=substring(ped$dam,1,7), sire=substring(ped$sire,1,7))
p[is.na(p[,2]),2]<-0
p[is.na(p[,3]),3]<-0

A  <- Amatrix(p, ploidy=2)

write.csv(p, "ped.csv")

table(complete.cases(match(unique(m$ringnr), p$id)))
table(complete.cases(match(unique(f$ringnr), p$id)))
table(complete.cases(match(unique(m$ringnr), unique(f$ringnr))))

m_animal <-droplevels(m[m$ringnr %in% p$id, ])
f_animal <-droplevels(f[f$ringnr %in% unique(m_animal$ringnr) & f$flokyear %in% unique(m_animal$flokyear), ])

A2<-A[colnames(A) %in% unique(m_animal$ringnr), colnames(A) %in% unique(m_animal$ringnr)]


IDs<-unique(m_animal[order(m_animal$ID),c("ringnr", "ID")])
row.names(IDs)<-1:nrow(IDs)
A3<-A2[match(IDs$ringnr, colnames(A2)), match(IDs$ringnr, colnames(A2))]

nrow(A3)


m_animal$ID2<-as.numeric(as.factor(m_animal$ringnr))
f_animal$ID2<-m_animal$ID2[match(f_animal$ringnr, m_animal$ringnr)]
m_animal$IslandYear_ID<-as.numeric(as.factor(m_animal$flokyear))
f_animal$IslandYear_ID<-as.numeric(as.factor(f_animal$flokyear))

nIY<-as.vector(table(f_animal$IslandYear_ID))
nI<-as.vector(table(f_animal$Island_ID))
nY<-as.vector(table(f_animal$Year))

dFY_animal<-pop[pop$flokyear %in% unique(m$flokyear),]
dFY_animal$IslandYear_ID<-as.numeric(as.factor(dFY_animal$flokyear))
dFY_animal$island_ID<- as.numeric(as.factor(dFY_animal$Island))


m_IY<-matrix(10000, max(nIY), max(m_animal$IslandYear_ID))

for(i in 1:max(m_animal$IslandYear_ID)){
  ind_tmp<-f_animal$ID2[f_animal$IslandYear_ID==i]
  m_IY[,i]<-c(ind_tmp, rep(10000,nrow(m_IY)-length(ind_tmp))) 
}

m_I<-matrix(10000, max(nI), max(m_animal$Island_ID))

for(i in 1:max(m_animal$Island_ID)){
  ind_tmp<-f_animal$ID2[f_animal$Island_ID==i]
  m_I[,i]<-c(ind_tmp, rep(10000,nrow(m_I)-length(ind_tmp))) 
}

m_Y<-matrix(10000, max(nY), max(m_animal$Year))

for(i in 1:max(m_animal$Year)){
  ind_tmp<-f_animal$ID2[f_animal$Year2==i]
  m_Y[,i]<-c(ind_tmp, rep(10000,nrow(m_Y)-length(ind_tmp))) 
}


delta<-list()
t<-t_plus1<-list()
for(i in 1:n_isl){
  dFY.tmp<-dFY_animal[dFY_animal$island_ID==i,]
  t[[i]]<-c(dFY.tmp$IslandYear_ID[-nrow(dFY.tmp)])
  t_plus1[[i]]<-c(dFY.tmp$IslandYear_ID[-1])
}

t<-unlist(t)
t_plus1<-unlist(t_plus1)

stan_data = list(m = m_animal$vekt,
                 s= m_animal$sex,
                 A = A3,
                 individual = m_animal$ID2,
                 n_ind = max(m_animal$ID2),
                 n_obs = nrow(m_animal),
                 n_yi=max(m_animal$IslandYear_ID),
                 n_y=max(m_animal$Year),
                 n_isl=max(m_animal$Island_ID),
                 island_year=m_animal$IslandYear_ID,
                 nIY=nIY,
                 m_IY=m_IY,
                 mnIY=nrow(m_IY),
                 nI=nI,
                 m_I=m_I,
                 mnI=nrow(m_I),
                 nY=nY,
                 m_Y=m_Y,
                 mnY=nrow(m_Y),
                 t=t,
                 t_plus1=t_plus1,
                 n_delta=length(t),
                 n_ind_isl_year=dFlokYear$estimated_pop_size
)
