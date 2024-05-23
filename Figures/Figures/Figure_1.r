require(rstan)
source("Fitness/Data_manipulation_fitness_stan.r")

postscript("Figures/Figure_1.eps", width=3.35, height=6, horizontal = FALSE, onefile = FALSE, paper = "special")


par(mfrow=c(3,1), mar=c(4,4,2,2))
IY_mf<-tapply(f$w, f$IslandYear_ID, mean)
dFlokYear$IY_mf<-IY_mf
dFlokYear$Island_ID<-as.numeric(as.factor(dFlokYear$Island))


dtmp<-dFlokYear[dFlokYear$Island_ID==i,]
plot(dtmp$IY_mf~dtmp$Year, ylim=c(0,3), xlim=c(min(dFlokYear$Year), max(dFlokYear$Year)), cex=1.5, xlab="Year", ylab="Expected growth rate", col=i, type="l", xaxt="n")
mtext("A)", 3, adj=0)

axis(1, seq(1998, 2018, 2), seq(1998, 2018, 2))
for(i in 1:n_isl){
  dtmp<-dFlokYear[dFlokYear$Island_ID==i,]
  points(dtmp$IY_mf~dtmp$Year, type="l", col=sample(c("black","gray")))
  }


source("Morphology/Data_Manipulation_Morphology_stan.R")
dFlokYear$IYM<-tapply(m$vekt, m$IslandYear_ID, mean, na.rm=TRUE)
dFlokYear$Island_ID<-as.numeric(as.factor(dFlokYear$Island))

dtmp<-dFlokYear[dFlokYear$Island_ID==1,]
plot(dtmp$IYM~dtmp$Year,ylim=c(28,36), xlim=c(min(dFlokYear$Year), max(dFlokYear$Year)), cex=1.5, xlab="Year", 
     ylab="Mean body mass", col=i, type="l", xaxt="n")
axis(1, seq(1998, 2018, 2), seq(1998, 2018, 2))
mtext("B)", 3, adj=0)


for(i in 1:n_isl){
  dtmp<-dFlokYear[dFlokYear$Island_ID==i,]
  points(dtmp$IYM~dtmp$Year, type="l", col=sample(c("black","gray")))
}

plot(dtmp$estimated_pop_size~dtmp$Year,ylim=c(0,300), xlim=c(min(dFlokYear$Year), max(dFlokYear$Year)), cex=1.5, xlab="Year", 
     ylab="Population size", col=i, type="l", xaxt="n")
axis(1, seq(1998, 2018, 2), seq(1998, 2018, 2))
mtext("C)", 3, adj=0)


for(i in 1:n_isl){
  dtmp<-dFlokYear[dFlokYear$Island_ID==i,]
  points(dtmp$estimated_pop_size~dtmp$Year, type="l", col=sample(c("black","gray")))
}
dev.off()



file.copy(from="Figures/Figure_1.eps", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Figures/Figure_1.eps", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

