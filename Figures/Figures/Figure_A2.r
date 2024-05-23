require(rstan)
source("Figures/Figure_3.r")

postscript("Figures/Figure_A2.eps", width=3.5, height=3.5, horizontal = FALSE, onefile = FALSE, paper = "special")
plot(dFY_animal$delta~dFY_animal$c, ylab="Change in breeding value", 
     xlab="Covariance mass-relative fitness", ylim=c(-0.5,0.5),xlim=c(-1,1), cex=1.2)
cor.test(dFY_animal$delta,dFY_animal$c)
abline(h=0, lty=2)
abline(v=0, lty=2)
dev.off()

file.copy(from="Figures/Figure_A2.eps", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Figures/Figure_A2.eps", 
         overwrite = TRUE, recursive = FALSE, 
         copy.mode = TRUE)
