require(ggplot2)
data<-readRDS(file="Results/sim_variance.rds")

panel_labels=c("1" = "Observed", "2" = "Corrected")
p<-ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity") +
  geom_errorbar( aes(x=name, y=value, ymin=CIl, ymax=CIu), width=0.4) + ylab("Stochasticity") + xlab("Source") +
  scale_x_discrete(labels=c('Fitness', 'Survival', 'Recruits', 'Covariance'))  +
  facet_grid(type~level) + theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), strip.text.y = element_text(size = 12), strip.text.x = element_text(size = 12), panel.background = element_blank(),  # Remove gray background
       panel.grid.major = element_blank(),  # Remove major grid lines
       panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))


ggsave("Figures/Figure_2.eps", plot = p, device = "eps", width = 7, height = 4.3)

file.copy(from="Figures/Figure_2.eps", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Figures/Figure_2.eps", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
