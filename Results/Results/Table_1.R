require(rstan)
require(xtable)

parnames<-c('\\textbf{Fixed effects} ($\\beta$)', 'Intercept', 
            'Age', 'Sex', "Age:Sex" ,'Pop size',
          '\\textbf{Random effects} ($\\sigma$)', 'Island:intercept', 'Island:n','Island int-n cov',
          'Year', "Year rec-surv cov" , 'Island-year', 'Island-year rec-surv cov',
          'Individual', 'Individual rec-surv cov', 'Residual') 


ests<-function(a,x){paste(a[x,6], " (", a[x,4] , ", ", a[x,8], ")", sep="")}

md_fitness<-readRDS(file="Results/md_recruits.rds")

ests_fitness<-format(round(summary(md_fitness)$summary[,],2),2)

Intercept_recruits<-ests(ests_fitness,"B_R[1]")
B_a_recruits<-ests(ests_fitness,"B_R[4]")
B_s_recruits<-ests(ests_fitness,"B_R[3]")
B_a_s_recruits<-ests(ests_fitness,"B_R[5]")

B_n_recruits<-ests(ests_fitness,"B_R[2]")
Isl_Int_recruits<-ests(ests_fitness,"sigma2_Isl_R[1]")
Isl_n_recruits<-ests(ests_fitness,"sigma2_Isl_R[2]")
Isl_cov<-ests(ests_fitness,"cov_Isl_R")

Year_recruits<-ests(ests_fitness, "sigma2_Y[1]")
Year_cov<-ests(ests_fitness, "cov_Y")

Island_year_recruits<-ests(ests_fitness,"sigma2_YI[1]")
Island_year_cov<-ests(ests_fitness, "cov_YI")

Individual_recruits<-ests(ests_fitness,"sigma2_ID[1]")
Individual_cov<-ests(ests_fitness,"cov_ID")

Observation_recruits<-ests(ests_fitness,"sigma2_ID[1]")
Recruits_Model<-c("",Intercept_recruits, B_a_recruits, B_s_recruits,  B_a_s_recruits, B_n_recruits,""
                 , Isl_Int_recruits, Isl_n_recruits, Isl_cov, Year_recruits, Year_cov, Island_year_recruits,Island_year_cov, Individual_recruits,Individual_cov, Observation_recruits)

Cov<-c("","", "", "",  "", "",""
       , "", "", Isl_cov, "", Year_cov, "",Island_year_cov, "",Individual_cov, Observation_recruits)


Intercept_survival<-ests(ests_fitness,"B_S[1]")
B_a_survival<-ests(ests_fitness,"B_S[4]")
B_s_survival<-ests(ests_fitness,"B_S[3]")
B_a_s_survival<-ests(ests_fitness,"B_S[5]")

B_n_survival<-ests(ests_fitness,"B_S[2]")
Isl_Int_survival<-ests(ests_fitness,"sigma2_Isl_S[1]")
Isl_n_survival<-ests(ests_fitness,"sigma2_Isl_S[2]")
Isl_cov<-ests(ests_fitness,"cov_Isl_S")

Year_survival<-ests(ests_fitness, "sigma2_Y[2]")
Island_year_survival<-ests(ests_fitness,"sigma2_YI[2]")
Individual_survival<-ests(ests_fitness,"sigma2_ID[2]")
Observation_survival<-""
Survival_Model<-c("",Intercept_survival, B_a_survival, B_s_survival,  B_a_s_survival, B_n_survival,""
                  , Isl_Int_survival, Isl_n_survival, Isl_cov, Year_survival, "", Island_year_survival,"", Individual_survival, "",Observation_survival)


md_fitness<-readRDS(file="Results/md_fitness.rds")

ests_fitness<-format(round(summary(md_fitness)$summary[1:16,],2),2)

Intercept_fitness<-paste(ests_fitness["B[1]",6], " (", ests_fitness["B[1]",4] , ", ", ests_fitness["B[1]",8], ")", sep="")
Intercept_fitness<-ests(ests_fitness,"B[1]")
B_a_fitness<-ests(ests_fitness,"B[4]")
B_s_fitness<-ests(ests_fitness,"B[3]")
B_a_s_fitness<-ests(ests_fitness,"B[5]")

B_n_fitness<-ests(ests_fitness,"B[2]")
Isl_Int_fitness<-ests(ests_fitness,"sigma_isl[1]")
Isl_n_fitness<-ests(ests_fitness,"sigma_isl[2]")
Isl_cov<-ests(ests_fitness,"cov_Isl")

Year_fitness<-ests(ests_fitness, "sigma[3]")
Island_year_fitness<-ests(ests_fitness,"sigma[2]")
Individual_fitness<-ests(ests_fitness,"sigma[1]")
Observation_fitness<-ests(ests_fitness,"phi")
Fitness_Model<-c("",Intercept_fitness, B_a_fitness, B_s_fitness,  B_a_s_fitness, B_n_fitness,""
                 , Isl_Int_fitness, Isl_n_fitness, Isl_cov, Year_fitness, "",Island_year_fitness, "",Individual_fitness,"", Observation_fitness)



md_morphology<-readRDS(file="Results/md_morphology.rds")

ests_morph<-format(round(summary(md_morphology)$summary[1:16,],2),2)

Intercept_Morph<-ests(ests_morph,"B[1]")
B_a_Morph<-ests(ests_morph,"B[4]")
B_s_Morph<-ests(ests_morph,"B[3]")
B_n_Morph<-ests(ests_morph,"B[2]")
Isl_Int_Morph<-ests(ests_morph,"sigma_Isl[1]")
Isl_n_Morph<-ests(ests_morph,"sigma_Isl[2]")
Isl_cov_Morph<-ests(ests_morph,"cov_Isl")
Year_Morph<-ests(ests_morph,"sigma_B0[3]")
Island_year_Morph<-ests(ests_morph,"sigma_B0[2]")
Individual_Morph<-ests(ests_morph,"sigma_B0[1]")
Observation_Morph<-ests(ests_morph,"sigma_B0[4]")

Mass_Model<-c("",Intercept_Morph, B_a_Morph, B_s_Morph, "", B_n_Morph,""
                 , Isl_Int_Morph, Isl_n_Morph, Isl_cov_Morph,Year_Morph, "",Island_year_Morph,  "",Individual_Morph, "",Observation_Morph)


res<-cbind(parnames, Recruits_Model, Survival_Model, Fitness_Model, Mass_Model)
res
colnames(res)<-c(" ", "\\textbf{Recruits}",  "\\textbf{Survival}", "\\textbf{Fitness}", "\\textbf{Mass}")
                 
print(xtable(res, type = "latex", caption = "", align=c("c","l","c","c","c", "c")) , 
      floating=FALSE, file="Results/Table_1.tex",include.rownames=FALSE,  
      sanitize.text.function = identity, sanitize.colnames.function=function(x){paste0("\\multicolumn{1}{c}{",x,"}")})


file.copy(from="Results/Table_1.tex", to="/home/yi/Dropbox/Apps/Overleaf/StochasticFluctuationsSparrows/Tables/Table_1.tex", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)



