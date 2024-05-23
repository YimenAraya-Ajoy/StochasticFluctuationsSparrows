data {
    //Density dependent fitness variance dataset
    // Number of clusters (an integer)
    int<lower=0> n_obs; // number of observations for fitness expressions
    int<lower=0> n_ind; //number of individuals
    //int<lower=0> nnp; // length of population size vector for predicting the genetic variance in fitness. Add length year and length island
    int<lower=0> n_isl; //length of island vector
    int<lower=0> n_yi; //length of year island vector
    int<lower=0> n_y; //length of year island vector
    vector[n_yi] n_ind_isl_year; //length of year island vector
    
    // Clusters indentifiers (an integer)
    int<lower=0> individual[n_obs];  //  Individual identites.
    int<lower=0> island[n_obs]; //island identities (names)
    int<lower=0> year_isl[n_obs]; // year_island identities (names)
    int<lower=0> year[n_obs]; // year_island identities (names)
     
    // Continuous predictors
    real n[n_obs];  // log pop sizes
    //real IslandPopSize[n_isl];  // log pop sizes
   
    real<lower=-1, upper = 1> age[n_obs]; // age (experienced breeder)
    real<lower=-1, upper = 1> sex[n_obs]; // sex
    real<lower=0> v_n;
    // continuous response variable
    real<lower=0> w[n_obs];  // fitness (recruits)
  }
  
  parameters {
     // Define parameters to estimate
    // Parameters for model of metapop-pop-individual density reg. function
     vector[5]   B; //
    
    real<lower=0> sigma_res; // negative binomial overdispersion parameter
    
    // Random effects. Do this for all random effect levels (Location (int and slope), Year_Location (intercept))
    vector[n_ind]         zI; //matrix scaled individual blups (intercepts)
    matrix[2, n_isl]        zIsl; //matrix scaled island blups(intercepts and slopes)// sd island intercepts and slopes
    cholesky_factor_corr[2] Lisl;  // factor to estimate covariance int-slopes. To estimate covariance. Here we specify the off-diagonal number of cells in the variance covariance matrix. We have two random effect levels, so there are two.
    vector[n_yi]           zYI; //vector scaled year island blups(intercepts)
    // sd year island intercepts
     vector[n_y]            zY; //vector scaled year island blups(intercepts)
     vector<lower=0>[3]         sigma;
     vector<lower=0>[2]         sigma_isl;
  }
  
    transformed parameters{
    vector[n_ind]   I; //  Unscaled (the actual values, not the scaled) blups intercept for individuals
    matrix[2, n_isl]  Isl;//  Location int and slope blups matrix
    vector[n_yi]      YI; //  Year_Location intercept blups vector
    vector[n_y]       Y; //  Year_Location intercept blups vector
    real v[n_obs]; // predicted value for log fitness. Could be below zero
    
    I  =  zI * sigma[1]; // ind intercept
    YI = zYI * sigma[2]; //  equation for random Year_Location intercept. Since this is univariate we don't need the cholesky tranformation
    Y =  zY * sigma[3]; //  equation for random Year intercept. Since this is univariate we don't need the cholesky tranformation
    
    Isl = diag_pre_multiply(sigma_isl, Lisl) * zIsl;// equation for random Location intercept and slope
    
   for (i in 1:n_obs) 
     v[i]  = B[1] +  I[individual[i]] + Isl[1, island[i]] + YI[year_isl[i]] + Y[year[i]]  + (B[2]+ Isl[2, island[i]])*n[i] + B[3]*sex[i] + B[4]*age[i] + B[5]*sex[i]*age[i]; //add Location slope intercept matrix and add Year_Location intercept vector
   
}
 
model {
// Create vector of predicted values
 to_vector(B) ~ normal(0, 1); 

 // Random effects prior distribution
    to_vector(zI) ~ normal(0, 1); //this is scaling the sampling of the blups to have mean 0 and sd 1. This is a z standardisation
    to_vector(zIsl) ~ normal(0, 1);
    to_vector(zYI) ~ normal(0, 1);
    to_vector(zY) ~ normal(0, 1);
        
   to_vector(sigma) ~ normal(0, 1);
   to_vector(sigma_isl) ~ normal(0, 1);
      
    sigma_res ~ normal(0, 1);
     //Li ~ lkj_corr_cholesky(4); // unclear why there is a 4. 
    Lisl ~ lkj_corr_cholesky(2); // unclear why there is a 4. 
   
    // Likelihood function
     w~normal(v, sigma_res); // sampling statement. How do the obsed values fit the predicted
}

generated quantities{
real<lower=0> sigma2_I; 
real<lower=0> sigma2_YI; 
real<lower=0> sigma2_Y;
vector<lower=0>[2]  sigma2_Isl; 
matrix[2,2] Omega_Isl;
real cov_Isl;

real<lower=0> temp_v;
real<lower=0> ind_v;
real<lower=0> spatial_v;
real<lower=0> spatio_temp_v;
real<lower=0> dem_v; 
real<lower=0> den_v; 
real<lower=0> dem_v_eff_isl_year[n_yi];
real<lower=0> dem_v_eff;
real<lower=0> tot;

real<lower=0> p_temp_v;
real<lower=0> p_ind_v;
real<lower=0> p_spatial_v;
real<lower=0> p_spatio_temp_v;
real<lower=0> p_den_v;
real<lower=0> p_dem_v_eff;

//Standard deviations to variance
sigma2_I   = sigma[1]^2;
sigma2_YI  = sigma[2]^2;
sigma2_Y   = sigma[3]^2; 

sigma2_Isl[1]= sigma_isl[1]^2; 
sigma2_Isl[2]= sigma_isl[2]^2; 
Omega_Isl = Lisl * Lisl'; 
cov_Isl = Omega_Isl[1,2]*sqrt(sigma2_Isl[1]*sigma2_Isl[2]);

ind_v         = sigma2_I;
spatio_temp_v = sigma2_YI;
temp_v        = sigma2_Y;
spatial_v     = sigma2_Isl[1];
dem_v         = sigma_res^2;

den_v = (B[2]^2 + sigma2_Isl[2])*v_n;

for(i in 1:n_yi)
  dem_v_eff_isl_year[i] = dem_v/n_ind_isl_year[i];


dem_v_eff= sum(dem_v_eff_isl_year)/n_yi;

tot = spatio_temp_v + temp_v + spatial_v + dem_v_eff + den_v;

//Proportions
p_ind_v =ind_v/tot; 
p_spatio_temp_v =spatio_temp_v/tot; 
p_temp_v = temp_v/tot;  
p_spatial_v = spatial_v/tot;
p_dem_v_eff = dem_v_eff/tot;  
p_den_v = den_v/tot;
}
