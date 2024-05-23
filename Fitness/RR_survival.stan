data {
    //Density dependent fitness variance dataset
    // Number of clusters (an integer)
    int<lower=0> n_obs; // number of observations for fitness expressions
    int<lower=0> n_yi; //length of year island vector
    int<lower=0> n_y; //length of year island vector
     
    // Clusters indentifiers (an integer)
    int<lower=0> year_isl[n_obs]; // year_island identities (names)
    int<lower=0> year[n_obs]; // year_island identities (names)
     
    // Continuous predictors
    real n[n_obs];  // log pop sizes
    //real IslandPopSize[n_isl];  // log pop sizes
   
    // continuous response variable
    int<lower=0> s[n_obs];  // fitness (recruits)
  }
  
  parameters {
     // Define parameters to estimate
    // Parameters for model of metapop-pop-individual density reg. function
     real   mu_phi; //
     vector[n_yi]           zYI; 
     vector[n_y]            zY;
     vector<lower=0>[2]     sigma;
    }
  
    transformed parameters{
    vector[n_yi]      YI; //  Year_Location intercept blups vector
    vector[n_y]       Y; //  Year_Location intercept blups vector
    real v[n_obs]; // predicted value for log fitness. Could be below zero
    
    YI = zYI * sigma[1]; //  equation for random Year_Location intercept. Since this is univariate we don't need the cholesky tranformation
    Y =  zY * sigma[2]; //  equation for random Year intercept. Since this is univariate we don't need the cholesky tranformation

   for (i in 1:n_obs) 
     v[i]  = inv_logit(mu_phi  + YI[year_isl[i]] + Y[year[i]]);  
}
 
model {
 mu_phi ~ normal(0, 1); 
 to_vector(zYI) ~ normal(0, 1);
 to_vector(zY) ~ normal(0, 1);
 to_vector(sigma) ~ normal(0, 1);
 s~bernoulli(v);
}

generated quantities{
real<lower=0> sigma2_YI; 
real<lower=0> sigma2_Y;
real<lower=0,upper=1> mean_phi;    // Mean survival

sigma2_YI  = sigma[1]^2;
sigma2_Y   = sigma[2]^2; 
mean_phi = inv_logit(mu_phi);

}
