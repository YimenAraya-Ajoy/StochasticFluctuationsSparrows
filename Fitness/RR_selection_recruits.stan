data {
    // Number of clusters both models
    int<lower=0> n_ind; //number of individuals
    int<lower=0> n_yi;
    int<lower=0> n_y;//length of year island vector
    int<lower=0> n_isl; //length of year island vector
    // Number of clusters morphology model
    int<lower=0> n_obs_m; 
    int<lower=0> n_m; 
     int<lower=0> n_observ;
    // Clusters indentifiers morphology model
    int<lower=0> individual_m[n_obs_m]; 
    int<lower=0> year_isl_m[n_obs_m];
    int<lower=0> month[n_obs_m]; // year_island identities (names)
    int<lower=0> observer[n_obs_m]; // year_island identities (names)
    
    // Continuous predictors morphology model
    real sex_m[n_obs_m];
    // response variable
    real z[n_obs_m];  // fitness (recruits)
    // Number of clusters fitness model
    int<lower=0> n_obs_f; 
   // Clusters indentifiers fitness model
    int<lower=0> individual_f[n_obs_f]; //  Individual identites.
    int<lower=0> year_isl_f[n_obs_f];
    int<lower=0> year_f[n_obs_f];
    int<lower=0> island_f[n_obs_f];
    // Continuous predictors fitness model
    real<lower=-1, upper = 1> age[n_obs_f]; // age (experienced breeder)
    real sex_f[n_obs_f];
    int<lower=1, upper = 2> sex2[n_obs_m];// sex
    real n[n_obs_f];
     // response variable
    int<lower=0> r[n_obs_f]; 
    }
  
  parameters {
// Define parameters to estimate
    vector[2]              B_m;
    vector[7]              B_f;

// Random effects. Do this for all random effect levels (Location (int and slope), Year_Location (intercept))
    matrix[2,n_ind]          zI; //matrix scaled individual blups (intercepts)
    matrix[2,n_yi]           zYI;
    matrix[2,n_y]           zY;//vector scaled year island blups(intercepts)
    matrix[2,n_isl]          zIsl; //vector scaled year island blups(intercepts)
   
    vector[n_observ]         zO; //vector scaled year island blups(intercepts)
    matrix[2, n_m]           zM;
    vector<lower=0>[2]       sigma_I;
    vector<lower=0>[2]       sigma_Isl;
    
    vector<lower=0>[2]     sigma_YI;
    vector<lower=0>[2]     sigma_Y;
    real<lower=0>          sigma_O;
    vector<lower=0>[2]      sigma_M;
    // sd island intercepts and slopes
    cholesky_factor_corr[2] LYI;
    cholesky_factor_corr[2] LIsl;
   cholesky_factor_corr[2] LY;

    real<lower=0> phi;
    real<lower=0> sigma_R;// factor to estimate covariance int-slopes. To estimate covariance. Here we specify the off-diagonal number of cells in the variance covariance matrix. We have two random effect levels, so there are two.
  }
  
  transformed parameters{
    matrix[2,n_ind]           I; //matrix scaled individual blups (intercepts)
    matrix[2,n_yi]           YI; //vector scaled year island blups(intercepts)
    matrix[2,n_isl]         Isl; //vector scaled year island blups(intercepts)
    matrix[2,n_y]            Y; //vector scaled year island blups(intercepts)
    
    vector[n_observ]         O; //vector scaled year island blups(intercepts)
    matrix[2, n_m]           M;
    
    vector[n_obs_m]          mu_z;
    vector[n_obs_f]          mu_v;// predicted value for log fitness. Could be below zero
    
    I[1,]  =  zI[1,] * sigma_I[1]; // ind intercept
    I[2,]  =  zI[2,] * sigma_I[2]; 
    O      =  zO * sigma_O;
    M[1,]=   zM[1,] * sigma_M[1];
    M[2,]=   zM[2,] * sigma_M[2];
   
    Y   = diag_pre_multiply(sigma_Y, LY) * zY;
    YI  = diag_pre_multiply(sigma_YI, LYI) * zYI;
    Isl = diag_pre_multiply(sigma_Isl, LIsl) * zIsl;// equation for random Location intercept and slope
    
   for (i in 1:n_obs_m){
     mu_z[i]  = B_m[1]  +  I[1,individual_m[i]]  + M[sex2[i],month[i]] + O[observer[i]] + B_m[2]*sex_m[i]; //+ B_age*age[i]; //add Location slope intercept matrix and add Year_Location intercept vector
                       }
   
   for (i in 1:n_obs_f){
    mu_v[i]  = B_f[1]  +  I[2,individual_f[i]] + Isl[1,island_f[i]] + YI[1,year_isl_f[i]] +  + Y[1,year_f[i]]  +(B_f[2] 
    + YI[2,year_isl_f[i]] + Isl[2,island_f[i]] + Y[2,year_f[i]])*I[1,individual_f[i]] + B_f[3]*I[1,individual_f[i]]^2 
    + B_f[4]*sex_f[i] + B_f[5]*age[i] + B_f[6]*n[i] + B_f[7]*sex_f[i]*age[i];
                        }
                        }
 
model {
        to_vector(B_m) ~ normal(0, 1); 
        to_vector(B_f) ~ normal(0, 1); 
        to_vector(sigma_M)  ~ normal(0, 1);
        sigma_YI[1] ~ normal(0.28, 0.5); 
        sigma_YI[2] ~ normal(0, 1); 
        
        sigma_I[1] ~ normal(1, 0.5);
        sigma_I[2] ~ normal(0.27, 0.5);
        
        sigma_Isl[1] ~ normal(0, 1); 
        sigma_Isl[2] ~ normal(0, 1); 

        sigma_Y[1] ~ normal(0, 1); 
        sigma_Y[2] ~ normal(0, 1); 
         
        sigma_O ~ normal(0.16, 0.5); 
        sigma_R  ~ normal(0, 1);
        
        to_vector(zI)        ~ normal(0, 1); 
        to_vector(zYI)       ~ normal(0, 1);
        to_vector(zY)       ~ normal(0, 1);
        to_vector(zM)        ~ normal(0, 1);
        to_vector(zO)        ~ normal(0, 1);
        to_vector(zIsl)      ~ normal(0, 1);
    
        z~normal(mu_z, sigma_R); 
    
        phi ~ gamma(0.01,0.01);
        LYI ~ lkj_corr_cholesky(2); 
        LIsl~ lkj_corr_cholesky(2); 
        
        r~neg_binomial_2(exp(mu_v), phi); 
}

generated quantities{
vector[2] sigma2_YI; 
vector[2] sigma2_Y; 
vector[2] sigma2_Isl; 

real CV;
real cov_IY; // the covariance between r0 and gamma, islands
real cov_Isl; // the covariance between r0 and gamma, islands
real cov_Y; // the covariance between r0 and gamma, islands

matrix[2, 2]  Omega_YI;
matrix[2, 2]  Omega_Isl;
matrix[2, 2]  Omega_Y;

row_vector[n_yi] sel; //island r0s. Must be row_vector because we specified Isl as a matrix with BLUP's on the rows.

sigma2_YI =sigma_YI^2;
sigma2_Y =sigma_Y^2;
sigma2_Isl =sigma_Isl^2;

CV= sigma_YI[2]/B_f[2];

Omega_YI = LYI * LYI'; 
Omega_Y = LY * LY'; 
Omega_Isl = LIsl * LIsl'; 

cov_IY = Omega_YI[1,2]*sqrt(sigma2_YI[1]*sigma2_YI[2]);
cov_Y = Omega_Y[1,2]*sqrt(sigma2_Y[1]*sigma2_Y[2]);
cov_Isl = Omega_Isl[1,2]*sqrt(sigma2_Isl[1]*sigma2_Isl[2]);

sel = B_f[2]+YI[2,];
}


