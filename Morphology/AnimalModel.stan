data {
  int<lower=1>    n_ind; // number of all animals
  int<lower=1>    n_obs; // number of observations
  int             n_yi;
  int             n_y;
  int             n_isl;
  int             n_delta;
  
  int             t[n_delta];
  int             t_plus1[n_delta];
  int             mnIY;
  int             mnI;
  int             mnY;
  int             s[n_obs];
  
  int             individual[n_obs]; // Random effects design matrix
  vector[n_obs]   m; // response variable
  matrix[n_ind,n_ind]     A; // relationship matrix
  int             island_year[n_obs];
  int             nIY[n_yi];
  int             m_IY[mnIY, n_yi];

  int             nI[n_isl];
  int             m_I[mnI, n_isl];
  
  int             nY[n_y];
  int             m_Y[mnY, n_y];
  int             n_ind_isl_year[n_yi];
}

transformed data{
  matrix[n_ind,n_ind] LA;
  real mu_m;
  real sd_m;
  vector[n_obs]   z;
  
  LA = cholesky_decompose(A);
  mu_m = mean(m);
  sd_m = sd(m);
  z = (m-mu_m)/sd_m;
}

parameters {
  real mu1;
  real B_sex;
  vector[n_ind]  a_decompose; // breeding values
  vector[n_ind] zI;
  vector[n_yi] zIY;

  real<lower=0> sigma_G; // genetic standard deviation
  real<lower=0> sigma_R; 
  real<lower=0> sigma_I;
  real<lower=0> sigma_IY;// residual standard deviation
}

transformed parameters{
  vector[n_ind] a;
  vector[n_ind] I;
   vector[n_yi] IY;
  vector[n_obs] v;

  a = sigma_G * (LA * a_decompose);
  I = sigma_I * zI;
  IY = sigma_IY*zIY;
 
 for(i in 1:n_obs)
  v[i] =  mu1 + B_sex*s[i] + a[individual[i]] + I[individual[i]] + IY[island_year[i]];
  
}

model {
  
a_decompose ~ normal(0, 1);
zI~ normal(0, 1);
zIY~ normal(0, 1);
sigma_G ~ normal(0.2, 1);
sigma_R ~ normal(0.6,  0.5);
sigma_I ~ normal(0.6,  0.5);
sigma_IY ~ normal(0.3, 0.5);
mu1~ normal(0, 0.1);

z~normal(v, sigma_R);
  
}

generated quantities{
  real sigma2_G_z;
  real sigma2_R_z;
  real sigma2_I_z;
  real sigma2_IY_z;
  real sigma2_G;
  real sigma2_R;
  real sigma2_I;
  real sigma2_P;
  
  real sigma2_IY;
  real sigma2_AIY;
  real sigma2_AY;
  real sigma2_AI;
  real p_sigma2_AY;
  real p_sigma2_AI;
  real h2;
  real cv;
  real drift_g;
  real drift_all;
  
  real p_drift;
  
  vector[n_yi]      meanAIY;
  vector[n_yi]       varAIY;
  vector[n_isl]      meanAI;
  vector[n_isl]       varAI;
  vector[n_y]      meanAY;
  vector[n_y]       varAY;
  vector[n_delta]       delta;
  real<lower=0> dem_v_eff_isl_year_g[n_yi];
  real<lower=0> dem_v_eff_isl_year_all[n_yi];
  
  sigma2_G_z = sigma_G * sigma_G; // genetic variance
  sigma2_R_z = sigma_R * sigma_R; // residual variance
  sigma2_I_z = sigma_I * sigma_I;
  sigma2_IY_z = sigma_IY * sigma_IY;

  sigma2_P = sd_m^2;
  sigma2_G = sigma2_G_z* sigma2_P; // genetic variance
  sigma2_R = sigma2_R_z * sigma2_P; // residual variance
  sigma2_I = sigma2_I_z * sigma2_P;
  sigma2_IY = sigma2_IY_z * sigma2_P;

  h2=sigma2_G/(sigma2_R + sigma2_G + sigma2_I + sigma2_IY);
  cv = 100*(sigma2_G*sigma2_P)/(mu1+mu_m)^2;

for(i in 1:n_yi)
meanAIY[i] = sum(a[m_IY[1:nIY[i],i]])/nIY[i];
 
for(i in 1:n_yi)
varAIY[i] = sum((a[m_IY[1:nIY[i],i]]-meanAIY[i])^2)/nIY[i];

for(i in 1:n_isl)
meanAI[i] = sum(a[m_I[1:nI[i],i]])/nI[i];
 
for(i in 1:n_isl)
varAI[i] = sum((a[m_I[1:nI[i],i]]-meanAI[i])^2)/nI[i];

for(i in 1:n_y)
meanAY[i] = sum(a[m_Y[1:nY[i],i]])/nY[i];
 
for(i in 1:n_y)
varAY[i] = sum((a[m_Y[1:nY[i],i]]-meanAY[i])^2)/nY[i];

for(i in 1:n_delta)
delta[i] = meanAIY[t_plus1[i]]-meanAIY[t[i]];

sigma2_AIY = sum((meanAIY-mu1)^2)/(n_yi-1);
sigma2_AI = sum((meanAI-mu1)^2)/(n_isl-1);
sigma2_AY = sum((meanAY-mu1)^2)/(n_y-1);

p_sigma2_AI = sigma2_AI/sigma2_AIY;
p_sigma2_AY = sigma2_AY/sigma2_AIY;

for(i in 1:n_yi){
dem_v_eff_isl_year_g[i] = sigma2_G/n_ind_isl_year[i];
dem_v_eff_isl_year_all[i] = (sigma2_G + sigma2_I + sigma2_R)/n_ind_isl_year[i];
}
drift_g = sum(dem_v_eff_isl_year_g)/n_yi*sigma2_P;
drift_all = sum(dem_v_eff_isl_year_all)/n_y;
p_drift=drift_g/(sigma2_IY + drift_all);

}
