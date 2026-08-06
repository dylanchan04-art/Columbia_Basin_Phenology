data {
  int<lower=1> N1;      // Number of observations
  int<lower=1> Nreg;   // Number of dams
  int<lower=1> Nyr;    // Number of years
  int<lower=1> Nsp;    // Number of species
  int<lower=1> Ns_d;   // Number of unique species, dam combinations
  
  array[N1] int<lower=1,upper=Nreg> r;  // Dam index
  array[N1] int<lower=1,upper=Nyr> t;   // Year index
  array[N1] int<lower=1, upper=Nsp> ii; //Species index
  array[N1] int<lower=1,upper=Ns_d> s_d; //unique species, dam combination index
  
  vector[N1] TT; //Temp        
  vector[N1] SS1; // Salmon passage
  
}

parameters {

  vector[Nsp] alpha1; 
  vector[Nsp] beta; 
  vector<lower=0>[Nsp] sigma_1;
  
  real mu_beta;
  real <lower=0>sigma_beta;
  
  real mu_alpha1;
  real <lower=0>sigma_alpha1;
  
  vector[Nreg] alpha2; 
  //real mu_alpha2;
  real <lower=0>sigma_alpha2;
  
}

model {
  
  mu_beta ~ normal(0,5); 
  sigma_beta ~ gamma(4,2); 
  
  mu_alpha1 ~ normal(130,30); 
  sigma_alpha1 ~ normal(10,5);
  
  beta ~ normal(mu_beta,sigma_beta); 
  alpha1 ~ normal(mu_alpha1,sigma_alpha1); 
  
  sigma_alpha2 ~ gamma(3,1);
  
  alpha2 ~ normal(0,sigma_alpha2);
  
  sigma_1 ~ gamma(3,1);

  
  SS1 ~ normal(alpha1[ii] + alpha2[r] + beta[ii] .* TT, sigma_1[ii]);
}

 

