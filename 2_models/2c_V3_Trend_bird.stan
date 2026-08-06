data {
  //Bird level
  int<lower=1> N2;      // Number of observations
  int<lower=1> Nreg;   // Number of dams

  array[N2] int<lower=1,upper=Nreg> r;  // Dam index
  vector[N2] YR; // Year
  vector[N2] BB; // Bird arrival
  vector[N2] BB_unc; // Bird arrival uncertainty
  
}

parameters {
  
  vector[N2] BB_true;
  
  //real zeta;
  real mu_zeta;
  real<lower=0> sigma_zeta;
  
  //vector[Nreg] zeta;
  
  //real mu_omega;
  //real<lower=0> sigma_omega;
  
  vector[Nreg] omega;
  
  real<lower=0> sigma;
  
  vector[Nreg] zeta_raw;
} 

transformed parameters {
  vector[Nreg] zeta;
  
  zeta = mu_zeta + sigma_zeta * zeta_raw;
}

model {
  //mu_omega ~ normal(120,40);
  //sigma_omega ~ lognormal(2,1);
  omega ~ normal(120,40);
  
  //omega ~ normal(mu_omega,sigma_omega);  
  mu_zeta ~ normal(0,3);
  sigma_zeta ~ gamma(3,2);
  zeta_raw ~ normal(0,1);
  //zeta ~ normal(mu_zeta,sigma_zeta);

  sigma ~ normal(20,10);

  BB ~ normal(BB_true,BB_unc);
  BB_true ~ normal(omega[r] + zeta[r] .* YR, sigma);
}


