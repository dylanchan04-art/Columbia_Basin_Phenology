data {
  int<lower=1> N;        // observations
  int<lower=1> Nreg;     // number of dams
  array[N] int<lower=1,upper=Nreg> r;  
  vector[N] YR;          // year
  vector[N] Y;           // median passage date
}

parameters {

  //real zeta;
  real mu_zeta;
  real<lower=0> sigma_zeta;
  
  //real mu_omega;
  //real<lower=0> sigma_omega;
  vector[Nreg] omega;    // intercepts
  real<lower=0> sigma;
  
  vector[Nreg] zeta_raw;
} 

transformed parameters {
  vector[Nreg] zeta;
  
  zeta = mu_zeta + sigma_zeta * zeta_raw;
}
model {

  //mu_omega ~ normal(120,40);
  //sigma_omega ~ normal(10,10);
  omega ~ normal(120,40);
  
  mu_zeta ~ normal(0,3);
  sigma_zeta ~ gamma(3,2);
  zeta_raw ~ normal(0,1);
  
  //zeta ~ normal(0,3);

  sigma ~ normal(20,10);
  Y ~ normal(omega[r] + zeta[r] .* YR, sigma);
  
}

