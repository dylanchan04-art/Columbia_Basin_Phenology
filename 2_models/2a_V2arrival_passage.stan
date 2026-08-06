data {
  //Bird level
  int<lower=1> N2;      // Number of observations
  int<lower=1> Nreg;   // Number of dams

  array[N2] int<lower=1,upper=Nreg> r;  // Dam index
  
  vector[N2] SS2; // Salmon passage - when bird arrival data is present
  vector[N2] BB; // Bird arrival
  vector[N2] BB_unc; // Bird arrival uncertainty
  
}

parameters {
  
  
  //for arrival model
  vector[N2] BB_true;
  real eta;  
  
  vector[Nreg] omega;
  real<lower=0> sigma_2;
  

  
}

model {
  
  omega ~ normal(100,40);  
  eta ~ normal(0, 10);
  sigma_2 ~ normal(20,10);
  

  BB ~ normal(BB_true,BB_unc);
  BB_true ~ normal(omega[r] + eta * SS2, sigma_2);
}


