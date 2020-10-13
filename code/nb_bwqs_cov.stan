// negative binomial parameterized as eta (log(mu)) and dispersion (phi)
// a basic GLM example
data {
  int<lower=1> N;          // rows of data
  int<lower=1> N_new;      // rows of new data
  int<lower=0> C;          // social variables of the mixture
  int<lower=0> K;          // number of covariates
  matrix[N,C] XC;          // matrix of elements in the first mixture
  matrix[N,K] XK;          // matrix of predictors
  vector[C] Dalp;          // vector of Dirichlet coefficients
  int<lower=0> y[N];       // response
  matrix[N_new,C] XC_new;  // matrix of elements in the first mixture
  matrix[N_new,K] XK_new;  // matrix of predictors
}
parameters {
  real<lower=0> phi;       // neg. binomial dispersion parameter
  real beta0;              // intercept
  real beta1;              // overall mixture effect
  vector[K] delta;         // covariates coefficients
  simplex[C] W;            // weights of first mix
}
transformed parameters {
  vector[N] eta;
  eta = beta0 + beta1*(XC*W) + XK*delta;
}
model {
  // priors:
  phi ~ inv_gamma(0.01, 0.01);
  beta0 ~ normal(0, 100);
  beta1 ~ normal(0, 100);
  W ~ dirichlet(Dalp);
  for(j in 1:K) delta[j] ~ normal(0,100);
  // data model
  y ~ neg_binomial_2_log(eta, phi);
}
generated quantities {
  vector[N] log_lik;
  vector[N_new] y_new;
  for (j in 1:N){
    log_lik[j] = neg_binomial_2_log_lpmf(y[j]| eta[j], phi);
  }
  for (n in 1:N_new){
    y_new[n] = neg_binomial_2_log_rng(beta0 + beta1*(XC_new[n,]*W) + XK_new[n,]*delta, phi);
  }
}

