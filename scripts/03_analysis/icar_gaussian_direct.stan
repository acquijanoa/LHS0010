data {
  int<lower=1> R;
  vector[R] y;                 
  vector<lower=0>[R] v;        
  int<lower=1> E;
  array[E] int<lower=1, upper=R> node1;
  array[E] int<lower=1, upper=R> node2;
  int<lower=0, upper=1> estimate_kappa;
  real<lower=0> scaling_factor; // Pre-calculated scaling for the ICAR component
}
parameters {
  real alpha;
  real<lower=0, upper=1> rho; // Proportion of variance that is spatial
  real<lower=0> sigma;        // Total standard deviation of random effects
  vector[R - 1] u_raw;        // Spatial component
  vector[R] epsilon;          // IID component
  real<lower=0> kappa;
}
transformed parameters {
  vector[R] u;
  u[1:(R - 1)] = u_raw;
  u[R] = -sum(u_raw);
  
  // BYM2 Convolution: Combine spatial and iid components
  // The spatial part is scaled by the scaling_factor and rho
  vector[R] combined_error = (sqrt(rho / scaling_factor) * u + 
                              sqrt(1 - rho) * epsilon) * sigma;

  vector[R] vv;
  if (estimate_kappa == 1) {
    vv = kappa * v;
  } else {
    vv = v;
  }
}
model {
  alpha ~ normal(0, 5);
  u_raw ~ normal(0, 1);
  epsilon ~ normal(0, 1);
  sigma ~ exponential(1);
  rho ~ beta(0.5, 0.5); // Prior favoring either high or low spatial correlation
  
  if (estimate_kappa == 1) kappa ~ lognormal(0, 0.5);

  // ICAR Prior
  target += -0.5 * dot_self(u[node1] - u[node2]);

  // Likelihood
  y ~ normal(alpha + combined_error, sqrt(vv));
}
generated quantities {
  vector[R] theta = alpha + combined_error;
  vector[R] OR = exp(theta);
  real logit_rho = log(rho / (1 - rho));
}

