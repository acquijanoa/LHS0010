// BYM2 + ICAR on logit scale. Tunable smoothing:
//   trust_direct  >1  → stronger likelihood anchor to survey direct estimates (less pooling).
//   icar_strength <1  → weaker neighbor coupling (retain more regional contrast).
//   rho prior skewed low → more IID noise, less spatial structure.
data {
  int<lower=1> R;
  vector[R] y;
  array[R] int<lower=0, upper=1> has_se;
  vector<lower=0>[R] v;
  int<lower=1> E;
  array[E] int<lower=1, upper=R> node1;
  array[E] int<lower=1, upper=R> node2;
  real<lower=0> scaling_factor;
  real<lower=0.7, upper=2.5> trust_direct;   // e.g. 1.2–1.5: divide obs SD → trust data more
  real<lower=0.15, upper=1> icar_strength;    // e.g. 0.35: multiply ICAR penalty (lower = smoother u off)
}
parameters {
  real alpha;
  real<lower=0, upper=1> rho;
  real<lower=0> sigma;
  vector[R - 1] u_raw;
  vector[R] epsilon;
  real<lower=0> sigma_miss;
}
transformed parameters {
  vector[R] u;
  u[1:(R - 1)] = u_raw;
  u[R] = -sum(u_raw);
  vector[R] combined = (sqrt(rho / scaling_factor) * u + sqrt(1 - rho) * epsilon) * sigma;
  vector[R] sd_y;
  for (i in 1:R) {
    if (has_se[i] == 1)
      sd_y[i] = sqrt(fmax(v[i], 1e-8)) / trust_direct;
    else
      sd_y[i] = sigma_miss;
  }
}
model {
  alpha ~ normal(0, 5);
  u_raw ~ normal(0, 1);
  epsilon ~ normal(0, 1);
  sigma ~ exponential(0.6);              // slightly more spread on IID/spatial scale
  rho ~ beta(2, 12);                      // favors low rho → epsilon dominates → regional noise
  sigma_miss ~ exponential(0.5);
  target += -0.5 * icar_strength * dot_self(u[node1] - u[node2]);
  y ~ normal(alpha + combined, sd_y);
}
generated quantities {
  vector[R] theta = alpha + combined;
  vector[R] p_smoothed = inv_logit(theta);
}
