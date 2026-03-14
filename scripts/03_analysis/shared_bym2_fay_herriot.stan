// Shared latent BYM2 factor Fay–Herriot model (bivariate normal sampling model).
//
// For each region i:
//   y_i ~ MVN_2(theta_i, V_i)     with known 2x2 sampling covariance V_i
//
// Linking model (lambda1 = 1 for identifiability):
//   theta_{i1} = X1[i] * beta1 + b_i
//   theta_{i2} = X2[i] * beta2 + lambda2 * b_i
//
// Shared regional effect (BYM2 mix on b_i):
//   b_i = sigma_b * ( sqrt(1-phi) * z_i + sqrt(phi / scale_icar) * s_i )
//   z_i ~ N(0,1) iid
//   s ~ ICAR (pairwise differences) + soft sum-to-zero
//
// scale_icar: computed in R from the adjacency graph (fixed data); do not estimate.
//
data {
  int<lower=1> N;
  int<lower=1> p1;
  int<lower=1> p2;
  matrix[N, p1] X1;
  matrix[N, p2] X2;
  array[N] vector[2] y;
  // Known sampling covariance per region (symmetric SPD; jitter in R if needed)
  array[N] cov_matrix[2] V;
  int<lower=1> N_edges;
  array[N_edges] int<lower=1, upper=N> node1;
  array[N_edges] int<lower=1, upper=N> node2;
  real<lower=0> scale_icar;
}

parameters {
  vector[p1] beta1;
  vector[p2] beta2;
  real lambda2;
  real<lower=0> sigma_b;
  real<lower=0, upper=1> phi;
  // Non-centered iid standard normals for unstructured component
  vector[N] z_std;
  // ICAR spatial component (soft sum-to-zero in model block)
  vector[N] s;
}

transformed parameters {
  // BYM2 combined regional effect b
  vector[N] b;
  for (i in 1:N) {
    b[i] = sigma_b * (sqrt(1.0 - phi) * z_std[i]
                      + sqrt(phi / scale_icar) * s[i]);
  }
  // Mean vectors theta_i on logit scale
  array[N] vector[2] theta;
  for (i in 1:N) {
    theta[i][1] = dot_product(X1[i], beta1) + b[i];
    theta[i][2] = dot_product(X2[i], beta2) + lambda2 * b[i];
  }
}

model {
  // Weakly informative regression and mixing
  beta1 ~ normal(0, 2.5);
  beta2 ~ normal(0, 2.5);
  lambda2 ~ normal(0, 1);
  sigma_b ~ normal(0, 1) T[0, ];   // half-Normal(0,1) on sigma_b
  phi ~ beta(0.5, 0.5);

  z_std ~ std_normal();

  // ICAR: penalty for each undirected edge (same as -0.5 * sum_e (s_i - s_j)^2)
  for (e in 1:N_edges) {
    target += -0.5 * square(s[node1[e]] - s[node2[e]]);
  }
  // Soft sum-to-zero (Morris / Stan ICAR examples)
  sum(s) ~ normal(0, 0.001 * N);

  // Sampling model: bivariate normal with known V_i
  for (i in 1:N) {
    y[i] ~ multi_normal(theta[i], V[i]);
  }
}

generated quantities {
  // Fitted probabilities
  matrix[N, 2] p_hat;
  vector[N] log_lik;
  for (i in 1:N) {
    p_hat[i, 1] = inv_logit(theta[i][1]);
    p_hat[i, 2] = inv_logit(theta[i][2]);
    log_lik[i] = multi_normal_lpdf(y[i] | theta[i], V[i]);
  }
}
