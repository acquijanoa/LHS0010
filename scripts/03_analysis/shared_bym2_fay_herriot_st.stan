// Spatio-temporal shared latent BYM2 factor Fay–Herriot (bivariate per region × time).
//
// y[i,t] ~ MVN_2(theta[i,t], V[i,t])   known V[i,t]
//
// theta[i,t,1] = X1[i,t]*beta1 + alpha1[t] + b[i,t]
// theta[i,t,2] = X2[i,t]*beta2 + alpha2[t] + lambda2 * b[i,t]
//
// Temporal AR(1) on shared factor b:
//   b[i,1] = sigma_b * (sqrt(1-phi)*z[i,1] + sqrt(phi/scale_icar)*s[i,1])
//   b[i,t] = rho*b[i,t-1] + sigma_b*(sqrt(1-phi)*z[i,t] + sqrt(phi/scale_icar)*s[i,t]), t>=2
//
// Each column s[,t] is an ICAR field over regions (same adjacency each t).
// z[i,t] ~ N(0,1) iid (BYM2 unstructured part of innovations; t=1 is initial BYM2 state).

data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1> p1;
  int<lower=1> p2;
  array[N, T] vector[2] y;
  array[N, T] cov_matrix[2] V;
  array[N, T] row_vector[p1] X1;
  array[N, T] row_vector[p2] X2;
  int<lower=1> N_edges;
  array[N_edges] int<lower=1, upper=N> node1;
  array[N_edges] int<lower=1, upper=N> node2;
  real<lower=0> scale_icar;
}

parameters {
  vector[p1] beta1;
  vector[p2] beta2;
  vector[T] alpha1;
  vector[T] alpha2;
  real lambda2;
  real<lower=0> sigma_b;
  real<lower=0, upper=1> phi;
  real<lower=-1, upper=1> rho;
  matrix[N, T] z;   // iid N(0,1) — unstructured BYM2 component per (i,t)
  matrix[N, T] s;   // ICAR spatial component per time slice t
}

transformed parameters {
  matrix[N, T] b;
  for (i in 1:N) {
    b[i, 1] = sigma_b * (sqrt(1.0 - phi) * z[i, 1]
                         + sqrt(phi / scale_icar) * s[i, 1]);
    for (t in 2:T) {
      b[i, t] = rho * b[i, t - 1]
                + sigma_b * (sqrt(1.0 - phi) * z[i, t]
                             + sqrt(phi / scale_icar) * s[i, t]);
    }
  }
}

model {
  beta1 ~ normal(0, 2.5);
  beta2 ~ normal(0, 2.5);
  alpha1 ~ normal(0, 1);
  alpha2 ~ normal(0, 1);
  lambda2 ~ normal(0, 1);
  sigma_b ~ normal(0, 1) T[0, ];
  phi ~ beta(0.5, 0.5);
  rho ~ normal(0, 0.5);

  to_vector(z) ~ std_normal();

  // ICAR + soft sum-to-zero for each time slice s[,t]
  for (t in 1:T) {
    for (e in 1:N_edges) {
      target += -0.5 * square(s[node1[e], t] - s[node2[e], t]);
    }
    {
      real sum_st = 0;
      for (i in 1:N) sum_st += s[i, t];
      sum_st ~ normal(0, 0.001 * N);
    }
  }

  for (i in 1:N) {
    for (t in 1:T) {
      vector[2] theta_it;
      theta_it[1] = dot_product(X1[i, t], beta1) + alpha1[t] + b[i, t];
      theta_it[2] = dot_product(X2[i, t], beta2) + alpha2[t] + lambda2 * b[i, t];
      y[i, t] ~ multi_normal(theta_it, V[i, t]);
    }
  }
}

generated quantities {
  array[N, T] vector[2] theta;
  array[N, T] vector[2] p_hat;
  array[N, T] real log_lik;
  for (i in 1:N) {
    for (t in 1:T) {
      theta[i, t][1] = dot_product(X1[i, t], beta1) + alpha1[t] + b[i, t];
      theta[i, t][2] = dot_product(X2[i, t], beta2) + alpha2[t] + lambda2 * b[i, t];
      p_hat[i, t][1] = inv_logit(theta[i, t][1]);
      p_hat[i, t][2] = inv_logit(theta[i, t][2]);
      log_lik[i, t] = multi_normal_lpdf(y[i, t] | theta[i, t], V[i, t]);
    }
  }
}
