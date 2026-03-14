// BYM2 Fay–Herriot + wave effects. Structured ICAR only on mainland; isolated island (no
// neighbors) enters only through the iid component u[r], not through s[.].
//
// Mainland: b[r] = sigma_b * (sqrt(phi)*s_star[mainland_id[r]] + sqrt(1-phi)*u[r])
// Island:    b[r] = sigma_b * sqrt(1-phi) * u[r]   // no structured spatial term

data {
  int<lower=1> R;                      // all departments (mainland + island)
  int<lower=1> R_main;                 // mainland only (ICAR dimension)
  int<lower=1> T;
  int<lower=1> N;
  vector[N] y;
  vector<lower=0>[N] v;
  array[N] int<lower=0, upper=1> has_obs;
  array[N] int<lower=1, upper=R> area_id;
  array[N] int<lower=1, upper=T> time_id;
  array[R] int<lower=0, upper=1> is_mainland;   // 1 = mainland, 0 = isolated island
  array[R] int<lower=0, upper=R_main> mainland_id; // 1..R_main on mainland, 0 on island
  int<lower=1> E;                      // edges among mainland nodes only (indices 1..R_main)
  array[E] int<lower=1, upper=R_main> node1;
  array[E] int<lower=1, upper=R_main> node2;
  real<lower=0> scaling_factor;       // from mainland graph only
}
parameters {
  real alpha;
  vector[T - 1] beta_raw;
  vector[R_main] s;                    // ICAR field on mainland departments only
  vector[R] u;                         // iid Normal(0,1) for every department
  real<lower=0> sigma_b;
  real<lower=0, upper=1> phi;
}
transformed parameters {
  vector[T] beta;
  beta[1] = 0;
  beta[2:T] = beta_raw;

  vector[R_main] s_star;
  for (j in 1:R_main) {
    s_star[j] = s[j] / sqrt(scaling_factor);
  }

  vector[R] b;
  for (r in 1:R) {
    if (is_mainland[r] == 1) {
      int j = mainland_id[r];
      b[r] = sigma_b * (sqrt(phi) * s_star[j] + sqrt(1 - phi) * u[r]);
    } else {
      // Isolated island: BYM2 reduces to scaled iid (no s term)
      b[r] = sigma_b * sqrt(1 - phi) * u[r];
    }
  }

  vector[N] theta;
  for (n in 1:N) {
    int r = area_id[n];
    int t = time_id[n];
    theta[n] = alpha + beta[t] + b[r];
  }
}
model {
  alpha ~ normal(0, 5);
  beta_raw ~ normal(0, 2);
  u ~ normal(0, 1);
  sigma_b ~ normal(0, 1);
  phi ~ beta(0.5, 0.5);

  // ICAR prior on mainland spatial field only (connected mainland graph)
  target += -0.5 * dot_self(s[node1] - s[node2]);
  target += normal_lpdf(sum(s) | 0, 0.001 * R_main);

  for (n in 1:N) {
    if (has_obs[n] == 1) {
      y[n] ~ normal(theta[n], sqrt(v[n]));
    }
  }
}
generated quantities {
  vector[N] p;
  for (n in 1:N) {
    p[n] = inv_logit(theta[n]);
  }
  // National fixed-effect prevalence by wave (no spatial random effect b[r]):
  // wave 1 = inv_logit(alpha); wave 2 = inv_logit(alpha + beta[2]); etc.
  array[T] real p_total;
  for (t in 1:T) {
    p_total[t] = inv_logit(alpha + beta[t]);
  }
}
