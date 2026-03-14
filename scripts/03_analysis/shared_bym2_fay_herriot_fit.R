#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Shared latent BYM2 factor Fay–Herriot model: data prep, scale_icar, cmdstanr fit,
# posterior summaries for theta and prevalence.
#
# Depends: cmdstanr, Matrix, spdep, sf (if building nb from polygons), posterior
#
# Example inputs:
#   y  <- N x 2 matrix (cols = outcome 1 & 2 logit direct estimates), rows = regions
#   V  <- array dim c(2, 2, N) or list of 2x2 matrices — sampling cov per region
#   nb <- spdep neighbor object (same region order as rows of y)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(Matrix)
  library(cmdstanr)
})

# =============================================================================
# 1) ICAR scaling constant scale_icar (Riebler / BYM2 interpretability)
# =============================================================================
# Average marginal variance of the intrinsic GMRF with precision Q = D - W
# when using a small ridge for numerical stability. Pass this into Stan as
# scale_icar so sqrt(phi / scale_icar) * s has comparable scale to the
# unstructured part. See Morris et al. (2019) and Stan ICAR case studies.

#' @param nb spdep::nb object (symmetric neighbors, one row per region)
#' @param ridge Small positive diagonal jitter for Q (default 1e-6)
#' @return Named list: scale_icar, Q (sparse), optional diag of Q^{-1} approx
scale_icar_from_nb <- function(nb, ridge = 1e-6) {
  stopifnot(inherits(nb, "nb"))
  W <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)
  n <- nrow(W)
  if (n < 2) stop("Need N >= 2 regions for ICAR scaling.")
  Q <- diag(rowSums(W)) - W
  Qr <- Q + diag(ridge, n)
  # Average marginal variance under ridge-regularized precision (common BYM2 choice)
  Qi <- tryCatch(solve(Qr), error = function(e) stop("Q ridge solve failed: ", e$message))
  scale_icar <- mean(diag(Qi))
  if (!is.finite(scale_icar) || scale_icar <= 0) {
    stop("scale_icar not finite/positive; check connectivity of nb.")
  }
  list(scale_icar = as.numeric(scale_icar), n = n, W = W, Q = Q)
}

# Alternative: geometric mean of marginal variances (used elsewhere in this repo)
scale_icar_geom_from_nb <- function(nb, ridge = 1e-6) {
  W <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)
  n <- nrow(W)
  Q <- diag(rowSums(W)) - W
  Qr <- Q + diag(ridge, n)
  d <- diag(solve(Qr))
  list(scale_icar = as.numeric(exp(mean(log(d)))), n = n)
}

# =============================================================================
# 2) Edge list for Stan (undirected edges once)
# =============================================================================

nb_to_stan_edges <- function(nb) {
  n <- length(nb)
  ed <- list()
  for (i in seq_len(n)) {
    js <- nb[[i]]
    if (length(js)) {
      for (j in js) {
        if (!is.na(j) && j > i) ed[[length(ed) + 1]] <- c(i, as.integer(j))
      }
    }
  }
  if (!length(ed)) stop("No edges in nb; ICAR needs at least one edge.")
  m <- do.call(rbind, ed)
  list(
    N_edges = nrow(m),
    node1 = as.integer(m[, 1]),
    node2 = as.integer(m[, 2])
  )
}

# =============================================================================
# 3) Ensure each V_i is SPD (tiny jitter on diagonal in R only)
# =============================================================================

jitter_V_array <- function(V_list, eps = 1e-8) {
  # V_list: length N, each 2x2 symmetric
  lapply(V_list, function(M) {
    M <- as.matrix(M)
    M + diag(eps, 2)
  })
}

as_stan_V <- function(V_list) {
  # cmdstanr expects list of 2x2 matrices for array[N] cov_matrix[2] V
  lapply(V_list, function(M) {
    M <- as.matrix(M)
    stopifnot(all(dim(M) == c(2L, 2L)))
    # Symmetrize
    (M + t(M)) / 2
  })
}

# =============================================================================
# 4) Build full Stan data list
# =============================================================================

#' @param y matrix N x 2 (outcome 1, outcome 2) direct logit estimates
#' @param V array(2,2,N) or list of N matrices — sampling covariance per row of y
#' @param X1 matrix N x p1 (include intercept column if desired)
#' @param X2 matrix N x p2
#' @param node1, node2, N_edges from nb_to_stan_edges
#' @param scale_icar from scale_icar_from_nb()
build_stan_data_shared_bym2 <- function(y, V, X1, X2, node1, node2, N_edges, scale_icar,
                                        V_jitter = 1e-8) {
  y <- as.matrix(y)
  stopifnot(ncol(y) == 2L, nrow(y) >= 2L)
  N <- nrow(y)
  if (is.array(V) && length(dim(V)) == 3L && dim(V)[1] == 2 && dim(V)[2] == 2) {
    V_list <- lapply(seq_len(N), function(i) V[, , i])
  } else if (is.list(V) && length(V) == N) {
    V_list <- V
  } else {
    stop("V must be 2x2xN array or length-N list of 2x2 matrices.")
  }
  V_list <- jitter_V_array(as_stan_V(V_list), eps = V_jitter)
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  stopifnot(nrow(X1) == N, nrow(X2) == N)

  y_list <- lapply(seq_len(N), function(i) as.numeric(y[i, ]))

  list(
    N = N,
    p1 = ncol(X1),
    p2 = ncol(X2),
    X1 = X1,
    X2 = X2,
    y = y_list,
    V = V_list,
    N_edges = as.integer(N_edges),
    node1 = as.integer(node1),
    node2 = as.integer(node2),
    scale_icar = as.numeric(scale_icar)
  )
}

# =============================================================================
# 5) Fit with cmdstanr + extract summaries
# =============================================================================

fit_shared_bym2 <- function(stan_data, stan_file, chains = 4, parallel_chains = 4,
                            iter_warmup = 1000, iter_sampling = 1000, refresh = 200,
                            seed = 123, force_recompile = FALSE) {
  mod <- cmdstan_model(stan_file, force_recompile = force_recompile)
  mod$sample(
    data = stan_data,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = refresh,
    seed = seed,
    max_treedepth = 12,
    adapt_delta = 0.95
  )
}

#' Posterior summaries for theta (logit) and p_hat (probability)
summarize_shared_bym2 <- function(fit, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop("Install package posterior for summarize_draws.")
  }
  dr <- fit$draws(variables = c("theta", "p_hat", "b", "lambda2", "sigma_b", "phi"),
                  format = "draws_array")
  summ <- posterior::summarise_draws(dr, ~ posterior::quantile2(.x, probs = probs))
  # Flatten theta / p_hat by region and outcome
  cn <- colnames(dr)
  theta_idx <- grep("^theta\\[", cn)
  # theta[i,1] and theta[i,2] in cmdstanr are variables theta[i,1] etc.
  list(full_summary = summ, draws = dr)
}

#' Region-level table: median and 95% CI for logit theta and prevalence p_hat
region_posterior_table <- function(fit, N = NULL, region_names = NULL) {
  arr <- fit$draws(variables = c("theta", "p_hat"), format = "draws_matrix")
  cn <- colnames(arr)
  if (is.null(N)) {
    th <- cn[grepl("^theta\\[", cn)]
    N <- max(as.integer(sub("^theta\\[(\\d+),.*", "\\1", th)), na.rm = TRUE)
  }
  if (is.null(region_names)) region_names <- paste0("region_", seq_len(N))

  parse_theta <- function(j, k) {
    v <- paste0("theta[", j, ",", k, "]")
    if (!v %in% cn) stop("Missing column ", v)
    x <- arr[, v]
    c(median = stats::median(x), q025 = stats::quantile(x, 0.025),
      q975 = stats::quantile(x, 0.975))
  }
  parse_p <- function(j, k) {
    v <- paste0("p_hat[", j, ",", k, "]")
    x <- arr[, v]
    c(median = stats::median(x), q025 = stats::quantile(x, 0.025),
      q975 = stats::quantile(x, 0.975))
  }
  rows <- vector("list", N)
  for (i in seq_len(N)) {
    t1 <- parse_theta(i, 1)
    t2 <- parse_theta(i, 2)
    p1 <- parse_p(i, 1)
    p2 <- parse_p(i, 2)
    rows[[i]] <- data.frame(
      region = region_names[i],
      theta1_median = unname(t1[1]), theta1_lo = unname(t1[2]), theta1_hi = unname(t1[3]),
      theta2_median = unname(t2[1]), theta2_lo = unname(t2[2]), theta2_hi = unname(t2[3]),
      p1_median = unname(p1[1]), p1_lo = unname(p1[2]), p1_hi = unname(p1[3]),
      p2_median = unname(p2[1]), p2_lo = unname(p2[2]), p2_hi = unname(p2[3]),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

