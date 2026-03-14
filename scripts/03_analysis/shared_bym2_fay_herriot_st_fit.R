# Spatio-temporal shared BYM2 Fay–Herriot: Stan data list, cmdstanr fit, summaries.
# Source after shared_bym2_fay_herriot_fit.R (scale_icar_from_nb, nb_to_stan_edges, as_stan_V).

user_lib <- "/nas/longleaf/home/aquijano/R/x86_64-pc-linux-gnu-library/4.5"
.libPaths(c(user_lib, .libPaths()))

# install sf package
install.packages("sf", lib = user_lib, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(cmdstanr)
  library(dplyr)
})

#' Build cmdstanr data for shared_bym2_fay_herriot_st.stan
#'
#' @param y array dim c(N, T, 2) or list: y[[i]][[t]] = c(outcome1, outcome2) on logit scale
#' @param V list length N, each V[[i]] length T of 2x2 matrices (sampling cov for region i, time t)
#' @param X1 array N x T x p1 or NULL (intercept only)
#' @param X2 array N x T x p2 or NULL
#' @param time_labels optional character length T (for summaries only)
build_stan_data_st <- function(
    y, V, N, T, p1, p2, node1, node2, N_edges, scale_icar,
    X1 = NULL, X2 = NULL, V_jitter = 1e-8) {
  stopifnot(N >= 2L, T >= 1L, p1 >= 1L, p2 >= 1L)

  if (is.array(y) && length(dim(y)) == 3L && dim(y)[3] == 2L) {
    y_stan <- vector("list", N)
    for (i in seq_len(N)) {
      y_stan[[i]] <- lapply(seq_len(T), function(t) as.numeric(y[i, t, ]))
    }
  } else if (is.list(y) && length(y) == N) {
    y_stan <- y
  } else {
    stop("y must be c(N,T,2) array or length-N list of length-T lists of length-2 numeric.")
  }

  V_stan <- vector("list", N)
  for (i in seq_len(N)) {
    V_stan[[i]] <- vector("list", T)
    for (t in seq_len(T)) {
      M <- as.matrix(V[[i]][[t]])
      M <- (M + t(M)) / 2 + diag(V_jitter, 2L)
      V_stan[[i]][[t]] <- M
    }
  }

  if (is.null(X1)) {
    X1 <- array(1, dim = c(N, T, 1L))
  }
  if (is.null(X2)) {
    X2 <- array(1, dim = c(N, T, 1L))
  }
  X1_stan <- vector("list", N)
  X2_stan <- vector("list", N)
  for (i in seq_len(N)) {
    X1_stan[[i]] <- lapply(seq_len(T), function(t) as.numeric(X1[i, t, , drop = TRUE]))
    X2_stan[[i]] <- lapply(seq_len(T), function(t) as.numeric(X2[i, t, , drop = TRUE]))
  }

  list(
    N = as.integer(N),
    T = as.integer(T),
    p1 = as.integer(p1),
    p2 = as.integer(p2),
    y = y_stan,
    V = V_stan,
    X1 = X1_stan,
    X2 = X2_stan,
    N_edges = as.integer(N_edges),
    node1 = as.integer(node1),
    node2 = as.integer(node2),
    scale_icar = as.numeric(scale_icar)
  )
}

fit_shared_bym2_st <- function(stan_data, stan_file, chains = 4L, parallel_chains = 4L,
                               iter_warmup = 1000L, iter_sampling = 1000L, refresh = 100L,
                               seed = 2025L, force_recompile = FALSE, adapt_delta = 0.95,
                               max_treedepth = 12L, output_dir = NULL) {
  mod <- cmdstan_model(stan_file, force_recompile = force_recompile)
  args <- list(
    data = stan_data,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = refresh,
    seed = seed,
    max_treedepth = max_treedepth,
    adapt_delta = adapt_delta
  )
  if (!is.null(output_dir)) args$output_dir <- output_dir
  do.call(mod$sample, args)
}

#' Posterior medians and 95% CI for p_hat by region × time × outcome
summarize_st_prevalence <- function(fit, region_names, time_labels, probs = c(0.025, 0.5, 0.975)) {
  N <- length(region_names)
  T <- length(time_labels)
  rows <- list()
  idx <- 0L
  arr <- fit$draws(variables = "p_hat", format = "draws_matrix")
  cn <- colnames(arr)
  for (i in seq_len(N)) {
    for (t in seq_len(T)) {
      for (k in 1:2) {
        # CmdStan names: p_hat[i,t,k] with commas
        v <- paste0("p_hat[", i, ",", t, ",", k, "]")
        if (!v %in% cn) {
          v2 <- grep(paste0("^p_hat\\[", i, ",", t, ","), cn, value = TRUE)
          if (length(v2) >= k) v <- v2[k]
          else stop("Missing draw column for i=", i, " t=", t, " k=", k)
        }
        x <- arr[, v]
        idx <- idx + 1L
        rows[[idx]] <- data.frame(
          region = region_names[i],
          survey = time_labels[t],
          time_index = t,
          outcome = c("Adolescent", "Young_adult")[k],
          p_median = median(x),
          p_lo = quantile(x, probs[1]),
          p_hi = quantile(x, probs[3]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  bind_rows(rows)
}

#' Same for theta (logit scale)
summarize_st_theta <- function(fit, region_names, time_labels, probs = c(0.025, 0.5, 0.975)) {
  N <- length(region_names)
  T <- length(time_labels)
  rows <- list()
  idx <- 0L
  arr <- fit$draws(variables = "theta", format = "draws_matrix")
  cn <- colnames(arr)
  for (i in seq_len(N)) {
    for (t in seq_len(T)) {
      for (k in 1:2) {
        v <- paste0("theta[", i, ",", t, ",", k, "]")
        if (!v %in% cn) {
          v2 <- grep(paste0("^theta\\[", i, ",", t, ","), cn, value = TRUE)
          if (length(v2) >= k) v <- v2[k]
          else stop("Missing theta column i=", i, " t=", t)
        }
        x <- arr[, v]
        idx <- idx + 1L
        rows[[idx]] <- data.frame(
          region = region_names[i],
          survey = time_labels[t],
          outcome = c("Adolescent", "Young_adult")[k],
          theta_median = median(x),
          theta_lo = quantile(x, probs[1]),
          theta_hi = quantile(x, probs[3]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  dplyr::bind_rows(rows)
}
