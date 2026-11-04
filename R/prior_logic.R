## Trial logic: One-drug prior calibration -- BOBYQA search for the Beta(a0,b0)/Beta(a1,b1) hyperparameters that match the clinician-elicited P(MTD < min dose) and P(MTD > max dose) targets (prior calibration, one-drug)
#' Calibrate single-agent MEWOC prior hyperparameters
#'
#' Finds beta prior hyperparameters for the reparameterized single-agent MEWOC
#' model so that the prior matches the clinician's elicited probabilities that the
#' maximum tolerated dose (MTD) lies below the minimum dose (`p0`) and above the
#' maximum dose (`p1`). The search uses the BOBYQA derivative-free optimizer
#' (package 'minqa'). The function name is kept for compatibility; no grid search
#' is performed.
#'
#' With `rho0` and `rho1` the probabilities of a dose-limiting toxicity (DLT) at the
#' minimum and maximum dose, the prior is `rho1 ~ Beta(a0, b0)` truncated to
#' `[0.001, 0.999]`, matching the posterior model, and
#' `rho0 / rho1 ~ Beta(a1, b1)`, with `a0 + b0 = a1 + b1 = condition`.
#'
#' @param theta Target probability of a DLT, strictly between 0 and 1.
#' @param p0 Prior probability that the MTD is below the minimum dose (0 to 1).
#' @param p1 Prior probability that the MTD is above the maximum dose (0 to 1);
#'   `p0 + p1` must not exceed 1.
#' @param condition Sum of the shape parameters of each beta prior (prior
#'   effective sample size); must be greater than 0.2. The app uses 2.
#' @param progress Optional 'shiny' `Progress` object (used by the app).
#' @param ... Further arguments for the BOBYQA search: `seed` (random seed; the
#'   caller's random-number state is restored afterwards), `n_prior` (Monte Carlo
#'   draws used to check the result, default 60000), `n_search_prior` (draws used
#'   during the search), `bobyqa_starts` (number of starting points, default 3) and
#'   `bobyqa_maxfun` (maximum function evaluations per start, default 200).
#'
#' @return A one-row data frame with the hyperparameters `a0`, `b0`, `a1`, `b1`;
#'   the prior probabilities they give (`matched_p0`, `matched_p1`); the targets
#'   (`target_p0`, `target_p1`); the distance between them (`rank_distance`); and
#'   the search settings (`n_search_prior`, `n_prior`, `bobyqa_starts`,
#'   `bobyqa_maxfun`). Additional diagnostics are `abs_error_p0` and
#'   `abs_error_p1` (absolute differences from the targets), and `mcse_p0` and
#'   `mcse_p1` (Monte Carlo standard errors of the matched probabilities).
#'   Each MCSE is `sqrt(p * (1 - p) / n)`, where `p` is the matched probability
#'   and `n` is the number of nonmissing validation indicators. It describes
#'   validation sampling uncertainty conditional on the selected hyperparameters,
#'   not optimizer convergence or uncertainty in the elicited targets. An MCSE
#'   of zero at an estimated probability of zero or one does not establish that
#'   the event is impossible or certain. No acceptance threshold is applied.
#'   These diagnostics are returned by the function and are not displayed in the
#'   Shiny app. Pass the result as `opt1` to the other functions.
#'
#' @references
#' Tighiouart M, Rogatko A (2010). Dose finding with escalation with overdose
#' control (EWOC) in cancer clinical trials. Statistical Science, 25(2), 217-226.
#' \doi{10.1214/10-STS333}
#'
#' @seealso [build_prior_plot()] to display the calibrated prior.
#' @examples
#' prior <- run_mewoc_grid_search(theta = 0.33, p0 = 0.1, p1 = 0.2, condition = 2,
#'                                seed = 1, n_prior = 6000, bobyqa_starts = 1)
#' prior[, c("a0", "b0", "a1", "b1", "matched_p0", "matched_p1")]
#' @export
run_mewoc_grid_search <- function(theta, p0, p1, condition, progress = NULL, ...) {
  is_num1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!is_num1(theta) || theta <= 0 || theta >= 1) stop("theta must be a single number strictly between 0 and 1.", call. = FALSE)
  if (!is_num1(p0) || p0 < 0 || p0 > 1) stop("p0 (prior probability that the MTD is below the minimum dose) must be between 0 and 1.", call. = FALSE)
  if (!is_num1(p1) || p1 < 0 || p1 > 1) stop("p1 (prior probability that the MTD is above the maximum dose) must be between 0 and 1.", call. = FALSE)
  if (p0 + p1 > 1) stop("p0 + p1 must not be greater than 1.", call. = FALSE)
  if (!is_num1(condition)) stop("condition must be a single number.", call. = FALSE)
  run_mewoc_bobyqa_search(theta = theta, p0 = p0, p1 = p1, condition = condition, progress = progress, ...)
}

## Trial logic: One-drug prior calibration (BOBYQA method) -- derivative-free optimization over (a0, b1) against shared-uniform Monte Carlo draws to match the clinician-elicited P(MTD < min dose) and P(MTD > max dose) targets, then re-validates the winner at the full draw count (prior calibration, one-drug)
q_mewoc_rho1 <- function(p, a0, b0) {
  bounds <- mewoc_rho1_bounds()
  if (!is.numeric(p) || any(!is.finite(p)) || any(p < 0 | p > 1) ||
      !is.numeric(a0) || length(a0) != 1L || !is.finite(a0) || a0 <= 0 ||
      !is.numeric(b0) || length(b0) != 1L || !is.finite(b0) || b0 <= 0) {
    stop("Prior quantiles require probabilities in [0, 1] and positive finite beta shapes.", call. = FALSE)
  }
  # Invert the conditional CDF, rather than clamping ordinary beta draws.
  # Use the survival tail for distributions concentrated near one.
  lower_tail <- stats::pbeta(bounds[["lower"]], a0, b0) <= 0.5
  lo <- stats::pbeta(bounds[["lower"]], a0, b0, lower.tail = lower_tail)
  hi <- stats::pbeta(bounds[["upper"]], a0, b0, lower.tail = lower_tail)
  if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
    stop("The beta prior has negligible probability within the rho1 truncation bounds.", call. = FALSE)
  }
  draws <- stats::qbeta(lo + p * (hi - lo), a0, b0, lower.tail = lower_tail)
  pmin(bounds[["upper"]], pmax(bounds[["lower"]], draws))
}

run_mewoc_bobyqa_search <- function(theta, p0, p1, condition, progress = NULL,
                                    n_prior = 60000, n_search_prior = NULL,
                                    bobyqa_starts = 3, bobyqa_maxfun = 200,
                                    seed = NULL) {
  if (!requireNamespace("minqa", quietly = TRUE)) {
    stop("Package 'minqa' is required for BOBYQA prior search.", call. = FALSE)
  }
  if (!is.finite(condition) || condition <= 0.2) {
    stop("Prior parameter condition must be greater than 0.2.", call. = FALSE)
  }
  check_numeric_scalar(n_prior, "n_prior", 1000, integer = TRUE)
  check_numeric_scalar(bobyqa_starts, "bobyqa_starts", 1, integer = TRUE)
  check_numeric_scalar(bobyqa_maxfun, "bobyqa_maxfun", 1, integer = TRUE)
  if (!is.null(seed)) check_numeric_scalar(seed, "seed", 0, integer = TRUE)
  if (!is.null(seed)) {
    restore_random_state <- save_random_state()
    on.exit(restore_random_state(), add = TRUE)
    set.seed(seed)
  }
  
  if (is.null(n_search_prior)) {
    n_search_prior <- min(as.integer(n_prior), 6000L)
  }
  check_numeric_scalar(n_search_prior, "n_search_prior", 1000, integer = TRUE)
  n_search_prior <- as.integer(n_search_prior)
  if (!is.finite(n_search_prior) || n_search_prior < 1000L) {
    stop("BOBYQA search draw count must be at least 1000.", call. = FALSE)
  }
  
  lo <- 0.1
  hi <- condition - 0.1
  target <- c(p_0 = p0, p_1 = p1)
  
  if (!is.null(progress)) {
    progress$set(
      message = "Running prior calculations...",
      detail = "Preparing BOBYQA rho prior search",
      value = 0.20
    )
  }
  
  # SK-style note: shared uniforms are drawn once and every candidate BOBYQA
  # proposes is scored against the same quantiles (via qbeta), so the search
  # objective is a deterministic empirical function of (a0, b1) rather than a
  # fresh-noise Monte Carlo draw each call -- BOBYQA's trust-region steps
  # assume a stable objective, so re-drawing rbeta() per-candidate would make
  # it chase simulation noise instead of converging. Uses the # BOBYQA search is used as the single-agent continuous optimizer.
  search_uniform_draws <- list(
    rho1 = stats::runif(n_search_prior),
    u = stats::runif(n_search_prior)
  )
  validation_uniform_draws <- list(
    rho1 = stats::runif(n_prior),
    u = stats::runif(n_prior)
  )
  
  score_candidate <- function(par, uniform_draws, return_details = FALSE) {
    par <- pmin(pmax(as.numeric(par), lo), hi)
    a0 <- par[1]
    b1 <- par[2]
    b0 <- condition - a0
    a1 <- condition - b1
    
    rho1 <- q_mewoc_rho1(uniform_draws$rho1, a0, b0)
    u <- stats::qbeta(uniform_draws$u, shape1 = a1, shape2 = b1)
    rho0 <- rho1 * u
    mtd <- (stats::qlogis(theta) - stats::qlogis(rho0)) / (stats::qlogis(rho1) - stats::qlogis(rho0))
    
    sim <- c(
      p_0 = mean(mtd < 0, na.rm = TRUE),
      p_1 = mean(mtd > 1, na.rm = TRUE)
    )
    dist <- sum((sim - target)^2)
    
    if (isTRUE(return_details)) {
      return(list(dist = dist, sim = sim, a0 = a0, b0 = b0, a1 = a1, b1 = b1,
                  n_validation = sum(!is.na(mtd))))
    }
    dist
  }
  
  score_search_candidate <- function(par) {
    score_candidate(par, uniform_draws = search_uniform_draws)
  }
  
  # Only a 2-D single-agent search, so fewer restarts are
  # needed to avoid local minima: box center plus one offset start along each
  # axis.
  center <- rep(condition / 2, 2L)
  deterministic_starts <- rbind(
    center,
    c(lo + 0.25 * (hi - lo), center[2]),
    c(center[1], lo + 0.25 * (hi - lo))
  )
  n_start <- max(1L, as.integer(bobyqa_starts))
  starts <- deterministic_starts[seq_len(min(n_start, nrow(deterministic_starts))), , drop = FALSE]
  if (n_start > nrow(starts)) {
    starts <- rbind(
      starts,
      matrix(stats::runif((n_start - nrow(starts)) * 2L, lo, hi), ncol = 2L)
    )
  }
  
  if (!is.null(progress)) {
    progress$set(
      message = "Running prior calculations...",
      detail = paste0("Running BOBYQA from ", nrow(starts), " start point(s)"),
      value = 0.45
    )
  }
  
  fits <- lapply(seq_len(nrow(starts)), function(i) {
    minqa::bobyqa(
      par = starts[i, ],
      fn = score_search_candidate,
      lower = rep(lo, 2L),
      upper = rep(hi, 2L),
      control = list(maxfun = as.integer(bobyqa_maxfun))
    )
  })
  fit_values <- vapply(fits, function(x) x$fval, numeric(1))
  best_fit <- fits[[which.min(fit_values)]]
  
  if (!is.null(progress)) {
    progress$set(
      message = "Running prior calculations...",
      detail = "Validating selected prior settings",
      value = 0.80
    )
  }
  
  # Re-score the winning (a0, b1) at the full validation draw count so the
  # reported matched probabilities aren't the noisier search-time estimate.
  best_details <- score_candidate(
    best_fit$par,
    uniform_draws = validation_uniform_draws,
    return_details = TRUE
  )
  matched_p0 <- best_details$sim[["p_0"]]
  matched_p1 <- best_details$sim[["p_1"]]
  n_validation <- best_details$n_validation
  mcse <- function(p) if (n_validation > 0L) sqrt(p * (1 - p) / n_validation) else NA_real_
  
  if (!is.null(progress)) {
    progress$set(
      message = "Running prior calculations...",
      detail = "Complete",
      value = 0.95
    )
  }
  
  data.frame(
    a0 = best_details$a0, b0 = best_details$b0,
    a1 = best_details$a1, b1 = best_details$b1,
    matched_p0 = best_details$sim[["p_0"]],
    matched_p1 = best_details$sim[["p_1"]],
    target_p0 = p0,
    target_p1 = p1,
    rank_distance = best_details$dist,
    n_search_prior = n_search_prior,
    n_prior = n_prior,
    bobyqa_starts = nrow(starts),
    bobyqa_maxfun = as.integer(bobyqa_maxfun),
    abs_error_p0 = abs(matched_p0 - p0),
    abs_error_p1 = abs(matched_p1 - p1),
    mcse_p0 = mcse(matched_p0),
    mcse_p1 = mcse(matched_p1),
    check.names = FALSE
  )
}

## Trial logic: Simulate the prior-predictive DLT probability at each dose level from the selected (a0,b0,a1,b1) and render the per-dose density plot (prior calibration, one-drug, display)
#' Plot calibrated prior toxicity distributions
#'
#' Draws the prior distribution of the probability of a dose-limiting toxicity
#' (DLT) at each dose implied by calibrated prior hyperparameters, as shown in the
#' MEWOC app and reports.
#'
#' @param opt1 Prior hyperparameters: a list or one-row data frame with elements
#'   `a0`, `b0`, `a1`, `b1`, for example the result of [run_mewoc_grid_search()].
#' @param num_doses Number of doses to display.
#' @param seed_value Random seed for the Monte Carlo draws; the caller's
#'   random-number state is restored afterwards.
#' @param dose_values Optional standardized dose positions between 0 (minimum
#'   dose) and 1 (maximum dose), one per dose; by default equally spaced.
#' @param dose_labels Optional labels for the doses, for example `"10 mg"`.
#' @param plot_title Plot title.
#'
#' @return A 'ggplot2' object with one density panel per dose.
#' @examples
#' prior <- data.frame(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)
#' build_prior_plot(prior, num_doses = 4, seed_value = 1,
#'                  dose_labels = c("10 mg", "20 mg", "30 mg", "40 mg"))
#' @export
build_prior_plot <- function(opt1, num_doses, seed_value,
                             dose_values = NULL,
                             dose_labels = NULL,
                             plot_title = "Distribution of Probability of DLT for Each Dose Level") {
  check_prior_parameters(opt1)
  check_numeric_scalar(num_doses, "num_doses", 1, integer = TRUE)
  check_numeric_scalar(seed_value, "seed_value", 0, integer = TRUE)
  
  n_sim <- 60000
  d_pdlt <- mat.or.vec(n_sim, num_doses)
  doselevels <- if (is.null(dose_values)) {
    seq(0, 1, length.out = num_doses)
  } else {
    as.numeric(dose_values)
  }
  if (length(doselevels) != num_doses) {
    stop("dose_values must have the same length as num_doses.")
  }
  if (anyNA(doselevels) || any(doselevels < 0 | doselevels > 1)) {
    stop("dose_values must be standardized dose positions between 0 and 1.", call. = FALSE)
  }
  dl_name <- if (is.null(dose_labels)) {
    as.character(seq_len(num_doses))
  } else {
    as.character(dose_labels)
  }
  if (length(dl_name) != num_doses) {
    stop("dose_labels must have the same length as num_doses.")
  }
  
  restore_random_state <- save_random_state()
  on.exit(restore_random_state(), add = TRUE)
  set.seed(seed_value)
  rho1_d <- q_mewoc_rho1(stats::runif(n_sim), opt1$a0, opt1$b0)
  u_d <- rbeta(n = n_sim, opt1$a1, opt1$b1)
  rho0_d <- rho1_d * u_d
  
  for (d in seq_len(num_doses)) {
    d_pdlt[, d] <- compute_pdlt(rho0 = rho0_d, rho1 = rho1_d, X = doselevels[d])
  }
  
  d_pdlt <- as.data.frame(d_pdlt)
  colnames(d_pdlt) <- dl_name
  d_pdlt_long <- pivot_longer(
    d_pdlt,
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )
  d_pdlt_long$variable <- factor(d_pdlt_long$variable, levels = dl_name)
  
  panel_limits <- d_pdlt_long %>%
    group_by(variable) %>%
    summarise(
      hist_density_max = {
        hist_data <- hist(value, breaks = 50, plot = FALSE)
        max(hist_data$density, na.rm = TRUE)
      },
      density_curve_max = max(density(value, na.rm = TRUE)$y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      panel_y_max = pmax(hist_density_max, density_curve_max) + 1,
      label_y = pmax(hist_density_max, density_curve_max) + 0.15
    )
  
  centralized_label_y <- max(panel_limits$label_y, na.rm = TRUE)
  
  median_value <- d_pdlt_long %>%
    group_by(variable) %>%
    summarise(median_val = median(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(panel_limits %>% select(variable, panel_y_max), by = "variable") %>%
    mutate(label_y = centralized_label_y)
  
  ggplot(d_pdlt_long, aes(x = value)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 50,
      na.rm = TRUE,
      fill = NA,
      color = "black"
    ) +
    geom_density(color = "blue", linewidth = 1, na.rm = TRUE) +
    geom_blank(
      data = median_value,
      aes(x = median_val, y = panel_y_max),
      inherit.aes = FALSE
    ) +
    geom_vline(
      data = median_value,
      aes(xintercept = median_val),
      color = "red",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    facet_wrap(
      ~ variable,
      ncol = min(3, num_doses),
      scales = "free",
      labeller = labeller(
        variable = function(dose_level) {
          median_val <- median_value %>%
            filter(variable == dose_level) %>%
            pull(median_val)
          if (length(median_val) > 0) {
            paste0(
              "<b style='font-size:20px; line-height:1.35'>", dose_level, "</b>",
              "<br><br><span style='font-size:15px; line-height:1.25'>Median = ", round(median_val, 3), "</span>"
            )
          } else {
            dose_level
          }
        }
      )
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.02, 0)),
      labels = scales::number_format(accuracy = 0.01)
    ) +
    labs(title = plot_title) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      strip.background = element_blank(),
      strip.text = ggtext::element_markdown(size = 11, face = "bold", margin = margin(b = 8, t = 6)),
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(size = 10)
    )
}
