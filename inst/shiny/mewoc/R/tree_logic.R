branch_patterns_for_cohort <- function(cohort_size) {
  switch(
    as.character(cohort_size),
    "1" = list(
      list(pattern = c(0L), label = "0 DLT"),
      list(pattern = c(1L), label = "1 DLT")
    ),
    "2" = list(
      list(pattern = c(0L, 0L), label = "0-0 DLT"),
      list(pattern = c(0L, 1L), label = "0-1 DLT"),
      list(pattern = c(1L, 1L), label = "1-1 DLT")
    ),
    "3" = list(
      list(pattern = c(0L, 0L, 0L), label = "0-0-0 DLT"),
      list(pattern = c(0L, 1L, 0L), label = "0-1-0 DLT"),
      list(pattern = c(0L, 1L, 1L), label = "0-1-1 DLT"),
      list(pattern = c(1L, 1L, 1L), label = "1-1-1 DLT")
    ),
    "4" = list(
      list(pattern = c(0L, 0L, 0L, 0L), label = "0-0-0-0 DLT"),
      list(pattern = c(0L, 0L, 0L, 1L), label = "0-0-0-1 DLT"),
      list(pattern = c(0L, 0L, 1L, 1L), label = "0-0-1-1 DLT"),
      list(pattern = c(0L, 1L, 1L, 1L), label = "0-1-1-1 DLT"),
      list(pattern = c(1L, 1L, 1L, 1L), label = "1-1-1-1 DLT")
    ),
    "5" = list(
      list(pattern = c(0L, 0L, 0L, 0L, 0L), label = "0-0-0-0-0 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 1L), label = "0-0-0-0-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 1L, 1L), label = "0-0-0-1-1 DLT"),
      list(pattern = c(0L, 0L, 1L, 1L, 1L), label = "0-0-1-1-1 DLT"),
      list(pattern = c(0L, 1L, 1L, 1L, 1L), label = "0-1-1-1-1 DLT"),
      list(pattern = c(1L, 1L, 1L, 1L, 1L), label = "1-1-1-1-1 DLT")
    ),
    "6" = list(
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 0L), label = "0-0-0-0-0-0 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 1L), label = "0-0-0-0-0-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 1L, 1L), label = "0-0-0-0-1-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 1L, 1L, 1L), label = "0-0-0-1-1-1 DLT"),
      list(pattern = c(0L, 0L, 1L, 1L, 1L, 1L), label = "0-0-1-1-1-1 DLT"),
      list(pattern = c(0L, 1L, 1L, 1L, 1L, 1L), label = "0-1-1-1-1-1 DLT"),
      list(pattern = c(1L, 1L, 1L, 1L, 1L, 1L), label = "1-1-1-1-1-1 DLT")
    ),
    "7" = list(
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 0L, 0L), label = "0-0-0-0-0-0-0 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 0L, 1L), label = "0-0-0-0-0-0-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 1L, 1L), label = "0-0-0-0-0-1-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 1L, 1L, 1L), label = "0-0-0-0-1-1-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 1L, 1L, 1L, 1L), label = "0-0-0-1-1-1-1 DLT"),
      list(pattern = c(0L, 0L, 1L, 1L, 1L, 1L, 1L), label = "0-0-1-1-1-1-1 DLT"),
      list(pattern = c(0L, 1L, 1L, 1L, 1L, 1L, 1L), label = "0-1-1-1-1-1-1 DLT"),
      list(pattern = c(1L, 1L, 1L, 1L, 1L, 1L, 1L), label = "1-1-1-1-1-1-1 DLT")
    ),
    "8" = list(
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), label = "0-0-0-0-0-0-0-0 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L), label = "0-0-0-0-0-0-0-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L), label = "0-0-0-0-0-0-1-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L), label = "0-0-0-0-0-1-1-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L), label = "0-0-0-0-1-1-1-1 DLT"),
      list(pattern = c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L), label = "0-0-0-1-1-1-1-1 DLT"),
      list(pattern = c(0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L), label = "0-0-1-1-1-1-1-1 DLT"),
      list(pattern = c(0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), label = "0-1-1-1-1-1-1-1 DLT"),
      list(pattern = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), label = "1-1-1-1-1-1-1-1 DLT")
    )
  )
}

branch_labels_to_dlt_counts <- function(branch_labels) {
  setNames(
    vapply(branch_labels, function(lbl) {
      pattern_token <- strsplit(lbl, " ", fixed = TRUE)[[1]][1]
      dose_outcomes <- as.integer(strsplit(pattern_token, "-", fixed = TRUE)[[1]])
      paste(sum(dose_outcomes, na.rm = TRUE), "DLT")
    }, character(1)),
    branch_labels
  )
}

tree_plot_height <- function(branch_factor, max_depth) {
  n_leaves <- branch_factor ^ max_depth
  max(420, min(1800, n_leaves * 42))
}

tree_progress_state <- function(total_fits) {
  list(total = max(as.integer(total_fits), 1L), done = 0L)
}

tick_tree_progress <- function(progress_state, progress) {
  progress_state$done <- progress_state$done + 1L
  if (!is.null(progress)) {
    progress$set(
      value = min(progress_state$done / progress_state$total, 1),
      detail = paste0("Computed ", progress_state$done, " of ", progress_state$total, " posterior updates")
    )
  }
}

fit_tree_branch_result <- function(model_string, X, Y, opt1, theta, burn, mm_val) {
  jags_init <- list(
    .RNG.name = "base::Wichmann-Hill",
    .RNG.seed = as.integer(sample.int(.Machine$integer.max, 1))
  )
  burn_split <- split_burn_in(burn)
  j <- rjags::jags.model(
    textConnection(model_string),
    data = list(
      Y = Y, X = X,
      a0 = opt1$a0, a1 = opt1$a1,
      b0 = opt1$b0, b1 = opt1$b1,
      theta = theta, N = length(X)
    ),
    inits = jags_init,
    n.chains = 1,
    n.adapt = burn_split$n_adapt,
    quiet = TRUE
  )
  if (burn_split$n_burn > 0L) {
    stats::update(j, n.iter = burn_split$n_burn, progress.bar = "none")
  }
  s <- rjags::coda.samples(j, c("rho0", "rho1", "mtd"), mm_val)
  as.data.frame(s[[1]])
}

## Tree logic: one decision on a tree branch, with the same rules and order as the simulation engines
## (run_ewoc_cohort_loop): check the Delta stop (> Delta at the minimum dose); recommend the next dose;
## check the efficiency stop (at least the maximum number of patients already treated at the recommended
## dose). recommend(ss, alpha_value) returns list(dose_value, dose_index).
tree_branch_decision <- function(ss, theta, deltastop, alpha_value, recommend,
                                 assigned_doses = numeric(0), efficiency = NULL) {
  safety <- safety_stop_check(ss, theta, deltastop)
  out <- list(
    delta_prob = safety$probability,
    stop_trial = FALSE,
    stop_type = NA_character_,
    next_dose_idx = NA_integer_,
    next_dose_value = NA_real_
  )
  if (isTRUE(safety$stop)) {
    out$stop_trial <- TRUE
    out$stop_type <- "safety"
    return(out)
  }
  next_dose <- recommend(ss, alpha_value)
  out$next_dose_value <- next_dose$dose_value
  out$next_dose_idx <- if (is.null(next_dose$dose_index)) NA_integer_ else as.integer(next_dose$dose_index)
  if (!is.null(efficiency) &&
      efficiency_stop_check(assigned_doses, next_dose$dose_value, efficiency$max_patients, efficiency$width)) {
    out$stop_trial <- TRUE
    out$stop_type <- "efficiency"
  }
  out
}

compute_tree_decision <- function(ss, doselevels, theta, alpha_value, current_dose, assigned_doses, incr,
                                  deltastop = 1, efficiency = NULL) {
  tree_branch_decision(
    ss, theta, deltastop, alpha_value,
    recommend = function(ss, alpha_value) {
      recommend_next_dose(ss, alpha_value, doselevels, current_dose, assigned_doses, incr)
    },
    assigned_doses = assigned_doses,
    efficiency = efficiency
  )
}

compute_continuous_tree_decision <- function(ss, theta, alpha_value, current_dose, max_increment_std,
                                             deltastop = 1, assigned_doses = numeric(0), efficiency = NULL) {
  tree_branch_decision(
    ss, theta, deltastop, alpha_value,
    recommend = function(ss, alpha_value) {
      recommend_next_continuous_dose(ss, alpha_value, current_dose, max_increment_std)
    },
    assigned_doses = assigned_doses,
    efficiency = efficiency
  )
}

## Standardized dose to actual dose.
continuous_tree_actual_dose <- function(standardized_dose, min_dose, max_dose) {
  min_dose + as.numeric(standardized_dose) * (max_dose - min_dose)
}

continuous_tree_dose_label <- function(standardized_dose, min_dose, max_dose, dose_unit) {
  actual_dose <- continuous_tree_actual_dose(standardized_dose, min_dose, max_dose)
  label <- format(round(actual_dose, 1), nsmall = 1, trim = TRUE, scientific = FALSE)
  if (!is.null(dose_unit) && nzchar(trimws(as.character(dose_unit)))) {
    label <- paste(label, trimws(as.character(dose_unit)))
  }
  standardized_label <- format(
    round(as.numeric(standardized_dose), 2),
    nsmall = 2,
    trim = TRUE,
    scientific = FALSE
  )
  paste0(label, "\n(x=", standardized_label, ")")
}

format_tree_alpha_label <- function(alpha_values) {
  alpha_values <- sort(unique(round(alpha_values[is.finite(alpha_values)], 3)))
  if (length(alpha_values) == 0L) {
    return(NULL)
  }
  if (length(alpha_values) == 1L) {
    return(paste0("\u03B1=", format(alpha_values, trim = TRUE)))
  }
  paste0(
    "\u03B1=",
    format(min(alpha_values), trim = TRUE),
    "-",
    format(max(alpha_values), trim = TRUE)
  )
}

build_adaptive_tree_x_labels <- function(max_depth, first_label, adaptive_prefix = "Cohort",
                                         alpha_labels = NULL) {
  labels <- character(max_depth + 1L)
  labels[1] <- first_label
  if (max_depth >= 1L) {
    labels[2:(max_depth + 1L)] <- vapply(seq_len(max_depth), function(depth_idx) {
      alpha_label <- if (length(alpha_labels) >= depth_idx) alpha_labels[[depth_idx]] else NULL
      if (is.null(alpha_label) || !nzchar(alpha_label)) {
        paste0(adaptive_prefix, " ", depth_idx + 1L)
      } else {
        paste0(adaptive_prefix, " ", depth_idx + 1L, "\n(", alpha_label, ")")
      }
    }, character(1))
  }
  labels
}

## Tree logic: EWOC decision tree shared by discrete and continuous designs. The root cohort is treated
## at the root dose; each branch adds one cohort with a DLT pattern and makes the next decision with
## tree_branch_decision(). History (hist_X0, hist_Y0) is data collected before the root cohort (the
## accelerated lead-in). Alpha follows the simulation engines: the cohort treated at the starting dose
## does not advance alpha; each EWOC cohort advances it when the cumulative DLT rate is below the cutoff.
## decide(ss, alpha_value, current_dose, assigned_doses) returns a branch decision.
build_ewoc_tree_structure <- function(dose_type, root_dose_value, root_dose_idx, cohort_size, max_depth,
                                      theta, burn, mm_val, alpha, opt1, model_string, decide,
                                      hist_X0 = numeric(0), hist_Y0 = integer(0), root_is_ewoc = FALSE,
                                      alpha_increment_cutoff = theta, progress_state = NULL, progress = NULL,
                                      label_for = NULL, title_text = NULL, panel_title = NULL,
                                      subtitle_text = NULL, root_label = NULL) {
  branch_patterns <- branch_patterns_for_cohort(cohort_size)
  branch_factor <- length(branch_patterns)
  next_id <- 2L
  jags_cache <- list()

  nodes_acc <- list(list(
    id = 1L, parent_id = NA_integer_, depth = 0L,
    dose_idx = as.integer(root_dose_idx), dose_value = root_dose_value, tree_pos = 0L, y_pos = NA_real_,
    is_stop = FALSE, stop_type = NA_character_, delta_prob = NA_real_,
    alpha_value_used = NA_real_,
    custom_label = if (is.null(root_label)) NA_character_ else as.character(root_label)
  ))
  edges_acc <- list()
  queue <- list(list(
    node_id = 1L, depth = 0L, dose_value = root_dose_value, dose_idx = as.integer(root_dose_idx),
    hist_X = hist_X0, hist_Y = hist_Y0, tree_pos = 0L,
    alpha_step = 1L, advance_alpha = isTRUE(root_is_ewoc)
  ))

  while (length(queue) > 0) {
    item <- queue[[1]]
    queue <- queue[-1]
    if (item$depth >= max_depth) {
      next
    }

    for (branch_idx in seq_along(branch_patterns)) {
      branch <- branch_patterns[[branch_idx]]
      new_X <- c(item$hist_X, rep(item$dose_value, cohort_size))
      new_Y <- c(item$hist_Y, branch$pattern)
      alpha_step <- if (isTRUE(item$advance_alpha)) {
        advance_alpha_step_if_allowed(item$alpha_step, alpha, new_Y, new_X, theta, alpha_increment_cutoff)
      } else {
        max(1L, min(item$alpha_step, length(alpha)))
      }

      ord <- order(new_X, new_Y)
      cache_key <- paste0(
        paste(round(new_X[ord], 8), new_Y[ord], sep = ":", collapse = "|"),
        "__a", alpha_step,
        "__c", cohort_size,
        "__d", round(item$dose_value, 8)
      )

      if (!is.null(jags_cache[[cache_key]])) {
        fit_result <- jags_cache[[cache_key]]
      } else {
        fit_result <- tryCatch({
          ss <- fit_tree_branch_result(
            model_string = model_string,
            X = new_X,
            Y = new_Y,
            opt1 = opt1,
            theta = theta,
            burn = burn,
            mm_val = mm_val
          )
          decision <- decide(ss, alpha[alpha_step], item$dose_value, new_X)
          decision$alpha_value_used <- alpha[alpha_step]
          decision$alpha_step_used <- alpha_step
          jags_cache[[cache_key]] <- decision
          if (!is.null(progress_state)) {
            tick_tree_progress(progress_state, progress)
          }
          decision
        }, error = function(e) {
          stop(
            paste0(
              "Escalation tree generation failed for cohort size ", cohort_size,
              ", decision step ", item$depth + 1L,
              ", branch '", branch$label, "'. ",
              "JAGS returned: ", conditionMessage(e)
            ),
            call. = FALSE
          )
        })
      }

      new_tree_pos <- item$tree_pos * branch_factor + (branch_idx - 1L)
      stop_label <- if (identical(fit_result$stop_type, "efficiency")) "EFF\nSTOP" else NA_character_
      nodes_acc[[length(nodes_acc) + 1L]] <- list(
        id = next_id, parent_id = item$node_id,
        depth = item$depth + 1L,
        dose_idx = fit_result$next_dose_idx,
        dose_value = fit_result$next_dose_value,
        tree_pos = new_tree_pos, y_pos = NA_real_,
        is_stop = fit_result$stop_trial,
        stop_type = fit_result$stop_type,
        delta_prob = fit_result$delta_prob,
        alpha_value_used = fit_result$alpha_value_used,
        custom_label = if (isTRUE(fit_result$stop_trial)) {
          stop_label
        } else if (is.function(label_for)) {
          label_for(fit_result$next_dose_value, fit_result$next_dose_idx)
        } else {
          NA_character_
        }
      )
      edges_acc[[length(edges_acc) + 1L]] <- list(
        from_id = item$node_id,
        to_id = next_id,
        branch_label = branch$label,
        dlt_total = sum(branch$pattern),
        stop_trial = fit_result$stop_trial
      )

      if (!isTRUE(fit_result$stop_trial)) {
        queue[[length(queue) + 1L]] <- list(
          node_id = next_id, depth = item$depth + 1L,
          dose_value = fit_result$next_dose_value,
          dose_idx = fit_result$next_dose_idx,
          hist_X = new_X, hist_Y = new_Y,
          tree_pos = new_tree_pos,
          alpha_step = fit_result$alpha_step_used,
          advance_alpha = TRUE
        )
      }

      next_id <- next_id + 1L
    }
  }

  nodes <- dplyr::bind_rows(nodes_acc)
  edges <- dplyr::bind_rows(edges_acc)

  if (nrow(nodes) > 0) {
    bf_safe <- pmax(1L, branch_factor)
    total_width <- pmax(1, bf_safe ^ pmax(0L, max_depth))
    depth_diff <- pmax(0L, max_depth - nodes$depth)
    y_offset <- (nodes$tree_pos + 0.5) * (bf_safe ^ depth_diff)
    nodes$y_pos <- as.numeric(total_width) - y_offset
  } else {
    nodes$y_pos <- numeric(0)
  }

  alpha_labels <- lapply(seq_len(max_depth), function(depth_idx) {
    format_tree_alpha_label(nodes$alpha_value_used[nodes$depth == depth_idx])
  })

  list(
    nodes = nodes,
    edges = edges,
    dose_type = dose_type,
    max_depth = max_depth,
    cohort_size = cohort_size,
    branch_factor = branch_factor,
    branch_labels = vapply(branch_patterns, function(x) x$label, character(1)),
    plot_height = tree_plot_height(branch_factor, max_depth),
    alpha_values = rep(NA_real_, max_depth + 1L),
    alpha_labels = alpha_labels,
    title_text = title_text,
    panel_title = panel_title,
    subtitle_text = subtitle_text
  )
}

## Tree logic: efficiency stopping settings for the tree (NULL when the rule is not used).
tree_efficiency_settings <- function(tree_inputs, width = 0) {
  if (!efficiency_stop_enabled(tree_inputs$stopping_rule_selected, tree_inputs$num_patients_restrict)) {
    return(NULL)
  }
  list(max_patients = as.numeric(tree_inputs$num_patients_restrict), width = width)
}

build_tree_for_cohort_result <- function(tree_inputs, opt1, cohort_selected, seed_value, progress = NULL) {
  dose_type <- if (!is.null(tree_inputs$dose_type)) tree_inputs$dose_type else "Discrete"
  theta <- tree_inputs$theta
  deltastop <- tree_inputs$deltastop
  burn <- tree_inputs$burn
  mm_val <- tree_inputs$mm
  alpha_increment_cutoff <- tree_inputs$alpha_increment_cutoff
  total_n <- tree_inputs$NN
  max_cohorts <- as.integer(total_n / tree_inputs$cohort)
  standard_max_depth <- min(tree_inputs$tree_steps - 1L, pmax(1L, max_cohorts - 1L))
  alpha <- build_alpha_sequence(tree_inputs$lower_f_bound, tree_inputs$increment, standard_max_depth + 1L)
  set.seed(seed_value + 42)
  model_string <- build_jags_model_string()
  branch_factor <- length(branch_patterns_for_cohort(cohort_selected))
  progress_state <- tree_progress_state(sum(branch_factor ^ (0:(standard_max_depth - 1L))))

  if (identical(dose_type, "Continuous")) {
    min_dose <- tree_inputs$min_dose
    max_dose <- tree_inputs$max_dose
    dose_unit <- tree_inputs$dose_unit
    max_increment_std <- tree_inputs$max_dose_increment / 100
    efficiency <- tree_efficiency_settings(tree_inputs, width = suppressWarnings(as.numeric(tree_inputs$efficiency_stop_tolerance)) / 100)
    label_for <- function(dose_value, dose_idx) continuous_tree_dose_label(dose_value, min_dose, max_dose, dose_unit)
    continuous_sc <- build_ewoc_tree_structure(
      dose_type = "Continuous",
      root_dose_value = 0,
      root_dose_idx = NA_integer_,
      cohort_size = cohort_selected,
      max_depth = standard_max_depth,
      theta = theta, burn = burn, mm_val = mm_val, alpha = alpha, opt1 = opt1, model_string = model_string,
      decide = function(ss, alpha_value, current_dose, assigned_doses) {
        compute_continuous_tree_decision(ss, theta, alpha_value, current_dose, max_increment_std,
                                         deltastop = deltastop, assigned_doses = assigned_doses,
                                         efficiency = efficiency)
      },
      alpha_increment_cutoff = alpha_increment_cutoff,
      progress_state = progress_state,
      progress = progress,
      label_for = label_for,
      title_text = paste("Continuous Dose Escalation Decision Tree: Cohort Size", cohort_selected),
      panel_title = "Continuous dose escalation tree",
      subtitle_text = paste0(
        "Continuous recommendations start at the minimum dose and are constrained by the ",
        tree_inputs$max_dose_increment, "% maximum dose increment",
        ".",
        if (!is.null(efficiency)) {
          paste0("\nEFF STOP: efficiency stopping rule (", format_dose_value(efficiency$max_patients),
                 " patients already treated within the dose-region width of the recommended dose).")
        } else {
          ""
        }
      ),
      root_label = label_for(0, NA_integer_)
    )
    continuous_sc$num_doses <- NA_integer_
    continuous_sc$min_dose <- min_dose
    continuous_sc$max_dose <- max_dose
    continuous_sc$dose_unit <- dose_unit
    continuous_sc$dose_range_std <- c(0, 1)
    continuous_sc$x_labels <- build_adaptive_tree_x_labels(
      max_depth = standard_max_depth,
      first_label = "Cohort 1\n(Minimum Dose)",
      alpha_labels = continuous_sc$alpha_labels
    )
    return(list(
      result = list(primary = list(panel_title = continuous_sc$panel_title, sc = continuous_sc), additional = list()),
      max_depth = standard_max_depth,
      max_cohorts = max_cohorts
    ))
  }

  # Discrete: level-based positions 0, 1/(K-1), ..., 1 (as in the simulations).
  num_doses <- as.integer(tree_inputs$num_doses)
  doselevels <- seq(0, 1, length.out = num_doses)
  incr <- 1 / (num_doses - 1)
  units <- if (length(tree_inputs$actual_dose_units) == num_doses) tree_inputs$actual_dose_units else rep(if (length(tree_inputs$dose_unit)) as.character(tree_inputs$dose_unit[1]) else "", num_doses)
  actual_dose_levels <- suppressWarnings(as.numeric(tree_inputs$actual_dose_levels))
  if (length(actual_dose_levels) != num_doses) actual_dose_levels <- rep(NA_real_, num_doses)
  dose_labels <- build_discrete_dose_labels(actual_dose_levels, units, num_doses)
  efficiency <- tree_efficiency_settings(tree_inputs, width = 0)
  decide <- function(ss, alpha_value, current_dose, assigned_doses) {
    compute_tree_decision(ss, doselevels, theta, alpha_value, current_dose, assigned_doses, incr,
                          deltastop = deltastop, efficiency = efficiency)
  }
  trial_mode <- tree_inputs$trial_mode
  startdose <- as.integer(tree_inputs$startdose)
  hist_X0 <- numeric(0)
  hist_Y0 <- integer(0)
  root_is_ewoc <- FALSE
  root_stop <- NULL

  if (identical(trial_mode, "Accelerated EWOC")) {
    # The tree starts from the EWOC decision after the lead-in: one patient at each dose level from the
    # starting dose to the maximum accelerated dose, none with a DLT. The lead-in itself is not drawn.
    cap_level <- as.integer(tree_inputs$max_accel_dose_level)
    leadin_idx <- seq.int(startdose, cap_level)
    hist_X0 <- doselevels[leadin_idx]
    hist_Y0 <- rep(0L, length(hist_X0))
    ss <- fit_tree_branch_result(model_string, hist_X0, hist_Y0, opt1, theta, burn, mm_val)
    first <- decide(ss, alpha[1], tail(hist_X0, 1L), hist_X0)
    if (isTRUE(first$stop_trial)) {
      root_stop <- first
      startdose <- tail(leadin_idx, 1L)
    } else {
      startdose <- first$next_dose_idx
      root_is_ewoc <- TRUE
    }
  }

  standard_sc <- build_ewoc_tree_structure(
    dose_type = "Discrete",
    root_dose_value = doselevels[startdose],
    root_dose_idx = startdose,
    cohort_size = cohort_selected,
    max_depth = if (is.null(root_stop)) standard_max_depth else 0L,
    theta = theta, burn = burn, mm_val = mm_val, alpha = alpha, opt1 = opt1, model_string = model_string,
    decide = decide,
    hist_X0 = hist_X0,
    hist_Y0 = hist_Y0,
    root_is_ewoc = root_is_ewoc,
    alpha_increment_cutoff = alpha_increment_cutoff,
    progress_state = progress_state,
    progress = progress,
    title_text = if (identical(trial_mode, "Accelerated EWOC")) {
      paste("Dose Escalation Decision Tree After the Accelerated Lead-in: Cohort Size", cohort_selected)
    } else {
      paste("Dose Escalation Decision Tree: Cohort Size", cohort_selected)
    },
    panel_title = if (identical(trial_mode, "Accelerated EWOC")) "Dose escalation tree after the accelerated lead-in" else "Dose escalation tree",
    subtitle_text = {
      notes <- c(
        if (identical(trial_mode, "Accelerated EWOC")) {
          paste0(
            "Starts from the EWOC decision after the lead-in reached ", dose_labels[cap_level],
            " (maximum accelerated dose) with no DLT.\nLead-in patients (one per dose level) are included in the model but not drawn."
          )
        },
        if (!is.null(efficiency)) {
          paste0("EFF STOP: efficiency stopping rule (", format_dose_value(efficiency$max_patients),
                 " patients already treated at the recommended dose).")
        }
      )
      if (length(notes)) {
        paste0(paste(notes, collapse = "\n"), "\nPrior: \u03C1\u2081 ~ Beta(", opt1$a0, ", ", opt1$b0, ") | ",
               "\u03C1\u2080/\u03C1\u2081 ~ Beta(", opt1$a1, ", ", opt1$b1, ")")
      } else {
        NULL
      }
    },
    root_label = dose_labels[startdose]
  )
  if (!is.null(root_stop)) {
    standard_sc$nodes$is_stop[1] <- TRUE
    standard_sc$nodes$stop_type[1] <- root_stop$stop_type
  }
  standard_sc$num_doses <- length(doselevels)
  standard_sc$dose_labels <- dose_labels
  standard_sc$actual_dose_levels <- actual_dose_levels
  standard_sc$dose_unit <- tree_inputs$dose_unit
  standard_sc$x_labels <- build_adaptive_tree_x_labels(
    max_depth = standard_sc$max_depth,
    first_label = if (identical(trial_mode, "Accelerated EWOC")) "EWOC Start\n(after lead-in)" else "Cohort 1\n(Starting Dose)",
    adaptive_prefix = if (identical(trial_mode, "Accelerated EWOC")) "EWOC Cohort" else "Cohort",
    alpha_labels = standard_sc$alpha_labels
  )

  list(
    result = list(primary = list(panel_title = standard_sc$panel_title, sc = standard_sc), additional = list()),
    max_depth = standard_max_depth,
    max_cohorts = max_cohorts
  )
}

build_tree_plot <- function(sc, cohort_size, opt1) {
  req(!is.null(sc), !is.null(opt1))
  req(nrow(sc$nodes) > 0)

  nodes <- sc$nodes
  edges <- if (is.null(sc$edges) || nrow(sc$edges) == 0) {
    data.frame()
  } else {
    sc$edges
  }

  nodes$is_stop <- as.logical(nodes$is_stop)
  num_doses <- sc$num_doses
  dose_type <- if (!is.null(sc$dose_type)) sc$dose_type else "Discrete"
  max_depth <- sc$max_depth
  branch_labels <- sc$branch_labels
  branch_legend_labels <- if (!is.null(sc$branch_legend_labels)) {
    sc$branch_legend_labels
  } else {
    branch_labels_to_dlt_counts(branch_labels)
  }
  axis_prefix <- if (!is.null(sc$axis_prefix)) {
    sc$axis_prefix
  } else if (cohort_size == 1) {
    "Patient"
  } else {
    "Cohort"
  }
  subtitle_text <- if (!is.null(sc$subtitle_text)) {
    sc$subtitle_text
  } else {
    paste0(
      "Prior: \u03C1\u2081 ~ Beta(", opt1$a0, ", ", opt1$b0, ") | ",
      "\u03C1\u2080/\u03C1\u2081 ~ Beta(", opt1$a1, ", ", opt1$b1, ")"
    )
  }

  edge_df <- data.frame()
  if (nrow(edges) > 0 && nrow(nodes) > 0) {
    edge_df <- tryCatch({
      ed_temp <- merge(
        edges,
        nodes[, c("id", "depth", "y_pos", "dose_idx")],
        by.x = "from_id", by.y = "id", all.x = FALSE
      )
      names(ed_temp)[names(ed_temp) %in% c("depth", "y_pos", "dose_idx")] <-
        c("from_depth", "from_y", "from_dose")

      ed_temp <- merge(
        ed_temp,
        nodes[, c("id", "depth", "y_pos", "dose_idx")],
        by.x = "to_id", by.y = "id", all.x = FALSE
      )
      names(ed_temp)[names(ed_temp) %in% c("depth", "y_pos", "dose_idx")] <-
        c("to_depth", "to_y", "to_dose")
      ed_temp
    }, error = function(e) {
      data.frame()
    })
  }

  if (nrow(edge_df) > 0) {
    edge_df$branch_label <- factor(edge_df$branch_label, levels = branch_labels)
    edge_df$branch_label_display <- unname(branch_legend_labels[as.character(edge_df$branch_label)])
    edge_df$mid_x <- (edge_df$from_depth + edge_df$to_depth) / 2 + 1
    edge_df$mid_y <- (edge_df$from_y + edge_df$to_y) / 2
  }

  if (identical(dose_type, "Continuous")) {
    dose_labels <- character(0)
  } else if (!is.null(sc$dose_labels) && length(sc$dose_labels) == num_doses) {
    # Labels of the dose levels, with per-level units.
    dose_labels <- sc$dose_labels
  } else if (!is.null(sc$actual_dose_levels) && length(sc$actual_dose_levels) == num_doses) {
    dose_values <- suppressWarnings(as.numeric(sc$actual_dose_levels))
    if (all(is.finite(dose_values))) {
      dose_labels <- format(round(dose_values, 4), trim = TRUE, scientific = FALSE)
      if (!is.null(sc$dose_unit) && nzchar(trimws(as.character(sc$dose_unit)))) {
        dose_labels <- paste(dose_labels, trimws(as.character(sc$dose_unit)))
      }
    } else {
      dose_labels <- paste("Dose", seq_len(num_doses))
    }
  } else {
    dose_labels <- paste("Dose", seq_len(num_doses))
  }
  dose_pal <- if (identical(dose_type, "Continuous")) c(`Stop Trial` = "#e79aa3") else c(
    setNames(
      colorRampPalette(c("#bcdff1", "#bfe3c7", "#ffe39a", "#efb6bd"))(num_doses),
      dose_labels
    ),
    `Stop Trial` = "#e79aa3"
  )
  branch_pal <- setNames(
    colorRampPalette(c("darkgreen", "#f1c40f", "#e67e22", "red3"))(length(branch_labels)),
    branch_labels
  )

  node_size <- 1.7 * max(4.5, 15 - max_depth * 1.4)
  label_size <- 1.3 * max(2.1, 4 - max_depth * 0.38)
  edge_label_size <- max(2.8, 4.0 - max_depth * 0.20)
  arrow_len <- unit(max(0.09, 0.26 - max_depth * 0.03), "cm")
  if (identical(dose_type, "Continuous")) {
    node_size <- node_size * 1.25
    label_size <- label_size * 0.95
  }

  active_nodes <- nodes[is.na(nodes$is_stop) | !nodes$is_stop, , drop = FALSE]
  stop_nodes <- nodes[!is.na(nodes$is_stop) & nodes$is_stop, , drop = FALSE]

  active_nodes$dose_legend <- if ("dose_legend" %in% names(active_nodes)) {
    active_nodes$dose_legend
  } else if (nrow(active_nodes) > 0) {
    dose_labels[active_nodes$dose_idx]
  } else {
    character(0)
  }
  active_nodes$label <- if ("custom_label" %in% names(active_nodes) && nrow(active_nodes) > 0) {
    ifelse(
      is.na(active_nodes$custom_label) | !nzchar(active_nodes$custom_label),
      dose_labels[active_nodes$dose_idx],
      active_nodes$custom_label
    )
  } else if (nrow(active_nodes) > 0) {
    dose_labels[active_nodes$dose_idx]
  } else {
    character(0)
  }

  if (identical(dose_type, "Continuous")) {
    # Continuous trees label each node with its actual dose.
    active_nodes$dose_value <- as.numeric(active_nodes$dose_value)
    active_nodes$label <- if (nrow(active_nodes) > 0) {
      ifelse(
        is.na(active_nodes$custom_label) | !nzchar(active_nodes$custom_label),
        continuous_tree_dose_label(active_nodes$dose_value, sc$min_dose, sc$max_dose, sc$dose_unit),
        active_nodes$custom_label
      )
    } else {
      character(0)
    }
  }
  stop_nodes$dose_legend <- rep("Stop Trial", nrow(stop_nodes))
  stop_nodes$label <- if ("custom_label" %in% names(stop_nodes) && nrow(stop_nodes) > 0) {
    ifelse(
      is.na(stop_nodes$custom_label) | !nzchar(stop_nodes$custom_label),
      "STOP\nTRIAL",
      stop_nodes$custom_label
    )
  } else {
    rep("STOP\nTRIAL", nrow(stop_nodes))
  }

  legend_breaks <- unique(c(
    dose_labels[dose_labels %in% unique(active_nodes$dose_legend)],
    if (nrow(stop_nodes) > 0) "Stop Trial"
  ))
  if (identical(dose_type, "Continuous")) legend_breaks <- character(0)

  x_labels <- if (!is.null(sc$x_labels)) {
    sc$x_labels
  } else {
    sapply(seq_len(max_depth + 1L), function(i) {
      if (i == 1) {
        paste0(axis_prefix, " ", i, "\n(Starting Dose)")
      } else {
        paste0(axis_prefix, " ", i, "\n(\u03B1=", round(sc$alpha_values[i], 3), ")")
      }
    })
  }
  if (any(grepl("adaptive", x_labels, ignore.case = TRUE))) {
    replacement_labels <- build_adaptive_tree_x_labels(
      max_depth = max_depth,
      first_label = x_labels[1],
      adaptive_prefix = axis_prefix,
      alpha_labels = sc$alpha_labels
    )
    x_labels <- ifelse(
      grepl("adaptive", x_labels, ignore.case = TRUE),
      replacement_labels,
      x_labels
    )
  }

  if (identical(dose_type, "Continuous")) {
    dose_limits <- if (length(sc$dose_range_std) == 2L) range(c(0, 1, sc$dose_range_std)) else c(0, 1)
    dose_breaks <- sort(unique(c(dose_limits, 0.5)))
    dose_break_labels <- vapply(dose_breaks, function(x) {
      actual_dose <- continuous_tree_actual_dose(x, sc$min_dose, sc$max_dose)
      label <- format(round(actual_dose, 4), trim = TRUE, scientific = FALSE)
      if (!is.null(sc$dose_unit) && nzchar(trimws(as.character(sc$dose_unit)))) {
        label <- paste(label, trimws(as.character(sc$dose_unit)))
      }
      label
    }, character(1))

    return(
      ggplot() +
        geom_segment(
          data = edge_df,
          aes(
            x = from_depth + 1, xend = to_depth + 1,
            y = from_y, yend = to_y,
            color = branch_label
          ),
          linewidth = 0.9,
          arrow = arrow(length = arrow_len, type = "closed")
        ) +
        geom_label(
          data = edge_df,
          aes(x = mid_x, y = mid_y, label = branch_label_display, color = branch_label),
          size = edge_label_size,
          alpha = 0.9,
          label.padding = unit(0.08, "lines"),
          linewidth = 0.18,
          show.legend = FALSE
        ) +
        geom_point(
          data = active_nodes,
          aes(x = depth + 1, y = y_pos, fill = dose_value),
          shape = 21,
          size = node_size,
          color = "grey20",
          stroke = 0.7
        ) +
        geom_point(
          data = stop_nodes,
          aes(x = depth + 1, y = y_pos),
          fill = "#e79aa3",
          shape = 22,
          size = node_size,
          color = "red3",
          stroke = 0.9
        ) +
        geom_text(
          data = active_nodes,
          aes(x = depth + 1, y = y_pos, label = label),
          size = label_size,
          fontface = "bold",
          color = "black",
          lineheight = 0.85
        ) +
        geom_text(
          data = stop_nodes,
          aes(x = depth + 1, y = y_pos, label = label),
          size = label_size,
          fontface = "bold",
          color = "red4",
          lineheight = 0.85
        ) +
        scale_color_manual(
          values = branch_pal,
          name = "Observed DLT pattern",
          labels = unname(branch_legend_labels[branch_labels]),
          drop = FALSE,
          guide = guide_legend(order = 2)
        ) +
        scale_fill_gradientn(
          colors = c("#2f80ed", "#19a974", "#f2c94c", "#d73027"),
          limits = dose_limits,
          breaks = dose_breaks,
          labels = dose_break_labels,
          name = "Recommended dose",
          guide = guide_colorbar(order = 1, barheight = unit(3.8, "cm"), barwidth = unit(0.35, "cm"))
        ) +
        scale_x_continuous(
          breaks = seq_len(max_depth + 1L),
          labels = x_labels,
          expand = expansion(mult = 0.09)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.06, 0.36))) +
        labs(
          title = if (!is.null(sc$title_text)) sc$title_text else paste("Continuous Dose Escalation Decision Tree: Cohort Size", cohort_size),
          subtitle = subtitle_text,
          x = NULL, y = NULL
        ) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 13, margin = margin(b = 7)),
          plot.subtitle = element_text(hjust = 0.5, size = 10.5, color = "grey15", margin = margin(b = 12)),
          plot.margin = margin(14, 14, 12, 14),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_text(size = 10, face = "bold"),
          legend.position = c(0.01, 0.99),
          legend.justification = c(0, 1),
          legend.direction = "vertical",
          legend.background = element_rect(fill = scales::alpha("white", 0.96), color = "grey70"),
          legend.key = element_rect(fill = scales::alpha("white", 0.96), color = NA),
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 9),
          legend.box = "vertical",
          legend.spacing.y = unit(0.12, "cm"),
          panel.grid.major.x = element_blank()
        )
    )
  }

  ggplot() +
    geom_segment(
      data = edge_df,
      aes(
        x = from_depth + 1, xend = to_depth + 1,
        y = from_y, yend = to_y,
        color = branch_label
      ),
      linewidth = 0.9,
      arrow = arrow(length = arrow_len, type = "closed")
    ) +
    geom_label(
      data = edge_df,
      aes(x = mid_x, y = mid_y, label = branch_label_display, color = branch_label),
      size = edge_label_size,
      alpha = 0.9,
      label.padding = unit(0.08, "lines"),
      linewidth = 0.18,
      show.legend = FALSE
    ) +
    geom_point(
      data = active_nodes,
      aes(x = depth + 1, y = y_pos, fill = dose_legend),
      shape = 21,
      size = node_size,
      color = "grey20",
      stroke = 0.7
    ) +
    geom_point(
      data = stop_nodes,
      aes(x = depth + 1, y = y_pos, fill = dose_legend),
      shape = 22,
      size = node_size,
      color = "red3",
      stroke = 0.9
    ) +
    geom_text(
      data = active_nodes,
      aes(x = depth + 1, y = y_pos, label = label),
      size = label_size,
      fontface = "bold",
      color = "black",
      lineheight = 0.85
    ) +
    geom_text(
      data = stop_nodes,
      aes(x = depth + 1, y = y_pos, label = label),
      size = label_size,
      fontface = "bold",
      color = "red4",
      lineheight = 0.85
    ) +
    scale_color_manual(
      values = branch_pal,
      name = "Observed DLT pattern",
      labels = unname(branch_legend_labels[branch_labels]),
      drop = FALSE,
      guide = guide_legend(order = 2)
    ) +
    scale_fill_manual(
      values = dose_pal,
      name = "Assigned dose",
      breaks = legend_breaks,
      guide = guide_legend(
        order = 1,
        override.aes = list(
          shape = ifelse(legend_breaks == "Stop Trial", 22, 21),
          fill = unname(dose_pal[legend_breaks]),
          color = ifelse(legend_breaks == "Stop Trial", "red3", "grey20"),
          stroke = ifelse(legend_breaks == "Stop Trial", 0.9, 0.7),
          size = rep(5, length(legend_breaks))
        )
      )
    ) +
    scale_x_continuous(
      breaks = seq_len(max_depth + 1L),
      labels = x_labels,
      expand = expansion(mult = 0.09)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
    labs(
      title = if (!is.null(sc$title_text)) sc$title_text else paste("Dose Escalation Decision Tree: Cohort Size", cohort_size),
      subtitle = subtitle_text,
      x = NULL, y = NULL
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
      plot.subtitle = element_text(hjust = 0.5, size = 10.5, color = "grey15"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.text.x = element_text(size = 10, face = "bold"),
      legend.position = c(0.02, 0.94),
      legend.justification = c(0, 1),
      legend.direction = "vertical",
      legend.background = element_rect(fill = scales::alpha("white", 0.9), color = "grey70"),
      legend.key = element_rect(fill = scales::alpha("white", 0.9), color = NA),
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      legend.box = "vertical",
      legend.spacing.y = unit(0.18, "cm"),
      panel.grid.major.x = element_line(color = "grey90", linetype = "dotted")
    )
}
