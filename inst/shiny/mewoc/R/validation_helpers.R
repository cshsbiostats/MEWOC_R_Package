MAX_SUPPORTED_TREE_COHORT_SIZE <- 8L

show_validation_error <- function(title, message) {
  showModal(modalDialog(title = title, message, easyClose = TRUE))
  NULL
}

validate_scalar_input <- function(value, title, message,
                                  integer_required = FALSE,
                                  min_value = -Inf,
                                  max_value = Inf,
                                  lower_inclusive = TRUE,
                                  upper_inclusive = TRUE) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.finite(numeric_value)) {
    return(show_validation_error(title, message))
  }
  if (isTRUE(integer_required) && numeric_value != floor(numeric_value)) {
    return(show_validation_error(title, message))
  }
  
  lower_ok <- if (is.infinite(min_value)) {
    TRUE
  } else if (isTRUE(lower_inclusive)) {
    numeric_value >= min_value
  } else {
    numeric_value > min_value
  }
  
  upper_ok <- if (is.infinite(max_value)) {
    TRUE
  } else if (isTRUE(upper_inclusive)) {
    numeric_value <= max_value
  } else {
    numeric_value < max_value
  }
  
  if (!lower_ok || !upper_ok) {
    return(show_validation_error(title, message))
  }
  
  if (isTRUE(integer_required)) as.integer(numeric_value) else numeric_value
}

validate_addon_inputs <- function(input, include_seed = FALSE, include_mcmc = FALSE) {
  validated <- list()
  
  if (isTRUE(include_seed)) {
    seed_value <- validate_scalar_input(
      input$seed,
      title = "Invalid seed",
      message = "Seed must be a whole number greater than or equal to 1.",
      integer_required = TRUE,
      min_value = 1
    )
    if (is.null(seed_value)) {
      return(NULL)
    }
    validated$seed <- seed_value
  }
  
  if (isTRUE(include_mcmc)) {
    burn_value <- validate_scalar_input(
      input$burn,
      title = "Invalid burn-in",
      message = "Burn-in must be a whole number greater than or equal to 0.",
      integer_required = TRUE,
      min_value = 0
    )
    if (is.null(burn_value)) {
      return(NULL)
    }
    
    mm_value <- validate_scalar_input(
      input$mm,
      title = "Invalid samples",
      message = "Samples must be a whole number greater than or equal to 1.",
      integer_required = TRUE,
      min_value = 1
    )
    if (is.null(mm_value)) {
      return(NULL)
    }
    
    validated$burn <- burn_value
    validated$mm <- mm_value
  }
  
  validated
}

validate_common_design_inputs <- function(input, context = c("trial", "tree")) {
  context <- match.arg(context)
  
  dose_type <- trimws(as.character(input$prior_dose_type))
  if (length(dose_type) != 1L || !dose_type %in% c("Discrete", "Continuous")) {
    dose_type <- "Discrete"
  }
  
  if (identical(dose_type, "Continuous")) {
    min_dose <- validate_scalar_input(
      input$min_dose,
      title = "Invalid minimum dose",
      message = "Minimum dose must be numeric and greater than or equal to 0.",
      min_value = 0
    )
    if (is.null(min_dose)) return(NULL)
    
    max_dose <- validate_scalar_input(
      input$max_dose,
      title = "Invalid maximum dose",
      message = "Maximum dose must be numeric and greater than the minimum dose.",
      min_value = 0
    )
    if (is.null(max_dose)) return(NULL)
    if (max_dose <= min_dose) {
      return(show_validation_error(
        "Invalid continuous dose range",
        "Maximum dose must be greater than minimum dose."
      ))
    }
    
    dose_unit <- trimws(as.character(input$dose_unit))
    if (!nzchar(dose_unit)) {
      return(show_validation_error(
        "Missing dose unit",
        "Please enter the unit for the continuous dose values."
      ))
    }
    
    total_n <- validate_scalar_input(
      input$NN,
      title = "Invalid sample size",
      message = "Total sample size must be a whole number greater than or equal to 1.",
      integer_required = TRUE,
      min_value = 1
    )
    if (is.null(total_n)) return(NULL)
    
    cohort <- validate_scalar_input(
      input$cohort,
      title = "Invalid cohort size",
      message = if (identical(context, "tree")) {
        paste0("Cohort size must be an integer between 1 and ", MAX_SUPPORTED_TREE_COHORT_SIZE, ".")
      } else {
        "Cohort size must be a whole number greater than or equal to 1."
      },
      integer_required = TRUE,
      min_value = 1,
      max_value = if (identical(context, "tree")) MAX_SUPPORTED_TREE_COHORT_SIZE else Inf
    )
    if (is.null(cohort)) return(NULL)
    
    if (total_n < cohort) {
      return(show_validation_error(
        "Invalid sample size",
        if (identical(context, "tree")) {
          "Total sample size must be at least as large as the cohort size."
        } else {
          "Total sample size must be greater than or equal to the cohort size."
        }
      ))
    }
    
    if ((total_n %% cohort) != 0L) {
      return(show_validation_error(
        "Incompatible sample and cohort sizes",
        if (identical(context, "tree")) {
          "Total sample size must be divisible by the cohort size to build the escalation tree."
        } else {
          "Total sample size must be divisible by the cohort size."
        }
      ))
    }
    
    theta <- validate_scalar_input(
      input$theta,
      title = "Invalid target DLT probability",
      message = "Target probability of DLT (theta) must be numeric and strictly between 0 and 1.",
      min_value = 0,
      max_value = 1,
      lower_inclusive = FALSE,
      upper_inclusive = FALSE
    )
    if (is.null(theta)) return(NULL)
    
    lower_f_bound <- validate_scalar_input(
      input$lower_f_bound,
      title = "Invalid feasibility bound",
      message = if (identical(context, "tree")) {
        "Lower feasibility bound must be between 0 and 0.5 for the tree builder."
      } else {
        "Lower feasibility bound must be between 0 and 0.5."
      },
      min_value = 0,
      max_value = 0.5
    )
    if (is.null(lower_f_bound)) return(NULL)
    
    increment <- validate_scalar_input(
      input$increment,
      title = "Invalid increment",
      message = if (identical(context, "tree")) {
        "Feasibility bound increment must be greater than 0."
      } else {
        "Feasibility bound increment must be numeric and > 0."
      },
      min_value = 0,
      lower_inclusive = FALSE
    )
    if (is.null(increment)) return(NULL)
    
    deltastop <- validate_scalar_input(
      input$deltastop,
      title = "Invalid delta threshold",
      message = "Threshold, Delta, must be numeric and strictly between 0 and 1.",
      min_value = 0,
      max_value = 1,
      lower_inclusive = FALSE,
      upper_inclusive = FALSE
    )
    if (is.null(deltastop)) return(NULL)
    
    max_dose_increment <- validate_scalar_input(
      input$max_dose_increment,
      title = "Invalid maximum dose increment",
      message = "Maximum dose increment percentage must be numeric and greater than 0.",
      min_value = 0,
      max_value = 100,
      lower_inclusive = FALSE
    )
    if (is.null(max_dose_increment)) return(NULL)
    
    return(list(
      dose_type = dose_type,
      num_doses = NA_integer_,
      startdose = NA_integer_,
      NN = total_n,
      cohort = cohort,
      theta = theta,
      lower_f_bound = lower_f_bound,
      increment = increment,
      deltastop = deltastop,
      min_dose = min_dose,
      max_dose = max_dose,
      max_dose_increment = max_dose_increment,
      actual_dose_levels = numeric(0),
      actual_dose_units = character(0),
      dose_unit = dose_unit
    ))
  }
  
  if (identical(context, "tree")) {
    num_doses <- validate_scalar_input(
      input$num_doses,
      title = "Need at least two dose levels",
      message = "Please use at least two dose levels to build the escalation trees.",
      integer_required = TRUE,
      min_value = 2
    )
  } else {
    num_doses <- validate_scalar_input(
      input$num_doses,
      title = "Invalid number of dose levels",
      message = "Number of dose levels must be a whole number greater than or equal to 2.",
      integer_required = TRUE,
      min_value = 2
    )
  }
  if (is.null(num_doses)) {
    return(NULL)
  }
  
  startdose <- validate_scalar_input(
    input$startdose,
    title = "Invalid starting dose",
    message = if (identical(context, "tree")) {
      "Starting dose level must be between 1 and one dose below the highest dose level."
    } else {
      "Starting dose level must be a whole number between 1 and one dose below the highest dose level."
    },
    integer_required = TRUE,
    min_value = 1,
    max_value = num_doses - 1L
  )
  if (is.null(startdose)) {
    return(NULL)
  }
  
  total_n <- validate_scalar_input(
    input$NN,
    title = "Invalid sample size",
    message = "Total sample size must be a whole number greater than or equal to 1.",
    integer_required = TRUE,
    min_value = 1
  )
  if (is.null(total_n)) {
    return(NULL)
  }
  
  cohort <- validate_scalar_input(
    input$cohort,
    title = "Invalid cohort size",
    message = if (identical(context, "tree")) {
      paste0("Cohort size must be an integer between 1 and ", MAX_SUPPORTED_TREE_COHORT_SIZE, ".")
    } else {
      "Cohort size must be a whole number greater than or equal to 1."
    },
    integer_required = TRUE,
    min_value = 1,
    max_value = if (identical(context, "tree")) MAX_SUPPORTED_TREE_COHORT_SIZE else Inf
  )
  if (is.null(cohort)) {
    return(NULL)
  }
  
  if (total_n < cohort) {
    return(show_validation_error(
      "Invalid sample size",
      if (identical(context, "tree")) {
        "Total sample size must be at least as large as the cohort size."
      } else {
        "Total sample size must be greater than or equal to the cohort size."
      }
    ))
  }
  
  if ((total_n %% cohort) != 0L) {
    return(show_validation_error(
      "Incompatible sample and cohort sizes",
      if (identical(context, "tree")) {
        "Total sample size must be divisible by the cohort size to build the escalation tree."
      } else {
        "Total sample size must be divisible by the cohort size."
      }
    ))
  }
  
  theta <- validate_scalar_input(
    input$theta,
    title = "Invalid target DLT probability",
    message = "Target probability of DLT (theta) must be numeric and strictly between 0 and 1.",
    min_value = 0,
    max_value = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )
  if (is.null(theta)) {
    return(NULL)
  }
  
  lower_f_bound <- validate_scalar_input(
    input$lower_f_bound,
    title = "Invalid feasibility bound",
    message = if (identical(context, "tree")) {
      "Lower feasibility bound must be between 0 and 0.5 for the tree builder."
    } else {
      "Lower feasibility bound must be between 0 and 0.5."
    },
    min_value = 0,
    max_value = 0.5
  )
  if (is.null(lower_f_bound)) {
    return(NULL)
  }
  
  increment <- validate_scalar_input(
    input$increment,
    title = "Invalid increment",
    message = if (identical(context, "tree")) {
      "Feasibility bound increment must be greater than 0."
    } else {
      "Feasibility bound increment must be numeric and > 0."
    },
    min_value = 0,
    lower_inclusive = FALSE
  )
  if (is.null(increment)) {
    return(NULL)
  }
  
  deltastop <- validate_scalar_input(
    input$deltastop,
    title = "Invalid delta threshold",
    message = "Threshold, Delta, must be numeric and strictly between 0 and 1.",
    min_value = 0,
    max_value = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )
  if (is.null(deltastop)) {
    return(NULL)
  }
  
  actual_dose_levels <- vapply(seq_len(num_doses), function(i) {
    suppressWarnings(as.numeric(input[[paste0("actual_dose_", i)]]))
  }, numeric(1))
  
  actual_dose_units <- vapply(seq_len(num_doses), function(i) {
    unit_value <- trimws(as.character(input[[paste0("actual_dose_unit_", i)]]))
    if (length(unit_value) != 1L || is.na(unit_value)) "" else unit_value
  }, character(1))
  
  if (any(!nzchar(actual_dose_units))) {
    return(show_validation_error(
      "Missing dose unit",
      "Please enter a unit for each discrete dose level."
    ))
  }
  
  shared_dose_units <- unique(actual_dose_units)
  dose_unit <- if (length(shared_dose_units) == 1L) shared_dose_units else ""
  
  if (any(!is.finite(actual_dose_levels))) {
    return(show_validation_error(
      "Invalid actual dose values",
      "Please enter a numeric actual dose value for each discrete dose level."
    ))
  }
  
  if (any(actual_dose_levels < 0)) {
    return(show_validation_error(
      "Invalid actual dose values",
      "Actual dose values must be greater than or equal to 0."
    ))
  }
  
  # Dose levels are ordered steps (listed from lowest to highest expected toxicity); their values are
  # labels and may use different units or describe combinations, so they need not increase. They must
  # be distinct because Trial Conduct identifies the dose given by its value.
  if (anyDuplicated(actual_dose_levels) > 0L) {
    return(show_validation_error(
      "Invalid actual dose values",
      "Each dose level needs a different actual dose value (Trial Conduct identifies the dose given by its value)."
    ))
  }
  
  list(
    dose_type = dose_type,
    num_doses = num_doses,
    startdose = startdose,
    NN = total_n,
    cohort = cohort,
    theta = theta,
    lower_f_bound = lower_f_bound,
    increment = increment,
    deltastop = deltastop,
    actual_dose_levels = actual_dose_levels,
    actual_dose_units = actual_dose_units,
    dose_unit = dose_unit
  )
}

validate_prior_inputs <- function(input, clear_prior_results_fn = NULL) {
  clear_prior_results <- function() {
    if (is.function(clear_prior_results_fn)) {
      clear_prior_results_fn()
    }
  }
  
  dose_type <- trimws(as.character(input$prior_dose_type))
  if (length(dose_type) != 1L || !dose_type %in% c("Discrete", "Continuous")) {
    dose_type <- "Discrete"
  }
  
  theta_input <- validate_scalar_input(
    input$theta,
    title = "Invalid target DLT probability",
    message = "Target probability of DLT (theta) must be numeric and strictly between 0 and 1.",
    min_value = 0,
    max_value = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )
  if (is.null(theta_input)) {
    clear_prior_results()
    return(NULL)
  }
  
  num_doses_input <- 2L
  min_dose_input <- NA_real_
  max_dose_input <- NA_real_
  dose_unit_input <- ""
  
  if (identical(dose_type, "Discrete")) {
    num_doses_input <- validate_scalar_input(
      input$num_doses,
      title = "Invalid number of dose levels",
      message = "Number of dose levels must be a whole number between 2 and 12.",
      integer_required = TRUE,
      min_value = 2,
      max_value = 12
    )
    if (is.null(num_doses_input)) {
      clear_prior_results()
      return(NULL)
    }
    dose_units_input <- vapply(seq_len(num_doses_input), function(i) {
      unit_value <- trimws(as.character(input[[paste0("actual_dose_unit_", i)]]))
      if (length(unit_value) != 1L || is.na(unit_value)) "" else unit_value
    }, character(1))
    if (any(!nzchar(dose_units_input))) {
      clear_prior_results()
      show_validation_error(
        "Missing dose unit",
        "Please enter a unit for each discrete dose level."
      )
      return(NULL)
    }
    shared_dose_units <- unique(dose_units_input)
    dose_unit_input <- if (length(shared_dose_units) == 1L) shared_dose_units else ""
  } else {
    dose_unit_input <- trimws(as.character(input$dose_unit))
    if (!nzchar(dose_unit_input)) {
      clear_prior_results()
      show_validation_error(
        "Missing dose unit",
        "Please enter the unit for the continuous dose values."
      )
      return(NULL)
    }
    
    min_dose_input <- validate_scalar_input(
      input$min_dose,
      title = "Invalid minimum dose",
      message = "Minimum dose must be numeric.",
      min_value = 0
    )
    if (is.null(min_dose_input)) {
      clear_prior_results()
      return(NULL)
    }
    
    max_dose_input <- validate_scalar_input(
      input$max_dose,
      title = "Invalid maximum dose",
      message = "Maximum dose must be numeric.",
      min_value = 0
    )
    if (is.null(max_dose_input)) {
      clear_prior_results()
      return(NULL)
    }
    
    if (max_dose_input <= min_dose_input) {
      clear_prior_results()
      show_validation_error(
        "Invalid continuous dose range",
        "Maximum dose must be greater than minimum dose."
      )
      return(NULL)
    }
    
  }
  
  p0_input <- validate_scalar_input(
    input$p0,
    title = "Invalid P(MTD < Min Dose)",
    message = "P(MTD < Min Dose) must be numeric and between 0 and 1.",
    min_value = 0,
    max_value = 1
  )
  if (is.null(p0_input)) {
    clear_prior_results()
    return(NULL)
  }
  
  p1_input <- validate_scalar_input(
    input$p1,
    title = "Invalid P(MTD > Max Dose)",
    message = "P(MTD > Max Dose) must be numeric and between 0 and 1.",
    min_value = 0,
    max_value = 1
  )
  if (is.null(p1_input)) {
    clear_prior_results()
    return(NULL)
  }
  
  if ((p0_input + p1_input) > 1) {
    clear_prior_results()
    show_validation_error(
      "Incompatible tail probabilities",
      "P(MTD < Min Dose) and P(MTD > Max Dose) cannot sum to more than 1."
    )
    return(NULL)
  }
  
  condition_input <- validate_scalar_input(
    input$condition,
    title = "Invalid prior parameter condition",
    message = "Prior parameter condition must be numeric and between 1 and 2.",
    min_value = 1,
    max_value = 2
  )
  if (is.null(condition_input)) {
    clear_prior_results()
    return(NULL)
  }
  
  addon_inputs <- validate_addon_inputs(input, include_seed = TRUE)
  if (is.null(addon_inputs)) {
    clear_prior_results()
    return(NULL)
  }
  
  list(
    dose_type = dose_type,
    theta = theta_input,
    num_doses = num_doses_input,
    min_dose = min_dose_input,
    max_dose = max_dose_input,
    dose_unit = dose_unit_input,
    p0 = p0_input,
    p1 = p1_input,
    condition = condition_input,
    seed = addon_inputs$seed
  )
}

validate_dose_probabilities <- function(input, num_doses) {
  doseprobs <- vapply(seq_len(num_doses), function(i) {
    value <- suppressWarnings(as.numeric(input[[paste0("dose_", i)]]))
    if (is.na(value)) NA_real_ else value
  }, numeric(1))
  
  if (any(is.na(doseprobs))) {
    show_validation_error("Missing dose probabilities", "Please enter a numeric probability (0-1) for each dose level.")
    return(NULL)
  }
  if (any(!is.finite(doseprobs)) || any(doseprobs < 0 | doseprobs > 1)) {
    show_validation_error("Invalid dose probabilities", "Each dose probability must be a numeric value between 0 and 1.")
    return(NULL)
  }
  
  as.numeric(doseprobs)
}

normalize_trial_mode_input <- function(trial_mode_input) {
  trial_mode_input <- trimws(as.character(trial_mode_input))
  
  if (identical(trial_mode_input, "Yes")) {
    return("Accelerated EWOC")
  }
  
  if (identical(trial_mode_input, "No")) {
    return("Standard EWOC")
  }
  
  if (trial_mode_input %in% c("Standard EWOC", "Accelerated EWOC")) {
    return(trial_mode_input)
  }
  
  NULL
}

validate_alpha_increment_cutoff <- function(input, theta, prefix = "") {
  rule_id <- paste0(prefix, "alpha_increment_rule")
  cutoff_id <- paste0(prefix, "alpha_increment_cutoff")
  use_custom <- identical(input[[rule_id]], "Custom")
  
  if (!isTRUE(use_custom)) {
    return(list(
      use_custom_alpha_increment_cutoff = FALSE,
      alpha_increment_cutoff = theta
    ))
  }
  
  cutoff <- validate_scalar_input(
    input[[cutoff_id]],
    title = "Invalid custom toxicity bound",
    message = "Custom toxicity bound must be numeric, greater than 0, and lower than theta.",
    min_value = 0,
    max_value = theta,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )
  if (is.null(cutoff)) {
    return(NULL)
  }
  
  list(
    use_custom_alpha_increment_cutoff = TRUE,
    alpha_increment_cutoff = cutoff
  )
}

## Validation: efficiency stopping inputs left blank while the optional rule is selected. Returns TRUE
## (after showing a message naming the missing inputs) when any is blank, otherwise FALSE.
efficiency_inputs_missing <- function(input, dose_type = "Discrete") {
  if (!identical(input$stopping_rule, "Yes")) return(FALSE)
  missing_fields <- c(
    if (!is.finite(suppressWarnings(as.numeric(input$num_patients_restrict)))) "the maximum number of patients at the recommended dose",
    if (identical(dose_type, "Continuous") && !is.finite(suppressWarnings(as.numeric(input$efficiency_stop_tolerance)))) "the dose-region width (%)"
  )
  if (length(missing_fields) == 0L) return(FALSE)
  show_validation_error(
    "Efficiency stopping inputs needed",
    paste0("The trial efficiency stopping rule is selected. Please provide ", paste(missing_fields, collapse = " and "), ", or set the rule to No.")
  )
  TRUE
}

validate_trial_inputs <- function(input, has_prior_results) {
  dose_type <- trimws(as.character(input$prior_dose_type))
  if (length(dose_type) != 1L || !dose_type %in% c("Discrete", "Continuous")) {
    dose_type <- "Discrete"
  }
  
  if (identical(dose_type, "Continuous")) {
    if (!isTRUE(has_prior_results)) {
      show_validation_error("Missing prior results", "Please run 'Run Prior Calculations' before running simulations.")
      return(NULL)
    }
    
    min_dose <- validate_scalar_input(
      input$min_dose,
      title = "Invalid minimum dose",
      message = "Minimum dose must be numeric and greater than or equal to 0.",
      min_value = 0
    )
    if (is.null(min_dose)) return(NULL)
    
    max_dose <- validate_scalar_input(
      input$max_dose,
      title = "Invalid maximum dose",
      message = "Maximum dose must be numeric and greater than the minimum dose.",
      min_value = 0
    )
    if (is.null(max_dose)) return(NULL)
    if (max_dose <= min_dose) {
      return(show_validation_error(
        "Invalid continuous dose range",
        "Maximum dose must be greater than minimum dose."
      ))
    }
    
    dose_unit <- trimws(as.character(input$dose_unit))
    if (!nzchar(dose_unit)) {
      return(show_validation_error(
        "Missing dose unit",
        "Please enter the unit for the continuous dose values."
      ))
    }
    
    total_n <- validate_scalar_input(
      input$NN,
      title = "Invalid sample size",
      message = "Total sample size must be a whole number greater than or equal to 1.",
      integer_required = TRUE,
      min_value = 1
    )
    if (is.null(total_n)) return(NULL)
    
    cohort <- validate_scalar_input(
      input$cohort,
      title = "Invalid cohort size",
      message = "Cohort size must be a whole number greater than or equal to 1.",
      integer_required = TRUE,
      min_value = 1
    )
    if (is.null(cohort)) return(NULL)
    
    if (total_n < cohort) {
      return(show_validation_error(
        "Invalid sample size",
        "Total sample size must be greater than or equal to the cohort size."
      ))
    }
    if ((total_n %% cohort) != 0L) {
      return(show_validation_error(
        "Incompatible sample and cohort sizes",
        "Total sample size must be divisible by the cohort size."
      ))
    }
    
    theta <- validate_scalar_input(
      input$theta,
      title = "Invalid target DLT probability",
      message = "Target probability of DLT (theta) must be numeric and strictly between 0 and 1.",
      min_value = 0,
      max_value = 1,
      lower_inclusive = FALSE,
      upper_inclusive = FALSE
    )
    if (is.null(theta)) return(NULL)
    
    lower_f_bound <- validate_scalar_input(
      input$lower_f_bound,
      title = "Invalid feasibility bound",
      message = "Lower feasibility bound must be between 0 and 0.5.",
      min_value = 0,
      max_value = 0.5
    )
    if (is.null(lower_f_bound)) return(NULL)
    
    increment <- validate_scalar_input(
      input$increment,
      title = "Invalid increment",
      message = "Feasibility bound increment must be numeric and greater than 0.",
      min_value = 0,
      lower_inclusive = FALSE
    )
    if (is.null(increment)) return(NULL)
    
    deltastop <- validate_scalar_input(
      input$deltastop,
      title = "Invalid delta threshold",
      message = "Threshold, Delta, must be numeric and strictly between 0 and 1.",
      min_value = 0,
      max_value = 1,
      lower_inclusive = FALSE,
      upper_inclusive = FALSE
    )
    if (is.null(deltastop)) return(NULL)
    
    max_dose_increment <- validate_scalar_input(
      input$max_dose_increment,
      title = "Invalid maximum dose increment",
      message = "Maximum dose increment percentage must be numeric and greater than 0.",
      min_value = 0,
      lower_inclusive = FALSE
    )
    if (is.null(max_dose_increment)) return(NULL)
    # Previous real-dose increment validation:
    # if (max_dose_increment > (max_dose - min_dose)) {
    #   return(show_validation_error(
    #     "Invalid maximum dose increment",
    #     "Maximum dose increment cannot exceed the initial continuous dose range."
    #   ))
    # }
    if (max_dose_increment > 100) {
      return(show_validation_error(
        "Invalid maximum dose increment",
        "Maximum dose increment percentage cannot exceed 100."
      ))
    }
    
    addon_inputs <- validate_addon_inputs(input, include_seed = TRUE, include_mcmc = TRUE)
    if (is.null(addon_inputs)) return(NULL)
    
    M <- validate_scalar_input(
      input$M,
      title = "Invalid number of trials",
      message = "Total number of trials must be a whole number greater than or equal to 1.",
      integer_required = TRUE,
      min_value = 1
    )
    if (is.null(M)) return(NULL)
    
    trial_mode_input <- normalize_trial_mode_input(input$trial_mode)
    if (is.null(trial_mode_input)) {
      show_validation_error(
        "Invalid trial mode",
        "Accelerated EWOC must be selected as either 'Yes' or 'No'."
      )
      return(NULL)
    }
    if (identical(trial_mode_input, "Accelerated EWOC")) {
      show_validation_error(
        "Continuous accelerated EWOC not available yet",
        "Please choose 'No' for Accelerated EWOC while running continuous dose simulations. Continuous trial conduct changes will be handled separately."
      )
      return(NULL)
    }
    
    alpha_increment_inputs <- validate_alpha_increment_cutoff(input, theta)
    if (is.null(alpha_increment_inputs)) return(NULL)
    
    stopping_rule_selected <- identical(input$stopping_rule, "Yes")
    num_patients_restrict_input <- suppressWarnings(as.numeric(input$num_patients_restrict))
    efficiency_stop_tolerance_input <- suppressWarnings(as.numeric(input$efficiency_stop_tolerance))
    if (efficiency_inputs_missing(input, "Continuous")) return(NULL)
    if (isTRUE(stopping_rule_selected)) {
      if (!is.finite(num_patients_restrict_input) ||
          num_patients_restrict_input <= 0 ||
          num_patients_restrict_input != floor(num_patients_restrict_input) ||
          num_patients_restrict_input > total_n) {
        show_validation_error(
          "Invalid efficiency-based stopping patient count",
          "The maximum number of patients at the recommended dose must be a whole number greater than 0 and no larger than the total sample size."
        )
        return(NULL)
      }
      if (!is.finite(efficiency_stop_tolerance_input) ||
          efficiency_stop_tolerance_input <= 0 ||
          efficiency_stop_tolerance_input > 100) {
        show_validation_error(
          "Invalid dose-region width",
          "The continuous dose-region width must be a percentage greater than 0 and at most 100."
        )
        return(NULL)
      }
    }

    return(list(
      dose_type = dose_type,
      min_dose = min_dose,
      max_dose = max_dose,
      dose_unit = dose_unit,
      max_dose_increment = max_dose_increment,
      num_doses = NA_integer_,
      startdose = 1L,
      NN = total_n,
      cohort = cohort,
      theta = theta,
      lower_f_bound = lower_f_bound,
      increment = increment,
      deltastop = deltastop,
      doseprobs = numeric(0),
      seed = addon_inputs$seed,
      burn = addon_inputs$burn,
      mm = addon_inputs$mm,
      M = M,
      trial_mode = trial_mode_input,
      max_accel_dose_level = NA_integer_,
      stopping_rule_selected = stopping_rule_selected,
      num_patients_restrict = if (isTRUE(stopping_rule_selected)) as.integer(num_patients_restrict_input) else NA_integer_,
      efficiency_stop_tolerance = if (isTRUE(stopping_rule_selected)) efficiency_stop_tolerance_input else NA_real_,
      use_custom_alpha_increment_cutoff = alpha_increment_inputs$use_custom_alpha_increment_cutoff,
      alpha_increment_cutoff = alpha_increment_inputs$alpha_increment_cutoff
    ))
  }
  
  design_inputs <- validate_common_design_inputs(input, context = "trial")
  if (is.null(design_inputs)) {
    return(NULL)
  }
  
  if (!isTRUE(has_prior_results)) {
    show_validation_error("Missing prior results", "Please run 'Run Prior Calculations' before running simulations.")
    return(NULL)
  }
  
  doseprobs <- validate_dose_probabilities(input, design_inputs$num_doses)
  if (is.null(doseprobs)) {
    return(NULL)
  }
  
  addon_inputs <- validate_addon_inputs(input, include_seed = TRUE, include_mcmc = TRUE)
  if (is.null(addon_inputs)) {
    return(NULL)
  }
  
  M <- validate_scalar_input(
    input$M,
    title = "Invalid number of trials",
    message = "Total number of trials must be a whole number greater than or equal to 1.",
    integer_required = TRUE,
    min_value = 1
  )
  if (is.null(M)) {
    return(NULL)
  }
  
  trial_mode_input <- normalize_trial_mode_input(input$trial_mode)
  if (is.null(trial_mode_input)) {
    show_validation_error(
      "Invalid trial mode",
      "Accelerated EWOC must be selected as either 'Yes' or 'No'."
    )
    return(NULL)
  }
  alpha_increment_inputs <- validate_alpha_increment_cutoff(input, design_inputs$theta)
  if (is.null(alpha_increment_inputs)) {
    return(NULL)
  }
  
  max_accel_dose_level_input <- suppressWarnings(as.numeric(input$max_accel_dose_level))
  if (identical(trial_mode_input, "Accelerated EWOC")) {
    if (!is.finite(max_accel_dose_level_input)) {
      show_validation_error(
        "Maximum dose level for acceleration needed",
        "Accelerated EWOC is selected. Please provide the maximum dose level for acceleration, or set Accelerated EWOC to No."
      )
      return(NULL)
    }
    if (max_accel_dose_level_input != floor(max_accel_dose_level_input) ||
        max_accel_dose_level_input < design_inputs$startdose ||
        max_accel_dose_level_input >= design_inputs$num_doses) {
      show_validation_error(
        "Invalid acceleration cap",
        "Maximum dose level for acceleration must be a whole number from the starting dose up to one level below the highest dose level (it must be lower than the highest dose level)."
      )
      return(NULL)
    }
  }
  
  stopping_rule_selected <- identical(input$stopping_rule, "Yes")
  num_patients_restrict_input <- suppressWarnings(as.numeric(input$num_patients_restrict))
  if (efficiency_inputs_missing(input, "Discrete")) return(NULL)
  if (isTRUE(stopping_rule_selected)) {
    if (!is.finite(num_patients_restrict_input) ||
        num_patients_restrict_input <= 0 ||
        num_patients_restrict_input != floor(num_patients_restrict_input) ||
        num_patients_restrict_input > design_inputs$NN) {
      show_validation_error(
        "Invalid maximum patients at the recommended dose",
        "The maximum number of patients at the recommended dose must be a whole number greater than 0 and no larger than the total sample size."
      )
      return(NULL)
    }
  }
  
  c(
    design_inputs,
    list(
      doseprobs = doseprobs,
      seed = addon_inputs$seed,
      burn = addon_inputs$burn,
      mm = addon_inputs$mm,
      M = M,
      trial_mode = trial_mode_input,
      max_accel_dose_level = if (identical(trial_mode_input, "Accelerated EWOC")) as.integer(max_accel_dose_level_input) else NA_integer_,
      stopping_rule_selected = stopping_rule_selected,
      num_patients_restrict = if (isTRUE(stopping_rule_selected)) as.integer(num_patients_restrict_input) else NA_integer_,
      use_custom_alpha_increment_cutoff = alpha_increment_inputs$use_custom_alpha_increment_cutoff,
      alpha_increment_cutoff = alpha_increment_inputs$alpha_increment_cutoff
    )
  )
}

validate_tree_inputs <- function(input, has_prior_results) {
  if (!isTRUE(has_prior_results)) {
    show_validation_error("Missing prior results", "Please run 'Run Prior Calculations' before building the tree.")
    return(NULL)
  }
  
  design_inputs <- validate_common_design_inputs(input, context = "tree")
  if (is.null(design_inputs)) {
    return(NULL)
  }
  
  addon_inputs <- validate_addon_inputs(input, include_mcmc = TRUE)
  if (is.null(addon_inputs)) {
    return(NULL)
  }
  
  trial_mode_input <- normalize_trial_mode_input(input$trial_mode)
  if (is.null(trial_mode_input)) {
    show_validation_error(
      "Invalid trial mode",
      "Accelerated EWOC must be selected as either 'Yes' or 'No'."
    )
    return(NULL)
  }
  if (identical(design_inputs$dose_type, "Continuous") &&
      identical(trial_mode_input, "Accelerated EWOC")) {
    show_validation_error(
      "Continuous accelerated EWOC not available yet",
      "Please choose 'No' for Accelerated EWOC while building a continuous dose decision tree."
    )
    return(NULL)
  }
  
  alpha_increment_inputs <- validate_alpha_increment_cutoff(input, design_inputs$theta)
  if (is.null(alpha_increment_inputs)) {
    return(NULL)
  }
  
  max_accel_dose_level_input <- suppressWarnings(as.numeric(input$max_accel_dose_level))
  if (identical(trial_mode_input, "Accelerated EWOC")) {
    if (!is.finite(max_accel_dose_level_input)) {
      show_validation_error(
        "Maximum dose level for acceleration needed",
        "Accelerated EWOC is selected. Please provide the maximum dose level for acceleration, or set Accelerated EWOC to No."
      )
      return(NULL)
    }
    if (max_accel_dose_level_input != floor(max_accel_dose_level_input) ||
        max_accel_dose_level_input < design_inputs$startdose ||
        max_accel_dose_level_input >= design_inputs$num_doses) {
      show_validation_error(
        "Invalid acceleration cap",
        "Maximum dose level for acceleration must be a whole number from the starting dose up to one level below the highest dose level (it must be lower than the highest dose level)."
      )
      return(NULL)
    }
  }
  
  if (efficiency_inputs_missing(input, design_inputs$dose_type)) {
    return(NULL)
  }
  
  tree_steps <- validate_scalar_input(
    input$tree_steps,
    title = "Invalid number of steps",
    message = "Decision steps to expand must be at least 2.",
    integer_required = TRUE,
    min_value = 2
  )
  if (is.null(tree_steps)) {
    return(NULL)
  }
  
  c(
    design_inputs,
    list(
      burn = addon_inputs$burn,
      mm = addon_inputs$mm,
      tree_steps = tree_steps,
      # Efficiency stopping, drawn as stop leaves in the tree (same settings as the simulations).
      stopping_rule_selected = identical(input$stopping_rule, "Yes"),
      num_patients_restrict = if (identical(input$stopping_rule, "Yes")) suppressWarnings(as.numeric(input$num_patients_restrict)) else NA_real_,
      efficiency_stop_tolerance = if (identical(input$stopping_rule, "Yes")) suppressWarnings(as.numeric(input$efficiency_stop_tolerance)) else NA_real_,
      trial_mode = trial_mode_input,
      max_accel_dose_level = if (identical(trial_mode_input, "Accelerated EWOC")) as.integer(max_accel_dose_level_input) else NA_integer_,
      use_custom_alpha_increment_cutoff = alpha_increment_inputs$use_custom_alpha_increment_cutoff,
      alpha_increment_cutoff = alpha_increment_inputs$alpha_increment_cutoff
    )
  )
}
