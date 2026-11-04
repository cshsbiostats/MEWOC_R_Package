#' Build a Trial Conduct design table
#'
#' Creates the design worksheet of the single-agent Trial Conduct workbook: the
#' prior, the dose-finding rules and the MCMC settings that later evaluations of
#' the trial data must use.
#'
#' @param p0,p1 Prior probabilities that the MTD is below the minimum dose and
#'   above the maximum dose.
#' @param a0,b0,a1,b1 Calibrated beta hyperparameters (see
#'   [run_mewoc_grid_search()]).
#' @param num_doses Number of discrete dose levels.
#' @param startdose Starting dose level.
#' @param theta Target DLT probability.
#' @param deltastop Safety stopping threshold.
#' @param lower_f_bound Starting feasibility bound alpha.
#' @param increment Increase of the feasibility bound per cohort.
#' @param trial_mode `"Standard EWOC"` or `"Accelerated EWOC"`. The workbook
#'   aliases `"No"` and `"Yes"`, respectively, are also accepted.
#' @param max_accel_dose_level Highest dose level of the accelerated lead-in
#'   (`NA` for standard EWOC).
#' @param seed Random seed.
#' @param burn Number of MCMC burn-in iterations.
#' @param mm Number of retained MCMC samples.
#'
#' @return A data frame with columns `Trial Design Parameter` and `Value`, one row
#'   per setting.
#' @seealso [conduct_trial_rule_rows()], [write_conduct_workbook()].
#' @examples
#' build_conduct_design_table(
#'   p0 = 0.1, p1 = 0.2, a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66,
#'   num_doses = 4, startdose = 1, theta = 0.33, deltastop = 0.85,
#'   lower_f_bound = 0.25, increment = 0.05, trial_mode = "Standard EWOC",
#'   max_accel_dose_level = NA, seed = 988, burn = 1000, mm = 1500)
#' @export
build_conduct_design_table <- function(p0, p1, a0, b0, a1, b1, num_doses,
                                       startdose, theta, deltastop, lower_f_bound, increment,
                                       trial_mode, max_accel_dose_level, seed, burn, mm) {
  if (!is.character(trial_mode) || length(trial_mode) != 1L ||
      is.na(trial_mode) ||
      !trial_mode %in% c("Standard EWOC", "Accelerated EWOC", "No", "Yes")) {
    stop('trial_mode must be "Standard EWOC" or "Accelerated EWOC" (or "No"/"Yes").', call. = FALSE)
  }
  trial_mode_value <- if (trial_mode %in% c("Accelerated EWOC", "Yes")) "Yes" else "No"
  max_accel_value <- if (identical(trial_mode_value, "Yes")) max_accel_dose_level else ""
  if (length(max_accel_value) != 1L || is.null(max_accel_value)) max_accel_value <- ""

  data.frame(
    `Trial Design Parameter` = c(
      "P(MTD < Min)", "P(MTD > Max)", "a0", "b0", "a1", "b1",
      "Dose Levels", "Starting Dose", "Target DLT Probability (theta)",
      "Stopping Threshold Delta", "\u03b1", "\u03b1 Increment", "Accelerated EWOC",
      "Maximum Accelerated Dose", "Seed", "Burn-in", "MCMC Samples"
    ),
    Value = c(
      p0, p1, a0, b0, a1, b1, num_doses, startdose, theta,
      deltastop, lower_f_bound, increment, trial_mode_value, max_accel_value, seed, burn, mm
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Build the trial size and efficiency stopping rows of a Trial Conduct design table
#'
#' Gives the rows for cohort size, maximum sample size and the optional efficiency
#' stopping rule, to be appended to [build_conduct_design_table()].
#'
#' @param cohort_size Minimum number of patients per cohort.
#' @param total_sample_size Maximum sample size N.
#' @param stopping_rule Logical or `"Yes"`/`"No"`; whether the efficiency stopping
#'   rule is used.
#' @param num_patients_restrict Maximum number of patients at the recommended dose.
#' @param efficiency_width Continuous doses: dose-region width, in percent of the
#'   dose range.
#' @param dose_type `"Discrete"` or `"Continuous"`.
#'
#' @return A data frame with columns `Trial Design Parameter` and `Value`.
#' @examples
#' conduct_trial_rule_rows(cohort_size = 3, total_sample_size = 30,
#'                         stopping_rule = TRUE, num_patients_restrict = 9)
#' @export
conduct_trial_rule_rows <- function(cohort_size, total_sample_size, stopping_rule = FALSE,
                                    num_patients_restrict = NA, efficiency_width = NA,
                                    dose_type = "Discrete") {
  enabled <- isTRUE(stopping_rule) || identical(stopping_rule, "Yes")
  text_value <- function(x) {
    if (length(x) == 1L && !is.na(x)) trimws(as.character(x)) else ""
  }
  parameter <- c("Cohort Size", "Maximum Sample Size", "Efficiency Stopping Rule")
  value <- c(text_value(cohort_size), text_value(total_sample_size), if (enabled) "Yes" else "No")
  if (enabled) {
    parameter <- c(parameter, "Maximum Patients at Recommended Dose")
    value <- c(value, text_value(num_patients_restrict))
    if (identical(dose_type, "Continuous")) {
      parameter <- c(parameter, "Dose-Region Width (%)")
      value <- c(value, text_value(efficiency_width))
    }
  }
  data.frame(
    `Trial Design Parameter` = parameter,
    Value = value,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

## Trial Conduct: EWOC categorical recommendation. The alpha-quantile of the MTD posterior is snapped to
## the nearest dose level; an untried dose more than one level above the current dose is replaced by the
## next level up.
recommend_conduct_categorical_dose <- function(ss, alpha_value, design, current_dose, assigned_doses) {
  doses <- design$doselevels
  posterior_mtd <- max(0, as.numeric(stats::quantile(ss$mtd, alpha_value, na.rm = TRUE)))
  posterior_mtd <- min(posterior_mtd, 1)
  target_position <- which.min(abs(doses - posterior_mtd))
  current_position <- which.min(abs(doses - current_dose))
  target_seen_before <- any(abs(assigned_doses - doses[target_position]) < sqrt(.Machine$double.eps))
  if (target_position > current_position + 1L && !target_seen_before) {
    target_position <- current_position + 1L
  }
  list(
    dose_value = doses[target_position],
    actual_dose = design$actual_dose_levels[target_position],
    dose_level = target_position
  )
}

#' Build a Trial Conduct patient template
#'
#' Creates the empty patient worksheet of the single-agent Trial Conduct workbook,
#' where the cohort, patient, dose given and DLT status of each patient are
#' recorded during the trial.
#'
#' @return A data frame with columns `Cohort`, `Patient`, `Dose Given`,
#'   `DLT Status`, `Dose Recommended` and alpha, and three example rows showing
#'   the DLT Status values (`No DLT`, `DLT`, `Unevaluable`).
#' @seealso [write_conduct_workbook()].
#' @examples
#' build_conduct_patient_template()
#' @export
build_conduct_patient_template <- function() {
  data.frame(
    Cohort = c(1L, 1L, 2L),
    Patient = c(1L, 2L, 3L),
    `Dose Given` = c(NA_integer_, NA_integer_, NA_integer_),
    `DLT Status` = c("No DLT", "DLT", "Unevaluable"),
    `Dose Recommended` = c(NA_integer_, NA_integer_, NA_integer_),
    "\u03b1" = c(NA_real_, NA_real_, NA_real_),
    check.names = FALSE
  )
}

#' Write a Trial Conduct workbook
#'
#' Writes the single-agent Trial Conduct workbook (Excel), with a design worksheet
#' and a patient data-input worksheet.
#'
#' @param file Path of the `.xlsx` file to write.
#' @param design_table Design table, for example from
#'   [build_conduct_design_table()] combined with [conduct_trial_rule_rows()].
#' @param data_table Patient template from [build_conduct_patient_template()] or
#'   patient data.
#'
#' @return The path `file`, invisibly.
#' @examples
#' design <- rbind(
#'   build_conduct_design_table(
#'     p0 = 0.1, p1 = 0.2, a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66,
#'     num_doses = 4, startdose = 1, theta = 0.33, deltastop = 0.85,
#'     lower_f_bound = 0.25, increment = 0.05, trial_mode = "Standard EWOC",
#'     max_accel_dose_level = NA, seed = 988, burn = 1000, mm = 1500),
#'   conduct_trial_rule_rows(cohort_size = 3, total_sample_size = 30))
#' file <- tempfile(fileext = ".xlsx")
#' write_conduct_workbook(file, design, build_conduct_patient_template())
#' file.exists(file)
#' unlink(file)
#' @export
write_conduct_workbook <- function(file, design_table, data_table) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The openxlsx package is required to create Trial Conduct workbooks.", call. = FALSE)
  }
  if (!is.data.frame(design_table) || ncol(design_table) != 2L) {
    stop("The Trial Design table must be a two-column data frame.", call. = FALSE)
  }
  if (!is.data.frame(data_table) || ncol(data_table) < 1L) {
    stop("The Data Input table must be a non-empty data frame.", call. = FALSE)
  }

  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "Trial Design")
  openxlsx::addWorksheet(workbook, "Data Input")
  openxlsx::writeData(workbook, "Trial Design", design_table)
  openxlsx::writeData(workbook, "Data Input", data_table)
  header_style <- openxlsx::createStyle(
    fgFill = "#D9EAD3", textDecoration = "bold", border = "Bottom", halign = "center"
  )
  openxlsx::addStyle(workbook, "Trial Design", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
  openxlsx::addStyle(workbook, "Data Input", header_style, rows = 1, cols = seq_len(ncol(data_table)), gridExpand = TRUE)
  openxlsx::setColWidths(workbook, "Trial Design", cols = 1, widths = 34)
  openxlsx::setColWidths(workbook, "Trial Design", cols = 2, widths = 18)
  openxlsx::setColWidths(workbook, "Data Input", cols = seq_len(ncol(data_table)), widths = "auto")
  openxlsx::freezePane(workbook, "Trial Design", firstRow = TRUE)
  openxlsx::freezePane(workbook, "Data Input", firstRow = TRUE)
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

## Trial Conduct: evaluate `expr` with a fixed random seed, then restore the session's random-number
## state, so reproducible Trial Conduct fits do not change other random draws in the app session.
## A NULL or non-finite seed evaluates `expr` without seeding.
with_local_seed <- function(seed, expr) {
  seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1L || !is.finite(seed)) return(expr)
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  expr
}

## Trial Conduct: map a dose given (actual value) to the standardized scale. A discrete dose must match
## one of the dose levels; a continuous dose must lie in the dose range. dose_index is the dose level.
conduct_standardize_actual_dose <- function(actual_dose, design) {
  actual_dose <- suppressWarnings(as.numeric(actual_dose))
  if (length(actual_dose) != 1L || !is.finite(actual_dose)) {
    return(list(ok = FALSE, message = "Enter administered dose and DLT to continue."))
  }
  if (identical(design$dose_type, "Continuous")) {
    if (!is.finite(design$min_dose) || !is.finite(design$max_dose) || design$max_dose <= design$min_dose) {
      return(list(ok = FALSE, message = "Enter valid continuous minimum and maximum doses."))
    }
    if (actual_dose < design$min_dose || actual_dose > design$max_dose) {
      return(list(ok = FALSE, message = "Dose given must be within the continuous minimum and maximum dose range."))
    }
    standardized <- (actual_dose - design$min_dose) / (design$max_dose - design$min_dose)
    return(list(ok = TRUE, standardized = standardized, dose_index = NA_integer_, actual_dose = actual_dose))
  }
  if (length(design$actual_dose_levels) != design$num_doses || any(!is.finite(design$actual_dose_levels))) {
    return(list(ok = FALSE, message = "Enter one actual dose value for each categorical dose level."))
  }
  actual_levels <- design$actual_dose_levels
  idx <- which(abs(actual_levels - actual_dose) <=
                 sqrt(.Machine$double.eps) * pmax(1, abs(actual_levels), abs(actual_dose)))
  if (length(idx) == 0L) {
    allowed <- paste(vapply(actual_levels, format_dose_value, character(1)), collapse = ", ")
    return(list(ok = FALSE, message = paste0("Dose given must match one of the categorical actual doses: ", allowed, ".")))
  }
  list(
    ok = TRUE,
    standardized = design$doselevels[idx[1]],
    dose_index = idx[1],
    actual_dose = actual_levels[idx[1]]
  )
}

## Trial Conduct: actual dose for a standardized dose (the nearest dose level for a discrete design).
conduct_actual_from_standardized <- function(standardized, design) {
  standardized <- suppressWarnings(as.numeric(standardized))
  if (length(standardized) != 1L || !is.finite(standardized)) return(NA_real_)
  standardized <- max(0, min(standardized, 1))
  if (identical(design$dose_type, "Continuous")) {
    return(design$min_dose + standardized * (design$max_dose - design$min_dose))
  }
  design$actual_dose_levels[which.min(abs(design$doselevels - standardized))]
}

## Trial Conduct: fill blank Cohort entries in row order. A blank row joins the previous row's cohort
## until that cohort has cohort_size patients, then starts the next cohort. During an accelerated lead-in
## (until the first DLT or a dose at the maximum accelerated level) each cohort is one patient.
conduct_fill_cohorts <- function(cohort, cohort_size, leadin_flags = NULL) {
  cohort <- trimws(as.character(cohort))
  cohort[is.na(cohort)] <- ""
  filled <- !nzchar(cohort)
  if (!any(filled)) {
    return(list(cohort = cohort, filled = filled))
  }
  cohort_size <- suppressWarnings(as.integer(cohort_size))
  if (length(cohort_size) != 1L || !is.finite(cohort_size) || cohort_size < 1L) cohort_size <- 1L
  if (is.null(leadin_flags)) leadin_flags <- rep(FALSE, length(cohort))
  next_label <- function(previous) {
    number <- suppressWarnings(as.numeric(previous))
    if (is.finite(number)) as.character(number + 1) else paste0(previous, "+1")
  }
  for (i in seq_along(cohort)) {
    if (!filled[i]) next
    if (i == 1L) {
      cohort[i] <- "1"
      next
    }
    previous <- cohort[i - 1L]
    size_limit <- if (isTRUE(leadin_flags[i - 1L])) 1L else cohort_size
    cohort[i] <- if (sum(cohort[seq_len(i - 1L)] == previous) < size_limit) previous else next_label(previous)
  }
  list(cohort = cohort, filled = filled)
}

#' Evaluate single-agent Trial Conduct data cohort by cohort
#'
#' Recomputes, for every cohort, the recommended dose and the feasibility bound
#' alpha that the design rules give, using only the data available before that
#' cohort was enrolled. The model, the maximum sample size N and the efficiency
#' rule count only evaluable patients (DLT 1 or 0); pending and unevaluable
#' patients are reported separately. The trial stops when the posterior probability
#' that the DLT probability at the minimum dose exceeds `theta` is greater than
#' Delta, or when the efficiency rule applies; later rows are not used. Entered
#' recommended doses and alphas are checked against the recomputed values and blank
#' ones are filled in. An invalid entry (a dose outside the protocol doses or range,
#' a dose that is not a number, a blank dose with a DLT result, or a DLT value other
#' than 1, 0 or blank) stops the evaluation with a message. Cohorts with fewer
#' evaluable patients than the minimum cohort size are reported in
#' `incomplete_cohorts`. The caller's random-number state is not changed.
#'
#' @param rows Data frame of patients with columns `Cohort`, `Patient`,
#'   `Administered_Dose`, `DLT` (1, 0 or `NA`), `Is_Unevaluable`,
#'   `Uploaded_Recommended_Dose` and `Uploaded_Alpha_Used`; optionally
#'   `Administered_Dose_Text` (the dose cell as entered) and `DLT_Status_Text`.
#' @param design List describing the doses: `dose_type` (`"Discrete"` or
#'   `"Continuous"`), `dose_unit`; for discrete designs `num_doses`, `startdose`,
#'   `actual_dose_levels` and `doselevels` (standardized, 0 to 1); for continuous
#'   designs `min_dose`, `max_dose` and `max_dose_increment` (percent of the range).
#' @param settings List with `theta`, `deltastop`, `lower_alpha`, `increment`,
#'   `alpha_increment_cutoff`, `burn`, `mm`, `seed`, `trial_mode`,
#'   `max_accel_dose_level`, `cohort_size`, `total_sample_size`, `efficiency_rule`,
#'   `num_patients_restrict` and `efficiency_width` (continuous, proportion of the
#'   dose range).
#' @param opt1 Prior hyperparameters with elements `a0`, `b0`, `a1`, `b1`.
#'
#' @return A list with `status` (`"error"` when an entry is invalid, with
#'   `message`); otherwise `rows` (the patient rows with the recomputed
#'   `Check_Recommended_Dose` and `Check_Alpha` and warnings),
#'   `cohort_summary`, `posterior_table`, `next_recommendation` (with `dose`, the
#'   dose for the next cohort, and `stop_type`/`stop_reason` after a stop),
#'   `alpha_for_next` (alpha for the next cohort), `trial_stop`, `warnings`,
#'   `messages`, `observed_dlt_rate`, the patient counts `n_patients`,
#'   `n_treated`, `n_pending` and `n_unevaluable`, and `incomplete_cohorts`.
#' @examples
#' prior <- list(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)
#' design <- list(dose_type = "Discrete", dose_unit = "mg", num_doses = 4,
#'                startdose = 1, actual_dose_levels = c(10, 20, 30, 40),
#'                dose_units = rep("mg", 4), doselevels = seq(0, 1, length.out = 4),
#'                min_dose = NA, max_dose = NA, max_dose_increment = NA)
#' settings <- list(theta = 0.33, deltastop = 0.85, lower_alpha = 0.25,
#'                  increment = 0.05, alpha_increment_cutoff = 0.33, burn = 200,
#'                  mm = 300, seed = 7, trial_mode = "Standard EWOC",
#'                  max_accel_dose_level = NA, cohort_size = 3,
#'                  total_sample_size = 30, efficiency_rule = FALSE,
#'                  num_patients_restrict = NA, efficiency_width = NA)
#' rows <- data.frame(Cohort = c("1", "1", "1"), Patient = c("1", "2", "3"),
#'                    Administered_Dose = c(10, 10, 10), DLT = c(0L, 0L, 0L),
#'                    Is_Unevaluable = FALSE, Uploaded_Recommended_Dose = NA,
#'                    Uploaded_Alpha_Used = NA)
#' result <- conduct_evaluate_trial(rows, design, settings, prior)
#' result$next_recommendation$dose
#' result$alpha_for_next
#' @export
conduct_evaluate_trial <- function(rows, design, settings, opt1) {
  check_prior_parameters(opt1)
  if (!is.list(design) || !is.list(settings)) stop("design and settings must be lists.", call. = FALSE)
  if (!is.data.frame(rows)) stop("rows must be a patient data frame.", call. = FALSE)
  required_columns <- c("Cohort", "Patient", "Administered_Dose", "DLT")
  if (!all(required_columns %in% names(rows))) {
    stop("rows is missing patient columns: ", paste(setdiff(required_columns, names(rows)), collapse = ", "), call. = FALSE)
  }
  if (!identical(design$dose_type, "Continuous") && !identical(design$dose_type, "Discrete")) {
    stop("design$dose_type must be Discrete or Continuous.", call. = FALSE)
  }
  setting <- function(field, default) if (is.null(settings[[field]])) default else settings[[field]]
  check_simulation_controls(settings$theta, settings$deltastop, setting("burn", 1000), setting("mm", 1000),
                            setting("efficiency_rule", FALSE), settings$num_patients_restrict,
                            setting("trial_mode", "Standard EWOC"), setting("alpha_increment_cutoff", settings$theta))
  check_numeric_scalar(settings$lower_alpha, "lower_alpha", 0, 0.5)
  check_numeric_scalar(settings$increment, "increment", 0, lower_open = TRUE)
  check_numeric_scalar(settings$seed, "seed", 0, integer = TRUE)
  check_numeric_scalar(settings$cohort_size, "cohort_size", 1, integer = TRUE)
  check_numeric_scalar(settings$total_sample_size, "total_sample_size", 1, integer = TRUE)
  if (identical(design$dose_type, "Discrete")) {
    check_numeric_scalar(design$num_doses, "num_doses", 2, integer = TRUE)
    check_single_trial_inputs(design$doselevels, rep(0, design$num_doses), design$startdose,
                              settings$cohort_size, settings$total_sample_size, settings$theta)
    if (!is.numeric(design$actual_dose_levels) || length(design$actual_dose_levels) != design$num_doses ||
        any(!is.finite(design$actual_dose_levels)) || anyDuplicated(design$actual_dose_levels)) {
      stop("actual_dose_levels must provide a distinct finite dose for each level.", call. = FALSE)
    }
    if (identical(settings$trial_mode, "Accelerated EWOC")) {
      check_numeric_scalar(settings$max_accel_dose_level, "max_accel_dose_level", design$startdose, design$num_doses, integer = TRUE)
    }
  } else {
    check_numeric_scalar(design$min_dose, "min_dose")
    check_numeric_scalar(design$max_dose, "max_dose", design$min_dose, lower_open = TRUE)
    check_numeric_scalar(design$max_dose_increment, "max_dose_increment", 0, 100, lower_open = TRUE)
    if (isTRUE(settings$efficiency_rule)) check_numeric_scalar(settings$efficiency_width, "efficiency_width", 0, 1, lower_open = TRUE)
  }
  tolerance <- sqrt(.Machine$double.eps)
  is_continuous <- identical(design$dose_type, "Continuous")
  num <- function(x, default = NA_real_) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 1L && is.finite(x)) x else default
  }
  theta <- num(settings$theta)
  deltastop <- num(settings$deltastop)
  lower_alpha <- max(0, min(num(settings$lower_alpha, 0), 0.5))
  increment <- num(settings$increment, 0.05)
  alpha_cutoff <- num(settings$alpha_increment_cutoff, theta)
  seed <- as.integer(num(settings$seed, 1))
  cohort_size <- as.integer(num(settings$cohort_size, 1))
  total_n <- num(settings$total_sample_size)
  accelerated <- !is_continuous && identical(settings$trial_mode, "Accelerated EWOC")
  cap_level <- if (accelerated) {
    max(design$startdose, min(as.integer(num(settings$max_accel_dose_level, design$startdose)), design$num_doses - 1L))
  } else {
    NA_integer_
  }
  efficiency_on <- efficiency_stop_enabled(settings$efficiency_rule, settings$num_patients_restrict)
  max_patients <- num(settings$num_patients_restrict)
  width <- if (is_continuous) num(settings$efficiency_width, 0.01) else 0
  if (width <= 0) width <- if (is_continuous) 0.01 else 0
  max_increment_std <- if (is_continuous) num(design$max_dose_increment, 100) / 100 else NA_real_
  start_std <- if (is_continuous) 0 else design$doselevels[design$startdose]
  start_actual <- if (is_continuous) design$min_dose else design$actual_dose_levels[design$startdose]
  model_string <- build_jags_model_string()
  same_dose <- function(a, b) {
    if (!is.finite(a) || !is.finite(b)) return(TRUE)
    if (is_continuous) {
      abs(a - b) <= (design$max_dose - design$min_dose) * width / 2 + tolerance
    } else {
      abs(a - b) <= tolerance * max(1, abs(a), abs(b))
    }
  }

  n_rows <- nrow(rows)
  if (n_rows == 0L) {
    return(list(status = "error", message = "Enter patient outcomes manually or upload a trial conduct file before evaluating the dose recommendation."))
  }
  unevaluable <- if ("Is_Unevaluable" %in% names(rows)) as.logical(rows$Is_Unevaluable) else rep(FALSE, n_rows)
  unevaluable[is.na(unevaluable)] <- FALSE
  # DLT must be exactly 1 (DLT), 0 (No DLT) or blank (pending); any other value is an input error.
  # It is checked before any conversion so that, for example, 0.9 is not truncated to 0.
  dlt_raw <- rows$DLT
  if (is.factor(dlt_raw)) dlt_raw <- as.character(dlt_raw)
  dlt_text <- trimws(as.character(dlt_raw))
  dlt_text[is.na(dlt_text) | toupper(dlt_text) == "NA"] <- ""
  dlt_number <- suppressWarnings(as.numeric(dlt_raw))
  dlt_valid <- is.finite(dlt_number) & dlt_number %in% c(0, 1)
  invalid_dlt <- nzchar(dlt_text) & !dlt_valid
  dlt <- ifelse(dlt_valid, as.integer(dlt_number), NA_integer_)
  dlt_entered <- dlt
  recorded_rec <- if ("Uploaded_Recommended_Dose" %in% names(rows)) suppressWarnings(as.numeric(rows$Uploaded_Recommended_Dose)) else rep(NA_real_, n_rows)
  recorded_alpha <- if ("Uploaded_Alpha_Used" %in% names(rows)) suppressWarnings(as.numeric(rows$Uploaded_Alpha_Used)) else rep(NA_real_, n_rows)
  dose_maps <- lapply(rows$Administered_Dose, conduct_standardize_actual_dose, design = design)
  dose_ok <- vapply(dose_maps, function(m) isTRUE(m$ok), logical(1))
  # Any dose that is not a valid protocol dose stops the evaluation until it is corrected: a number not
  # in the protocol (or outside the range), text that is not a number (the upload keeps the original
  # cell text in Administered_Dose_Text), or a blank dose on a row that has a DLT result.
  dose_text <- if ("Administered_Dose_Text" %in% names(rows)) rows$Administered_Dose_Text else rows$Administered_Dose
  dose_text <- trimws(as.character(dose_text))
  dose_text[is.na(dose_text) | toupper(dose_text) == "NA"] <- ""
  dose_number <- suppressWarnings(as.numeric(rows$Administered_Dose))
  text_dose <- nzchar(dose_text) & !is.finite(suppressWarnings(as.numeric(dose_text)))
  entered_dose <- is.finite(dose_number) | nzchar(dose_text)
  invalid_dose <- entered_dose & !text_dose & !dose_ok
  missing_dose <- !entered_dose & dlt_valid
  input_error_rows <- which(text_dose | invalid_dose | missing_dose | invalid_dlt)
  if (length(input_error_rows)) {
    errors <- vapply(input_error_rows, function(i) {
      problems <- c(
        if (text_dose[i]) paste0("dose \"", dose_text[i], "\" is not a number - enter the dose as a number only (for example 20)."),
        if (invalid_dose[i]) conduct_invalid_dose_message(dose_number[i], design, dose_maps[[i]]$message, omit = FALSE),
        if (missing_dose[i]) "a DLT result is entered but the dose given is blank - enter the dose given.",
        if (invalid_dlt[i]) paste0("DLT value \"", dlt_text[i], "\" is not valid - use 1 (DLT), 0 (No DLT) or leave it blank (pending).")
      )
      paste0("Row ", i, ", cohort ", rows$Cohort[i], ", patient ", rows$Patient[i], ": ", paste(problems, collapse = " Also, "))
    }, character(1))
    rows$Recommended_Dose <- NA_real_
    rows$Alpha_Used <- NA_real_
    return(list(status = "error", rows = rows, message = paste(
      paste(errors, collapse = "\n"),
      "Correct the invalid dose(s) or DLT value(s) and click Evaluate Next Dose Recommendation again. No recommendation was calculated."
    )))
  }
  # DLT Status must be exactly one of DLT, No DLT or Unevaluable. A row marked both DLT/No DLT and
  # Unevaluable, or with an unrecognized status text, is omitted from the calculation.
  status_text <- if ("DLT_Status_Text" %in% names(rows)) trimws(as.character(rows$DLT_Status_Text)) else rep(NA_character_, n_rows)
  status_conflict <- unevaluable & is.finite(dlt)
  status_unknown <- !is.na(status_text) & nzchar(status_text) & !is.finite(dlt) & !unevaluable
  conflict_label <- ifelse(is.finite(dlt) & dlt == 1L, "DLT", "No DLT")
  treated <- dose_ok & !status_conflict & !status_unknown
  std_given <- vapply(dose_maps, function(m) if (isTRUE(m$ok)) m$standardized else NA_real_, numeric(1))
  level_given <- vapply(dose_maps, function(m) if (isTRUE(m$ok)) as.integer(m$dose_index) else NA_integer_, integer(1))
  actual_given <- vapply(dose_maps, function(m) if (isTRUE(m$ok)) m$actual_dose else NA_real_, numeric(1))
  evaluable <- treated & !unevaluable & is.finite(dlt) & dlt %in% c(0L, 1L)
  empty_row <- !treated & !is.finite(suppressWarnings(as.numeric(rows$Administered_Dose))) & !is.finite(dlt) & !unevaluable &
    !status_unknown
  # Omitted rows count nowhere (not as DLT, No DLT, pending or unevaluable).
  unevaluable[status_conflict] <- FALSE
  dlt[status_conflict] <- NA_integer_

  # Cohorts: blank entries are filled in row order (one patient per cohort during the lead-in).
  leadin_flags <- rep(FALSE, n_rows)
  if (accelerated) {
    still_leadin <- TRUE
    for (i in seq_len(n_rows)) {
      leadin_flags[i] <- still_leadin
      if (isTRUE(dlt[i] == 1L) || (is.finite(level_given[i]) && level_given[i] >= cap_level)) still_leadin <- FALSE
    }
  }
  cohort_fill <- conduct_fill_cohorts(rows$Cohort, cohort_size, leadin_flags)
  cohort_fill$filled[empty_row] <- FALSE
  cohort_labels <- cohort_fill$cohort
  cohort_order <- unique(cohort_labels[!empty_row])
  # Numeric cohort labels are evaluated in numeric order, even if the rows are not sorted.
  cohort_numbers <- suppressWarnings(as.numeric(cohort_order))
  if (length(cohort_order) > 1L && all(is.finite(cohort_numbers))) cohort_order <- cohort_order[order(cohort_numbers)]
  cohort_id <- match(cohort_labels, cohort_order)
  # Keep display rows intact; model history and previous-dose checks use cohort order.
  history_order <- order(cohort_id, seq_len(n_rows), na.last = TRUE)
  next_cohort_label <- if (length(cohort_numbers) && all(is.finite(cohort_numbers))) {
    as.character(max(cohort_numbers) + 1)
  } else {
    candidate <- as.character(length(cohort_order) + 1L)
    while (candidate %in% cohort_labels) candidate <- paste0(candidate, "+1")
    candidate
  }

  out <- rows
  # The checked outcome (exactly 0, 1 or NA as an integer), whatever type was supplied.
  out$DLT <- dlt_entered
  out$Cohort <- cohort_labels
  out$Cohort_Filled <- cohort_fill$filled
  out$Administered_Dose <- ifelse(treated, actual_given, suppressWarnings(as.numeric(rows$Administered_Dose)))
  out$Standardized_Dose_Given <- std_given
  out$Dose_Level_Given <- level_given
  out$Recommended_Dose <- recorded_rec
  out$Recommended_Filled <- FALSE
  out$Recommended_Standardized_Dose <- NA_real_
  out$Check_Recommended_Dose <- NA_real_
  out$Alpha_Used <- recorded_alpha
  out$Alpha_Filled <- FALSE
  out$Check_Alpha <- NA_real_
  out$Suggested_Next_Alpha <- NA_real_
  out$EWOC_xx <- NA_real_
  out$Posterior_Pr_Dose1_gt_theta <- NA_real_
  out$Decision <- NA_character_
  out$Used_In_Model <- FALSE
  out$Warning <- ""
  add_warning <- function(i, text) {
    out$Warning[i] <<- if (nzchar(out$Warning[i])) paste(out$Warning[i], text, sep = "; ") else text
  }
  omitted_note <- " - this row was omitted from the calculation"
  for (i in which(!treated & !empty_row)) {
    dose_value <- suppressWarnings(as.numeric(rows$Administered_Dose[i]))
    if (!is.finite(dose_value)) {
      add_warning(i, "Dose given is missing - row not used")
    } else if (!dose_ok[i]) {
      add_warning(i, conduct_invalid_dose_message(dose_value, design, dose_maps[[i]]$message))
    }
    if (status_conflict[i]) {
      add_warning(i, paste0("Error: the row is marked both ", conflict_label[i], " and Unevaluable - DLT Status must be one of DLT, No DLT or Unevaluable", omitted_note))
    }
    if (status_unknown[i]) {
      add_warning(i, paste0("Error: DLT Status \"", status_text[i], "\" is not recognized - enter DLT, No DLT or Unevaluable", omitted_note))
    }
  }
  # Each patient should appear once: flag duplicate Patient IDs (the rows are still used).
  patient_id <- if ("Patient" %in% names(rows)) trimws(as.character(rows$Patient)) else rep(NA_character_, n_rows)
  has_id <- !empty_row & !is.na(patient_id) & nzchar(patient_id)
  duplicate_id <- has_id & patient_id %in% patient_id[has_id][duplicated(patient_id[has_id])]
  for (i in which(duplicate_id)) {
    add_warning(i, paste0("Duplicate Patient ID (", patient_id[i], ") - each patient should appear only once; please check the input"))
  }
  out$Decision[empty_row] <- "Empty row"

  leadin_active <- accelerated
  ewoc_count <- 0L
  alpha_previous <- NA_real_
  trial_stop <- NULL
  posterior_table <- NULL
  cohort_alpha <- rep(NA_real_, length(cohort_order) + 1L)
  cohort_leadin <- rep(FALSE, length(cohort_order))
  cohort_summary <- list()

  # Recommendation for cohort k from the data of cohorts 1..k-1 (k = number of cohorts + 1 is the next cohort).
  recommend_for <- function(k) {
    if (k == 1L) {
      return(list(status = "ok", action = "Starting dose", std = start_std, actual = start_actual, alpha = NA_real_))
    }
    prior <- history_order[!empty_row[history_order] & is.finite(cohort_id[history_order]) & cohort_id[history_order] < k]
    used <- prior[evaluable[prior]]
    treated_prior <- prior[treated[prior]]
    # Efficiency count: patients with a DLT result. Pending patients are checked separately (warning only).
    counted_prior <- treated_prior[evaluable[treated_prior]]
    pending_prior <- treated_prior[!evaluable[treated_prior] & !unevaluable[treated_prior]]
    X <- std_given[used]
    Y <- dlt[used]
    if (leadin_active) {
      previous <- which(!empty_row & cohort_id == k - 1L & treated)
      any_dlt <- any(dlt[previous] == 1L & evaluable[previous], na.rm = TRUE)
      previous_level <- if (length(previous)) max(level_given[previous], na.rm = TRUE) else design$startdose - 1L
      all_resolved <- length(previous) > 0L && all(evaluable[previous])
      if (!any_dlt && previous_level < cap_level) {
        if (all_resolved) {
          level <- min(previous_level + 1L, cap_level)
          action <- "Accelerated escalation"
        } else {
          level <- max(previous_level, design$startdose)
          action <- "Accelerated escalation - repeat dose (waiting for an evaluable result)"
        }
        return(list(status = "ok", action = action, std = design$doselevels[level],
                    actual = design$actual_dose_levels[level], alpha = NA_real_, leadin = TRUE))
      }
      leadin_active <<- FALSE
    }
    if (length(Y) == 0L) {
      return(list(status = "waiting", action = "Waiting for DLT results", std = NA_real_, actual = NA_real_, alpha = NA_real_))
    }
    ewoc_count <<- ewoc_count + 1L
    alpha_value <- if (ewoc_count == 1L || !is.finite(alpha_previous)) {
      lower_alpha
    } else {
      rate <- compute_dlt_percentage(Y, X)
      if (is.finite(rate) && is.finite(alpha_cutoff) && rate < alpha_cutoff) min(alpha_previous + increment, 0.5) else alpha_previous
    }
    alpha_value <- max(lower_alpha, min(alpha_value, 0.5))
    alpha_previous <<- alpha_value
    ss <- with_local_seed(seed + k, fit_trial_posterior(model_string, Y, X, opt1, theta, length(Y), 1L, as.integer(num(settings$burn, 1000)), as.integer(num(settings$mm, 1000))))
    safety <- safety_stop_check(ss, theta, deltastop)
    xx <- min(1, max(0, as.numeric(stats::quantile(ss$mtd, alpha_value, na.rm = TRUE))))
    posterior_grid <- if (is_continuous) seq(0, 1, by = 0.25) else design$doselevels
    posterior <- data.frame(
      Actual_Dose = vapply(posterior_grid, conduct_actual_from_standardized, numeric(1), design = design),
      Standardized_Dose = posterior_grid,
      Posterior_Mean_DLT = round(vapply(posterior_grid, function(z) mean(compute_pdlt(ss$rho0, ss$rho1, X = z), na.rm = TRUE), numeric(1)), 4),
      Posterior_Pr_DLT_gt_theta = round(vapply(posterior_grid, function(z) mean(compute_pdlt(ss$rho0, ss$rho1, X = z) > theta, na.rm = TRUE), numeric(1)), 4),
      check.names = FALSE
    )
    if (!is_continuous) posterior$Dose_Level <- seq_along(posterior_grid)
    base <- list(status = "ok", alpha = alpha_value, xx = xx, posterior = posterior,
                 post_dose1 = safety$probability, n_used = length(Y))
    if (isTRUE(safety$stop)) {
      return(c(base, list(
        action = "Stop Trial", std = NA_real_, actual = NA_real_, stop_type = "lowest_dose_too_toxic",
        stop_reason = paste0(
          "Posterior probability that DLT at the minimum dose",
          " exceeds theta is ", format_dose_value(safety$probability),
          ", which is greater than Delta = ", format_dose_value(deltastop), ". No dose is recommended as the MTD."
        )
      )))
    }
    current_dose <- if (length(treated_prior)) std_given[tail(treated_prior, 1L)] else start_std
    if (is_continuous) {
      rec_std <- recommend_next_continuous_dose(ss, alpha_value, current_dose, max_increment_std)$dose_value
      rec_actual <- conduct_actual_from_standardized(rec_std, design)
    } else {
      rec <- recommend_conduct_categorical_dose(ss, alpha_value, design, current_dose, std_given[treated_prior])
      rec_std <- rec$dose_value
      rec_actual <- rec$actual_dose
    }
    if (efficiency_on && efficiency_stop_check(std_given[counted_prior], rec_std, max_patients, if (is_continuous) width else 0)) {
      return(c(base, list(
        action = "Stop Trial", std = rec_std, actual = rec_actual, stop_type = "efficiency",
        stop_reason = paste0(
          "At least ", format_dose_value(max_patients), " patients have already been treated at the recommended dose (",
          format_dose_value(rec_actual), "). The trial stops for efficiency; the MTD is estimated from the data."
        )
      )))
    }
    pending_efficiency <- NULL
    if (efficiency_on && length(pending_prior) &&
        efficiency_stop_check(std_given[c(counted_prior, pending_prior)], rec_std, max_patients, if (is_continuous) width else 0)) {
      at_dose <- function(idx) sum(abs(std_given[idx] - rec_std) <= (if (is_continuous) width / 2 else 0) + tolerance)
      pending_efficiency <- list(dose = rec_actual, n_evaluable = at_dose(counted_prior), n_pending = at_dose(pending_prior))
    }
    c(base, list(action = "Recommend Dose", std = rec_std, actual = rec_actual, pending_efficiency = pending_efficiency))
  }

  n_cohorts <- length(cohort_order)
  for (k in seq_len(n_cohorts)) {
    members <- which(!empty_row & cohort_id == k)
    if (!is.null(trial_stop)) {
      out$Decision[members] <- "Entered after trial stop - not used"
      for (i in members) add_warning(i, "Entered after the trial stopped - not used")
      next
    }
    previous_treated <- history_order[!empty_row[history_order] & !is.na(cohort_id[history_order]) &
                                       cohort_id[history_order] < k & treated[history_order]]
    rec <- tryCatch(recommend_for(k), error = function(e) list(status = "error", message = paste("Posterior fitting failed:", conditionMessage(e))))
    if (identical(rec$status, "error")) {
      return(list(status = "error", message = rec$message, rows = out, posterior_table = posterior_table))
    }
    if (!is.null(rec$posterior)) posterior_table <- rec$posterior
    cohort_alpha[k] <- rec$alpha
    cohort_leadin[k] <- isTRUE(rec$leadin) || identical(rec$action, "Starting dose") && accelerated
    if (identical(rec$action, "Stop Trial")) {
      trial_stop <- list(type = rec$stop_type, reason = rec$stop_reason, cohort = cohort_order[k], probability = rec$post_dose1)
      out$Check_Recommended_Dose[members] <- rec$actual
      out$Check_Alpha[members] <- rec$alpha
      out$Posterior_Pr_Dose1_gt_theta[members] <- rec$post_dose1
      out$Decision[members] <- paste("Trial stopped before this cohort:", rec$stop_reason, "Rows not used.")
      for (i in members) add_warning(i, "Entered after the trial stopped - not used")
      next
    }
    # N counts patients with a DLT result (pending and unevaluable patients are not counted).
    previous_counted <- previous_treated[evaluable[previous_treated]]
    if (is.finite(total_n) && length(previous_counted) >= total_n) {
      for (i in members) add_warning(i, "Patient entered beyond the maximum sample size N - update N or re-check the input")
    }
    for (i in members) {
      out$Check_Recommended_Dose[i] <- rec$actual
      out$Recommended_Standardized_Dose[i] <- rec$std
      out$Check_Alpha[i] <- rec$alpha
      out$EWOC_xx[i] <- if (is.null(rec$xx)) NA_real_ else rec$xx
      out$Posterior_Pr_Dose1_gt_theta[i] <- if (is.null(rec$post_dose1)) NA_real_ else rec$post_dose1
      out$Decision[i] <- rec$action
      out$Used_In_Model[i] <- evaluable[i]
      if (!is.finite(recorded_rec[i]) && is.finite(rec$actual)) {
        out$Recommended_Dose[i] <- rec$actual
        out$Recommended_Filled[i] <- TRUE
      } else if (is.finite(recorded_rec[i]) && is.finite(rec$actual) && !same_dose(recorded_rec[i], rec$actual)) {
        add_warning(i, paste0("Recorded recommended dose (", format_dose_value(recorded_rec[i]),
                              ") differs from the recalculated recommendation (", format_dose_value(rec$actual), ")"))
      }
      if (is.finite(rec$alpha)) {
        if (!is.finite(recorded_alpha[i])) {
          out$Alpha_Used[i] <- rec$alpha
          out$Alpha_Filled[i] <- TRUE
        } else if (abs(recorded_alpha[i] - rec$alpha) > 1e-6) {
          add_warning(i, paste0("\u03b1 entered (", format_dose_value(recorded_alpha[i]),
                                ") differs from the correct \u03b1 (", format_dose_value(rec$alpha), ")"))
        }
      } else if (is.finite(recorded_alpha[i]) && (identical(rec$action, "Starting dose") || isTRUE(rec$leadin))) {
        out$Alpha_Used[i] <- NA_real_
        add_warning(i, paste0("\u03b1 entered (", format_dose_value(recorded_alpha[i]),
                              ") is not used for the starting dose or the accelerated lead-in - set to NA"))
      }
      if (treated[i] && is.finite(out$Recommended_Dose[i]) && !same_dose(actual_given[i], out$Recommended_Dose[i])) {
        add_warning(i, paste0("Dose given (", format_dose_value(actual_given[i]), ") differs from the recorded recommended dose (",
                              format_dose_value(out$Recommended_Dose[i]), ") - please re-check the input"))
      }
      if (treated[i] && isTRUE(rec$leadin) && k > 1L) {
        previous_leadin <- which(!empty_row & cohort_id == k - 1L & treated)
        previous_level <- if (length(previous_leadin)) max(level_given[previous_leadin], na.rm = TRUE) else NA_integer_
        if (is.finite(previous_level) && is.finite(level_given[i]) && level_given[i] > previous_level + 1L) {
          add_warning(i, "A dose level was skipped during the accelerated lead-in - please re-check the input")
        }
      }
      if (treated[i] && is_continuous && length(previous_treated)) {
        last_dose <- std_given[tail(previous_treated, 1L)]
        if (std_given[i] > last_dose + max_increment_std + tolerance) {
          add_warning(i, "Dose given exceeds the maximum dose increment from the previous dose - please re-check the input")
        }
      }
      if (treated[i] && !unevaluable[i] && !is.finite(dlt[i])) {
        out$Decision[i] <- paste0(rec$action, " (DLT result pending - not used in the model yet)")
      } else if (unevaluable[i]) {
        out$Decision[i] <- paste0(rec$action, " (unevaluable - not used in the model or the efficiency count)")
      }
    }
    cohort_summary[[length(cohort_summary) + 1L]] <- data.frame(
      Cohort = cohort_order[k], Patients = length(members), Recommended_Dose = rec$actual,
      Alpha = rec$alpha, Decision = rec$action, check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  # Incomplete cohorts: a cohort used so far with fewer evaluable patients than the minimum cohort size.
  # Only patients with a DLT result are used in the model, so the next recommendation and alpha are based
  # on incomplete data and may change once the pending results are entered.
  # The cohort still being enrolled (fewer patients than the cohort size) is covered by its own message,
  # and accelerated lead-in cohorts (one patient, no alpha) are not checked.
  incomplete <- list()
  for (k in seq_len(n_cohorts)) {
    members <- which(!empty_row & cohort_id == k & treated)
    if (!length(members) || cohort_leadin[k] || any(grepl("after trial stop|stopped before this cohort", out$Decision[members]))) next
    n_eval <- sum(evaluable[members])
    if (n_eval >= cohort_size) next
    if (k == n_cohorts && length(members) < cohort_size) next
    n_pend <- sum(!evaluable[members] & !unevaluable[members])
    n_unev <- sum(unevaluable[members])
    still_needed <- cohort_size - n_eval - n_pend
    counts <- c(if (n_pend > 0L) paste0(n_pend, " pending"), if (n_unev > 0L) paste0(n_unev, " unevaluable"))
    message_k <- paste0(
      "Incomplete cohort data: cohort ", cohort_order[k], " has ", n_eval, " of ", cohort_size, " evaluable patient",
      if (cohort_size > 1L) "s" else "", if (length(counts)) paste0(" (", paste(counts, collapse = ", "), ")") else "", ". ",
      "Only patients with a DLT result are used in the model, so the recommendation and \u03b1 for the next cohort are based on incomplete data. ",
      if (n_pend > 0L) "The recommendation for the next cohort may change after the pending results are entered; re-evaluate before enrolling. ",
      if (n_unev > 0L) "Unevaluable patients do not count and are usually replaced. ",
      if (still_needed > 0L) paste0("This cohort needs ", still_needed, " more evaluable patient", if (still_needed > 1L) "s" else "",
                                    " to reach the minimum cohort size of ", cohort_size, ". "),
      "Per protocol, evaluate the next cohort only after at least ", cohort_size, " patient", if (cohort_size > 1L) "s" else "",
      " in each cohort ", if (cohort_size > 1L) "are" else "is", " evaluable."
    )
    incomplete[[length(incomplete) + 1L]] <- data.frame(
      Cohort = cohort_order[k], Minimum = cohort_size, Evaluable = n_eval, Pending = n_pend, Unevaluable = n_unev,
      Message = message_k, check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  incomplete_cohorts <- if (length(incomplete)) do.call(rbind, incomplete) else NULL

  used_rows <- which(out$Used_In_Model)
  treated_used <- which(treated & !empty_row & !grepl("after trial stop|stopped before this cohort", out$Decision))
  # N counts patients with a DLT result; pending and unevaluable patients are reported separately.
  counted_used <- treated_used[evaluable[treated_used]]
  pending_used <- treated_used[!evaluable[treated_used] & !unevaluable[treated_used]]
  messages <- character(0)
  next_recommendation <- NULL
  pending_efficiency_warning <- NULL
  if (is.null(trial_stop)) {
    if (is.finite(total_n) && length(counted_used) > total_n) {
      messages <- c(messages, paste0("More evaluable patients (", length(counted_used), ") were entered than the maximum sample size N = ",
                                     format_dose_value(total_n), ". Update N or re-check the input."))
    } else if (is.finite(total_n) && length(counted_used) >= total_n) {
      messages <- c(messages, "The maximum sample size N has been reached. Confirm that the trial is completed to run the end-of-trial report.")
    } else {
      rec <- tryCatch(recommend_for(n_cohorts + 1L), error = function(e) list(status = "error", message = paste("Posterior fitting failed:", conditionMessage(e))))
      if (identical(rec$status, "error")) {
        return(list(status = "error", message = rec$message, rows = out, posterior_table = posterior_table))
      }
      if (!is.null(rec$posterior)) posterior_table <- rec$posterior
      cohort_alpha[n_cohorts + 1L] <- rec$alpha
      if (identical(rec$status, "waiting")) {
        messages <- c(messages, "No DLT results are available yet; the next recommendation needs at least one evaluable patient.")
      } else {
        next_recommendation <- list(
          status = "ok", action = rec$action, dose = rec$actual, standardized_dose = rec$std,
          xx = rec$xx, posterior = rec$posterior, post_dose1 = rec$post_dose1,
          stop_type = if (is.null(rec$stop_type)) NA_character_ else rec$stop_type,
          stop_reason = if (is.null(rec$stop_reason)) NA_character_ else rec$stop_reason,
          cohort = paste("Cohort", next_cohort_label)
        )
        if (identical(rec$action, "Stop Trial")) {
          trial_stop <- list(type = rec$stop_type, reason = rec$stop_reason, cohort = "next", probability = rec$post_dose1)
        }
        if (!is.null(rec$pending_efficiency)) {
          pe <- rec$pending_efficiency
          pending_efficiency_warning <- paste0(
            "Pending DLT status: the efficiency rule would be triggered in this trial if the ", pe$n_pending,
            " patient(s) still waiting for a DLT result at the recommended dose (", format_dose_value(pe$dose),
            ") were counted (", pe$n_evaluable, " evaluable + ", pe$n_pending, " pending >= ", format_dose_value(max_patients),
            "). Re-evaluate once their DLT status is known; if they are evaluable, the trial stops for efficiency."
          )
        }
      }
    }
    last_members <- which(!empty_row & cohort_id == n_cohorts)
    if (n_cohorts > 0L && length(last_members) < cohort_size && is.null(trial_stop) && !leadin_active) {
      last_rec <- out$Check_Recommended_Dose[last_members[1]]
      messages <- c(messages, paste0(
        "Cohort ", cohort_order[n_cohorts], " has ", length(last_members), " of at least ", cohort_size,
        " patients; further patients in this cohort receive its recommended dose (", format_dose_value(last_rec), ")."
      ))
    }
  } else {
    next_recommendation <- list(status = "ok", action = "Stop Trial", dose = NA_real_, standardized_dose = NA_real_,
                                stop_type = trial_stop$type, stop_reason = trial_stop$reason)
  }
  if (!is.null(trial_stop)) {
    next_recommendation$stop_type <- trial_stop$type
    next_recommendation$stop_reason <- trial_stop$reason
  }
  for (k in seq_len(n_cohorts)) {
    members <- which(!empty_row & cohort_id == k)
    out$Suggested_Next_Alpha[members] <- cohort_alpha[k + 1L]
  }
  out$Decision[!treated & !empty_row] <- "Omitted from the calculation (see Warning)"
  warning_rows <- which(nzchar(out$Warning))
  warnings <- if (length(warning_rows)) {
    paste0("Cohort ", out$Cohort[warning_rows], ", patient ", rows$Patient[warning_rows], ": ", out$Warning[warning_rows])
  } else {
    character(0)
  }

  list(
    status = "ok",
    rows = out,
    cohort_summary = if (length(cohort_summary)) do.call(rbind, cohort_summary) else NULL,
    posterior_table = posterior_table,
    next_recommendation = next_recommendation,
    trial_stop = trial_stop,
    warnings = warnings,
    messages = messages,
    observed_dlt_rate = compute_dlt_percentage(dlt[used_rows], std_given[used_rows]),
    alpha_for_next = cohort_alpha[n_cohorts + 1L],
    n_patients = length(used_rows),
    n_treated = length(counted_used),
    n_pending = length(pending_used),
    n_unevaluable = length(treated_used) - length(counted_used) - length(pending_used),
    pending_efficiency_warning = pending_efficiency_warning,
    incomplete_cohorts = incomplete_cohorts,
    leadin_active = leadin_active,
    trial_mode = if (accelerated) "Accelerated EWOC" else "Standard EWOC",
    conduct_design = design
  )
}

# Error text for a dose given that is not in the protocol (discrete) or outside its range (continuous).
conduct_invalid_dose_message <- function(dose_value, design, fallback, omit = TRUE) {
  if (!omit && !is.finite(suppressWarnings(as.numeric(dose_value)))) {
    return(paste0("Invalid dose entered (", dose_value, "): enter a finite numeric dose."))
  }
  if (is.null(fallback) || !grepl("^Dose given must", fallback)) return(fallback)
  omitted <- if (omit) " - this row was omitted from the calculation" else ""
  if (identical(design$dose_type, "Continuous")) {
    return(paste0("Error: dose given (", format_dose_value(dose_value), ") is outside the protocol dose range (",
                  format_dose_value(design$min_dose), " to ", format_dose_value(design$max_dose), ")", omitted))
  }
  allowed <- paste(vapply(design$actual_dose_levels, format_dose_value, character(1)), collapse = ", ")
  paste0("Error: dose given (", format_dose_value(dose_value), ") is not one of the doses listed in the protocol (",
         allowed, ")", omitted)
}

