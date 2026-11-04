phase1_references <- function() c(
  "Babb, J., A. Rogatko, and S. Zacks, Cancer phase I clinical trials: efficient dose escalation with overdose control. Stat Med, 1998. 17(10): p. 1103-20.",
  "Tighiouart, M., A. Rogatko, and J.S. Babb, Flexible Bayesian methods for cancer phase I clinical trials. Dose escalation with overdose control. Stat Med, 2005. 24(14): p. 2183-96.",
  "Tighiouart, M. and A. Rogatko, Dose Finding with Escalation with Overdose Control (EWOC) in Cancer Clinical Trials. Statistical Science, 2010. 25(2): p. 217-226.",
  "Chu, P.L., Y. Lin, and W.J. Shih, Unifying CRM and EWOC designs for phase I cancer clinical trials. Journal of Statistical Planning and Inference, 2009. 139(3): p. 1146-1163.",
  "Tighiouart M., Cook-Wiens G, Rogatko A. A Bayesian Adaptive Design for Cancer Phase I Trials Using a Flexible Range of Doses. Journal of Biopharmaceutical Statistics, 2018. 28: p. 562-574.",
  "Wheeler, G. M., Sweeting, M. J., and Mander, A. P. Toxicity-dependent feasibility bounds for the escalation with overdose control approach in phase I cancer trials. Statistics in Medicine, 2017. 36: p. 2499-2513."
)

phase1_value <- function(x, fallback = "NOT AVAILABLE") {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) return(fallback)
  as.character(x[1])
}

# Rounds a numeric value to 4 decimals and formats it for display, matching
# b1, a2, b2, a3, b3) on the interactive Prior tab (round(x, 4)).
phase1_round4 <- function(x, fallback = "NOT AVAILABLE") {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return(fallback)
  format(round(x, 4), nsmall = 0, trim = TRUE)
}

phase1_percent <- function(x, digits = 1, fallback = "NOT AVAILABLE") {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) return(fallback)
  paste0(format(round(x * 100, digits), nsmall = digits, trim = TRUE), "%")
}

phase1_true_mtd_index <- function(probabilities, theta) {
  probabilities <- suppressWarnings(as.numeric(probabilities))
  theta <- suppressWarnings(as.numeric(theta)[1])
  if (!length(probabilities) || !is.finite(theta) || any(!is.finite(probabilities))) return(NA_integer_)
  which.min(abs(probabilities - theta))
}

phase1_scenario_classification <- function(probabilities, theta) {
  probabilities <- suppressWarnings(as.numeric(probabilities))
  theta <- suppressWarnings(as.numeric(theta)[1])
  if (!length(probabilities) || !is.finite(theta) || any(!is.finite(probabilities))) {
    return(list(type = "invalid", dose_level = NA_integer_))
  }
  if (all(probabilities > theta)) {
    return(list(type = "toxic", dose_level = NA_integer_))
  }
  exact <- which(abs(probabilities - theta) <= sqrt(.Machine$double.eps))
  if (length(exact)) {
    return(list(type = "mtd", dose_level = exact[1]))
  }
  list(type = "closest", dose_level = which.min(abs(probabilities - theta)))
}

## Phase 1 text: whether a design setting is switched on ("Yes", TRUE).
phase1_is_on <- function(value) {
  isTRUE(value) || identical(value, "Yes")
}

## Phase 1 text: the safety (Delta) stopping sentence.
phase1_safety_stop_text <- function(x, lowest_dose_text) {
  paste0(
    "The study will be stopped for safety if the posterior probability that the DLT probability at ", lowest_dose_text,
    " exceeds \u03B8 is greater than ", x$deltastop, ". This rule is checked each time a dose recommendation is calculated ",
    "and once more on the data at the end of the study. If the study is stopped for safety, no dose will be declared the MTD."
  )
}

## Phase 1 text: the accelerated EWOC lead-in (discrete doses only).
phase1_accelerated_text <- function(x) {
  if (!identical(x$trial_mode, "Accelerated EWOC")) {
    return(NULL)
  }
  paste0(
    "An accelerated lead-in will be used: one participant is treated at a time, starting at dose level ", x$startdose,
    " and escalating by one dose level per participant, with no dose level skipped, until the first DLT is observed or dose level ",
    x$max_accel_dose_level, " (the maximum dose level for acceleration) has been given. No model-based decision is made during the lead-in. ",
    "EWOC then uses all available data to determine the dose for the next cohort, starting from \u03B1 = ", x$lower_f_bound,
    ". Participants treated in the lead-in count toward the maximum sample size."
  )
}

phase1_trial_design_paragraphs <- function(x) {
  alpha_increment_bound_text <- if (isTRUE(x$use_custom_alpha_increment_cutoff)) {
    phase1_percent(x$alpha_increment_cutoff)
  } else {
    "\u03B8"
  }
  paragraphs <- c(
    paste0(
      "Escalation with overdose control (EWOC) [1,3] design will be used for dose escalation. A maximum of n = ", x$NN,
      " evaluable participants will be enrolled. The MTD is defined as the dose level at which the probability of dose-limiting toxicity (DLT) is \u03B8 = ", x$theta,
      ". The DLT period is DEFINE DLT PERIOD HERE. Consecutive cohorts of at least ", x$cohort,
      " participants will be enrolled. The dose allocated to the next cohort will be determined so that, based on all available data, ",
      "the probability that it exceeds the MTD equals a feasibility bound \u03B1. This will begin with \u03B1 = ", x$lower_f_bound,
      " and increase in increments of ", x$increment, " after each cohort until \u03B1 = 0.5. No \u03B1 increment will be used if the cumulative observed DLT rate ",
      "(total DLTs divided by the total number of evaluable participants) is greater than or equal to ", alpha_increment_bound_text,
      " (Wheeler, Sweeting, and Mander, 2017). No untried dose level will be skipped."
    ),
    phase1_accelerated_text(x),
    phase1_safety_stop_text(x, "dose level 1"),
    if (isTRUE(x$stopping_rule_selected)) paste0(
      "The study will be stopped for efficiency if ", x$num_patients_restrict,
      " evaluable participants have already been treated at the dose level recommended for the next cohort."
    ) else NULL,
    paste0(
      "At the end of the study, the MTD will be estimated as the median of the posterior distribution of the MTD, rounded to the nearest available dose level. ",
      "If the trial stops for efficiency, any remaining participants from the planned n = ", x$NN,
      " participants may be treated at the recommended MTD according to the protocol."
    )
  )
  paragraphs
}

continuous_phase1_trial_design_paragraphs <- function(x) {

  alpha_increment_bound_text <- if (
    isTRUE(x$use_custom_alpha_increment_cutoff)
  ) {
    phase1_percent(x$alpha_increment_cutoff)
  } else {
    "\u03B8"
  }
  max_increment <- suppressWarnings(as.numeric(x$max_dose_increment))
  width <- suppressWarnings(as.numeric(x$efficiency_stop_tolerance))

  paragraphs <- c(
    paste0(
      "Escalation with overdose control (EWOC) [1,3] design will be used for dose escalation. ",
      "A maximum of n = ", x$NN,
      " evaluable participants will be enrolled. ",
      "The MTD is defined as the dose value at which the probability of dose-limiting toxicity (DLT) is \u03B8 = ", x$theta, ". ",
      "The DLT period is DEFINE DLT PERIOD CYCLES. ",
      "The first cohort will be treated at the minimum dose. Consecutive cohorts of at least ", x$cohort,
      " participants will be enrolled, and the dose value allocated to the next cohort will be determined so that, based on all available data, ",
      "the probability that it exceeds the MTD equals a feasibility bound \u03B1. ",
      "This will begin with \u03B1 = ", x$lower_f_bound,
      " and increase in small increments of ", x$increment,
      " after each cohort until \u03B1 = 0.5. ",
      "This value is a compromise between the therapeutic aspect of the agent and its toxic side effects. ",
      "Chu et al. [4] showed that the EWOC design with varying feasibility bound provides better protection in limiting higher doses for participants than four CRM designs with a similar convergence rate. ",
      "No \u03B1 increment will be used if the cumulative observed DLT rate (total DLTs divided by the total number of evaluable participants) is greater than or equal to ",
      alpha_increment_bound_text, ". ",
      if (is.finite(max_increment)) {
        paste0("The dose will not be increased by more than ", max_increment, "% of the dose range from the previous dose. ")
      } else {
        ""
      },
      "At any time during the study, a participant may be enrolled at a given dose if no more than ", x$cohort,
      " participants have unresolved DLT status at that dose."
    ),

    phase1_safety_stop_text(x, "the minimum dose"),

    if (isTRUE(x$stopping_rule_selected)) {
      paste0(
        "The study will be stopped for efficiency if ", x$num_patients_restrict,
        " evaluable participants have already been treated at the dose recommended for the next cohort",
        if (is.finite(width)) paste0(" (doses within \u00B1", width / 2, "% of the dose range of it count as the same dose)") else "",
        "."
      )
    } else {
      NULL
    },

    paste0(
      "At the end of the study, the MTD will be estimated as the median of the posterior distribution of the MTD. ",
      "In the event that the trial stops for efficiency, any remaining participants from the planned n = ", x$NN,
      " participants will be treated at the MTD."
    )
  )

  paragraphs
}

phase1_operating_paragraphs <- function(x, prior_data, scenarios) {
  opt1 <- prior_data$opt1
  intro <- paste0(
    "The MTD is defined as the dose level at which the probability of DLT is \u03b8 = ", x$theta,
    ". An extension of the EWOC [1,3] algorithm [5] was used. This design allows the true MTD to be below the minimum dose level or above the maximum dose level with positive a priori probability. ",
    "The probability of DLT is modeled as P(DLT) = F(\u03b2\u2080 + \u03b2\u2081x), where F(u) = (1 + exp(-u))\u207b\u00b9. The model is reparameterized using \u03c1\u2080 and \u03c1\u2081, ",
    "the probabilities of DLT at the minimum and maximum doses. The prior probabilities that the MTD is below dose level 1 and above dose level ", x$num_doses,
    " are ", x$p0, " and ", x$p1, ". The fitted prior distributions are \u03c1\u2081 ~ beta(", phase1_value(opt1$a0), ", ", phase1_value(opt1$b0),
    ") and \u03c1\u2080/\u03c1\u2081 ~ beta(", phase1_value(opt1$a1), ", ", phase1_value(opt1$b1), ")."
  )
  setup <- paste0(
    "The algorithm treats cohorts of ", x$cohort, " participants at a time. Operating characteristics were derived using ", x$M,
    " simulated trial replicates for each of ", length(scenarios), " scenario(s)."
  )
  table_description <- paste0(
    "Table X shows ", length(scenarios), " scenarios for the true MTD (in bold). The summary statistics for trial efficiency include ",
    "the percent of MTD selection, percent of participants allocated to each dose level, the average DLT rate across all trials, ",
    "percent of trials with an excessive rate of DLTs, defined as the percent of trials with a DLT rate exceeding \u03b8 + 0.1, ",
    "and the average sample size."
  )
  summaries <- vapply(seq_along(scenarios), function(i) {
    s <- scenarios[[i]]$summary
    classification <- phase1_scenario_classification(s$inputs$doseprobs, x$theta)
    scenario_label <- phase1_value(scenarios[[i]]$label, paste("Scenario", i))
    if (identical(classification$type, "toxic")) {
      highest_dose <- nrow(s$combined_table)
      return(paste0(
        "For ", scenario_label, ", the true MTD is below the current minimum dose level. The trial stops with probability ",
        phase1_value(round(s$pstop_delta, 3)), " based on \u0394, and only ",
        phase1_value(s$combined_table$`% Allocation`[highest_dose]),
        "% of participants are allocated to the highest toxic dose level ", highest_dose, "."
      ))
    }
    idx <- classification$dose_level
    mtd_wording <- if (identical(classification$type, "mtd")) "is the MTD" else "is closest to the MTD"
    efficiency_text <- if (isTRUE(x$stopping_rule_selected)) {
      paste0(" The probability of stopping early for efficiency is ", phase1_value(round(s$pstop_rule, 3)), ".")
    } else ""
    paste0(
      "For ", scenario_label, ", dose level ", idx, " ", mtd_wording, ". The MTD selection percentage is ",
      phase1_value(s$combined_table$`% MTD recommendation`[idx]), "%, with ",
      phase1_value(s$combined_table$`% Allocation`[idx]), "% of participants allocated to that dose. The average sample size (95% CI) is ",
      phase1_value(s$overall_avg_n_ci_text), ".", efficiency_text
    )
  }, character(1))
  c(intro, setup, table_description, paste(summaries, collapse = " "))
}

continuous_phase1_operating_paragraphs <- function(
    x,
    prior_data,
    scenarios
) {
  opt1 <- prior_data$opt1
  
  intro <- paste0(
    "The MTD is defined as the dose value at which the probability of DLT at the MTD is \u03b8 = ", x$theta,
    ". An extension of EWOC [1,3] algorithm [5] was used for this study. ",
    "This design allows the true MTD to be below the minimum dose or above the maximum dose value with positive a priori probability. ",
    "The probability of DLT is modeled as P(DLT) = F(\u03b2\u2080 + \u03b2\u2081x), where x is the standardized dose ranging from 0 to 1 and F(u) = (1 + exp(-u))\u207b\u00b9. ",
    "The model is reparameterized in terms of \u03c1\u2080 and \u03c1\u2081, the probabilities of DLT at the minimum and maximum dose, respectively. ",
    "It is assumed, a priori, that the probabilities that the MTD is below the minimum dose value and above the maximum dose value are p0 and p1, respectively. ",
    "This implies that the prior distributions of \u03c1\u2080 and \u03c1\u2081 are ",
    "\u03c1\u2081 ~ beta(",
    opt1$a0,
    ", ",
    opt1$b0,
    ") and, given \u03c1\u2081, \u03c1\u2080/\u03c1\u2081 ~ beta(",
    opt1$a1,
    ", ",
    opt1$b1,
    ")."
  )
  
  setup <- paste0(
    "The algorithm proceeds as described in the protocol, treating cohorts of ",
    x$cohort,
    " participants at a time. ",
    "The operating characteristics were evaluated from M = ",
    x$M,
    " simulated trial replicates under ",
    length(scenarios),
    " simulation scenario(s) defined by the true MTD and the probability of DLT at the minimum dose."
  )
  
  table_description <- paste0(
    "Table X shows ",
    length(scenarios),
    " scenarios for the true MTD and the corresponding probability of DLT at the minimum dose. ",
    "The summary statistics for trial efficiency include the average estimated MTD, average bias, RMSE, percent selection within 10% of theta, ",
    "and number allocated to toxic doses. ",
    "The summary statistics for trial safety include the average DLT rate across all trials, percent of trials with an excessive rate of DLTs, ",
    "defined as the percent of trials with a DLT rate exceeding \u03b8 + 0.1, the average sample size, ",
    "and the percent of trials stopped for safety",
    if (isTRUE(x$stopping_rule_selected)) " or for efficiency" else "",
    ", with the average sample size of stopped trials."
  )
  
  c(
    intro,
    setup,
    table_description
  )
}

build_phase1_operating_table <- function(scenarios, theta) {
  max_doses <- max(vapply(scenarios, function(z) nrow(z$summary$combined_table), integer(1)))
  dose_names <- paste("Dose", seq_len(max_doses))
  result <- list()
  true_cells <- list()
  scenario_rows <- integer(0)
  dose_header_merge_rows <- integer(0)
  summary_merge_rows <- integer(0)
  vertical_merge_ranges <- list()
  
  make_row <- function(metric = "", dose_values = rep("", max_doses), pstop = "", average_n = "") {
    values <- c(
      Metric = metric,
      stats::setNames(as.character(dose_values), dose_names),
      `P(Stop)` = pstop,
      `Average sample size (95% CI)` = average_n
    )
    as.data.frame(as.list(values), stringsAsFactors = FALSE, check.names = FALSE)
  }
  
  for (i in seq_along(scenarios)) {
    s <- scenarios[[i]]$summary
    tab <- s$combined_table
    classification <- phase1_scenario_classification(s$inputs$doseprobs, theta)
    pstop_text <- paste0(
      "Safety: ", round(s$pstop_delta, 3),
      if (isTRUE(s$inputs$stopping_rule_selected)) {
        paste0("\nEfficiency: ", round(s$pstop_rule, 3))
      } else ""
    )
    average_n <- phase1_value(s$overall_avg_n_ci_text)
    dose_values <- function(x) c(as.character(x), rep("", max_doses - length(x)))
    dose_label_col <- if ("Actual Dose" %in% names(tab)) {
      "Actual Dose"
    } else if ("Dose Level" %in% names(tab)) {
      "Dose Level"
    } else {
      names(tab)[1]
    }
    first_row <- length(result) + 1L
    result[[length(result) + 1L]] <- make_row(
      metric = phase1_value(scenarios[[i]]$label, paste("Scenario", i)),
      dose_values = c(dose_label_col, rep("", max_doses - 1L)),
      pstop = "P(Stop)", average_n = "Average sample size (95% CI)"
    )
    result[[length(result) + 1L]] <- make_row("", dose_values(tab[[dose_label_col]]), pstop_text, average_n)
    result[[length(result) + 1L]] <- make_row("True Prob DLT", dose_values(tab$Probability), pstop_text, average_n)
    result[[length(result) + 1L]] <- make_row("% MTD recommendation", dose_values(tab$`% MTD recommendation`), pstop_text, average_n)
    result[[length(result) + 1L]] <- make_row("% patients allocated", dose_values(tab$`% Allocation`), pstop_text, average_n)
    result[[length(result) + 1L]] <- make_row("Ave % DLTs", dose_values(s$dlt_summary$Value[1]), pstop_text, average_n)
    result[[length(result) + 1L]] <- make_row("% Trials: rate DLT > \u03b8 + 0.1", dose_values(s$dlt_summary$Value[2]), pstop_text, average_n)
    
    scenario_rows <- c(scenario_rows, first_row)
    dose_header_merge_rows <- c(dose_header_merge_rows, first_row)
    summary_merge_rows <- c(summary_merge_rows, first_row + 5L, first_row + 6L)
    vertical_merge_ranges[[i]] <- (first_row + 1L):(first_row + 6L)
    true_cells[[i]] <- if (!identical(classification$type, "toxic")) {
      list(
        probability_row = first_row + 2L,
        recommendation_row = first_row + 3L,
        col = 1L + classification$dose_level
      )
    } else NULL
  }
  result <- do.call(rbind, result)
  attr(result, "true_mtd_cells") <- true_cells
  attr(result, "scenario_rows") <- scenario_rows
  attr(result, "dose_header_merge_rows") <- dose_header_merge_rows
  attr(result, "summary_merge_rows") <- summary_merge_rows
  attr(result, "vertical_merge_ranges") <- vertical_merge_ranges
  attr(result, "dose_names") <- dose_names
  result
}

build_continuous_phase1_operating_table <- function(scenarios)
{
  if(is.null(scenarios) || length(scenarios) == 0L)
  {
    stop( "At least one continuous trial simulation scenario is required.", call. = FALSE )
  }
  
  format_probability_pair <- function(p_min, p_max)
  {
    p_min <- suppressWarnings(as.numeric(p_min)[1]) 
    p_max <- suppressWarnings(as.numeric(p_max)[1]) 
    if (!is.finite(p_min) || !is.finite(p_max)) { return("NOT AVAILABLE") }
    
    paste0(
      format(round(p_min, 4), nsmall = 2, trim = TRUE, scientifix = FALSE),
      ", ",
      format(round(p_max, 4), nsmall = 2, trim = TRUE, scientifix = FALSE)
    )
  }
  
  get_table_value <- function(df, column_name)
  {
    if ( is.null(df) || nrow(df) == 0L || !column_name %in% names(df) )
    {
      return("NOT AVAILABLE")
    }
    
    phase1_value(df[[column_name]])
  }
  
  get_dlt_value <- function(dlt_summary, row_number)
  {
    if ( is.null(dlt_summary) || nrow(dlt_summary) < row_number || ncol(dlt_summary) < 2L )
    {
      return("NOT AVAILABLE")
    }
    
    phase1_value(dlt_summary[[2]][row_number])
  }
  
  efficiency_rows <- lapply(seq_along(scenarios), function(i){
    scenario_item <- scenarios[[i]] 
    s <- scenario_item$summary
    if (is.null(s)) 
    { 
      stop(paste0("Scenario ", i, " does not contain a summary."), call. = FALSE) 
    }
    tab <- s$combined_table
    
    data.frame(
      Scenario = phase1_value(scenario_item$label),
      `True P(DLT) at Minimum and Maximum Dose` = format_probability_pair(s$inputs$true_p_min, s$inputs$true_p_max),
      `True MTD` = get_table_value(tab, "True MTD"),
      `Average Estimated MTD` = get_table_value(tab, "Average Estimated MTD"), 
      RMSE = get_table_value(tab, "RMSE"),
      `Average Bias` = get_table_value(tab, "Average Bias"),
      `Percent Selection Within 10% of Theta` = get_table_value( tab, "% Selection within 10% of theta" ),
      `Number Allocated to Toxic Dose (Mean, 90% CI)` = get_table_value(tab, "Number allocated to toxic doses (mean, 90% CI)"),
      stringsAsFactors = FALSE, 
      check.names = FALSE
    )
  })
  
  safety_rows <- lapply(seq_along(scenarios), function(i) {
    scenario_item <- scenarios[[i]] 
    s <- scenario_item$summary
    stopping_rule_enabled <- isTRUE(s$inputs$stopping_rule_selected)
    stopping_probability <- if (stopping_rule_enabled) 
    { 
      pstop_rule <- suppressWarnings( as.numeric(s$pstop_rule)[1])
      if(is.infinite(pstop_rule))
      {
        as.character(round(pstop_rule * 100, 3))
      } else {
        "NOT AVAILABLE"
      }
    } else {
      "Not applied"
    }
    
    stopped_sample_size <- if (stopping_rule_enabled) {
      phase1_value(s$avg_n_stop_rule_ci_text)
    } else {
      "Not applied"
    }
    
    data.frame(
      Scenario = phase1_value(scenario_item$label),
      `Average DLT Rate` = get_dlt_value(s$dlt_summary, 1L),
      `% Trials with DLT Rate > Theta + 0.1` = get_dlt_value(s$dlt_summary, 2L),
      `Average Sample Size (95% CI)` = phase1_value(s$overall_avg_n_ci_text),
      `% Trials Stopped by Maximum Patient Allocation Restriction` = stopping_probability,
      `Average Sample Size of Stopped Trials (95% CI)` = stopped_sample_size,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  
  list(
    efficiency = do.call(rbind, efficiency_rows), 
    safety = do.call(rbind, safety_rows)
  )
}

continuous_phase1_operating_table_html <- function(table_continuous_data)
{
  if (!requireNamespace("htmltools", quietly = TRUE))
  {
    stop( 
      "The htmltools package is required to create the HTML table.", call. = FALSE 
    )
  }
  
  html_escape <- function(x) { htmltools::htmlEscape(as.character(x)) }
  
  efficiency_df <- table_continuous_data$efficiency 
  safety_df <- table_continuous_data$safety
  
  if ( is.null(efficiency_df) || nrow(efficiency_df) == 0L || is.null(safety_df) || nrow(safety_df) == 0L )
  {
    return( "<p class='muted'>No continuous operating-characteristic data available.</p>" )
  }
  
  efficiency_header <- paste0(
    "<tr>",
    paste0( "<th>", 
            html_escape(names(efficiency_df)), 
            "</th>", 
            collapse = ""
    ),
    "</tr>"
  )
  
  efficiency_rows <- paste(
    vapply(seq_len(nrow(efficiency_df)), function(i) 
    {
      row_values <- vapply(seq_along(efficiency_df), function(j)
      {
        value <- html_escape(efficiency_df[[j]][i])
        
        paste0("<td>", value, "</td>")
      },
      character(1)
      )
      
      paste0( "<tr>", paste(row_values, collapse = ""), "</tr>" )
    }, character(1)),
    collapse = ""
  )
  
  safety_header <- paste0( "<tr>",
                           paste0( "<th>", 
                                   html_escape(names(safety_df)), "</th>", 
                                   collapse = "" ), 
                           "</tr>" 
  )
  
  safety_rows <- paste(
    vapply(seq_len(nrow(safety_df)), function(i) {
      row_values <- vapply(seq_along(safety_df), function(j)
      {
        paste0( "<td>", html_escape(safety_df[[j]][i]), "</td>" )
      },
      character(1)
      )
      paste0( "<tr>", paste(row_values, collapse = ""), "</tr>" )
    }, character(1)),
    collapse = ""
  )
  
  paste0(
    "<div class='continuous-operating-table'>",
    "<p class='table-title'>",
    "<strong>Table X. Continuous design operating characteristics.</strong>",
    "</p>",
    "<div class='table-scroll'>",
    "<table>", 
    "<tbody>",
    "<tr class='section-row'>", 
    "<th colspan='", ncol(efficiency_df),
    "'>Efficiency</th>", 
    "</tr>", efficiency_header, efficiency_rows,
    "<tr class='section-spacer'>",
    "<td colspan='", ncol(efficiency_df),
    "'></td>", 
    "</tr>",
    "<tr class='section-row'>",
    "<th colspan='", ncol(safety_df),
    "'>Safety</th>", 
    "</tr>", safety_header, safety_rows,
    "</tbody>", 
    "</table>", 
    "</div>", 
    "</div>"
  )
}

# ---------------------------------------------------------------------------
#
# Everything below builds the same "Phase 1 Design" Word/HTML document as
# agents, EWOC with cohorts of two, Table 1 safety summary, and Figures 1-3
# per scenario (True vs. Estimated MTD curve, pointwise average bias, pointwise
# percent of MTD recommendation). It is intentionally NOT gated on
# blocks that combination before a run ever happens), so in practice this is
# only ever reached for Continuous, but the text and tables here don't depend
# on dose type, only on FALSE.
# ---------------------------------------------------------------------------

p_block   <- function(text) list(type = "p", text = text)
img_block <- function(name) list(type = "img", name = name)
# A bold sub-heading within a section (e.g. "A. Prior and Posterior
# Distributions"), as opposed to the top-level "Trial Design"/"Operating
# Characteristics" headings added outside these blocks by the Word/HTML
# writers.
heading_block <- function(text) list(type = "heading", text = text)

# "" for a missing/blank unit, otherwise " (unit)" -- used to build axis-style
# was entered.
phase1_unit_suffix <- function(unit) {
  unit <- trimws(as.character(unit))
  if (length(unit) != 1L || is.na(unit) || !nzchar(unit)) "" else paste0(" (", unit, ")")
}

# Returns the "Trial Design" section as a list of blocks (see p_block()/
# img_block() above), reproducing the PI's original Appendix wording
# verbatim (Sections 1.1 "Dose-Toxicity Model", "A. Prior and Posterior
# Distributions", and 1.2 "Trial Design", including the stopping rule),
# with the yellow-highlighted placeholders in that document substituted by
# the app's actual values:
#   - dose ranges/unit          -> drug_a/b_min/max_dose, drug_a/b_unit
#   - theta (theta)             -> theta
#   - prior hyperparameters     -> prior_a1, prior_b1, prior_a2, prior_b2,
#                                  prior_a3, prior_b3, prior_a, prior_b,
#                                  rounded to 4 decimals (phase1_round4),
#                                  matching the interactive Prior tab
#   - zeta1, zeta2              -> zeta1, zeta2
#   - starting dose combination -> (drug_a_min_dose, drug_b_min_dose), i.e.
#                                  the actual doses corresponding to the
#                                  standardized starting point (0, 0) that
#                                  the app's simulation/conduct engine always
#                                  starts the first cohort at
#   - feasibility bound / max dose escalation -> lower_f_bound, increment,
#                                  max_increment_a, max_increment_b
#   - n, cohort size             -> NN, cohort
#
# The numbered display equations (2.1)-(2.6) and the "where G(...)" formula
# are inserted as images (img_block) rather than re-typeset, since they were
# recursive dose formulas in step 3 of the algorithm and the inline
# stopping-rule formula are reproduced as plain-text math (best-effort
# Unicode), since in the original document they sit inline mid-sentence
# rather than as a standalone display equation.
# variance (e.g. mean 21, variance 900), and the Gamma shape/rate (a, b) are
# back-solved from those -- so the Phase 1 design text should quote the
# actual eta_mean/eta_var the calibration used, not re-derive "variance =
# a/b^2" from a and b (that's not how these numbers were chosen). Falls back
# to a/b and a/b^2 only if eta_mean/eta_var weren't supplied (e.g. an older
# design bundle from before this field was added).
write_phase1_design <- function(output_file, design, prior_data, scenarios, tree_plot_file = NULL,
                                tree_metadata = NULL, report_title = "MEWOC Phase 1 Design Report") {
  if (!requireNamespace("officer", quietly = TRUE) || !requireNamespace("flextable", quietly = TRUE)) {
    stop("The officer and flextable packages are required to create the Phase 1 design document.", call. = FALSE)
  }
  if (is.null(prior_data)) stop("Run Prior Calculations before downloading the Phase 1 design document.", call. = FALSE)
  if (!length(scenarios)) stop("Run at least one Trial Simulation scenario before downloading the Phase 1 design document.", call. = FALSE)
  
  dose_type <- phase1_value(design$dose_type, fallback = "Discrete")
  
  doc <- officer::read_docx()
  report_title <- trimws(as.character(report_title[1]))
  if (length(report_title) != 1L || !nzchar(report_title)) report_title <- "MEWOC Phase 1 Design Report"
  doc <- officer::body_add_par(doc, report_title, style = "heading 1")
  doc <- officer::body_add_par(doc, "Trial Design", style = "heading 1")
  
  if(identical(dose_type, "Continuous"))
  {
    trial_paragraphs <- continuous_phase1_trial_design_paragraphs(design)
  } else{
    trial_paragraphs <- phase1_trial_design_paragraphs(design)
  }
  
  for (p in trial_paragraphs) doc <- officer::body_add_par(doc, p, style = "Normal")
  doc <- officer::body_add_par(doc, "Operating Characteristics", style = "heading 1")
  
  if(identical(dose_type, "Continuous"))
  {
    operating_paragraphs <- continuous_phase1_operating_paragraphs(x = design, prior_data = prior_data, scenarios = scenarios)
  } else{
    operating_paragraphs <- phase1_operating_paragraphs(x = design, prior_data = prior_data, scenarios = scenarios)
  }
  
  for (p in operating_paragraphs) doc <- officer::body_add_par(doc, p, style = "Normal")
  
  if(identical(dose_type, "Continuous"))
  {
    table_data <- build_continuous_phase1_operating_table(scenarios = scenarios)
    efficiency_ft <- flextable::flextable(table_data$efficiency)
    efficiency_ft <- flextable::add_header_lines(efficiency_ft, values = "Table X. Continuous design operating characteristics - Efficiency")
    efficiency_ft <- flextable::theme_box(efficiency_ft)
    efficiency_ft <- flextable::align(efficiency_ft, align = "center", part = "all")
    efficiency_ft <- flextable::valign(efficiency_ft, valign = "center", part = "all")
    efficiency_ft <- flextable::fontsize(efficiency_ft, size = 9, part = "all")
    efficiency_ft <- flextable::padding(efficiency_ft, padding = 2, part = "all")
    efficiency_ft <- flextable::autofit(efficiency_ft)
    efficiency_ft <- flextable::fit_to_width(efficiency_ft, max_width = 6.25)
    efficiency_ft <- flextable::set_table_properties(efficiency_ft,layout = "autofit",
                                                     opts_word = list(
                                                       split = FALSE,
                                                       keep_with_next = TRUE,
                                                       repeat_headers = TRUE
                                                     )
    )
    safety_ft <- flextable::flextable(table_data$safety)
    safety_ft <- flextable::add_header_lines(safety_ft, values = "Safety")
    safety_ft <- flextable::theme_box(safety_ft)
    safety_ft <- flextable::align(safety_ft, align = "center", part = "all")
    safety_ft <- flextable::valign(safety_ft, valign = "center", part = "all")
    safety_ft <- flextable::fontsize(safety_ft, size = 9, part = "all")
    safety_ft <- flextable::padding(safety_ft, padding = 2, part = "all")
    safety_ft <- flextable::autofit(safety_ft)
    safety_ft <- flextable::fit_to_width(safety_ft,max_width = 6.25)
    safety_ft <- flextable::set_table_properties(safety_ft, layout = "autofit",
                                                 opts_word = list(
                                                   split = FALSE,
                                                   keep_with_next = TRUE,
                                                   repeat_headers = TRUE
                                                 )
    )
    doc <- officer::body_add_break(doc)
    doc <- flextable::body_add_flextable(doc, efficiency_ft)
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- flextable::body_add_flextable(doc, safety_ft)
  } else {
    table_data <- build_phase1_operating_table(scenarios, design$theta)
    cells <- attr(table_data, "true_mtd_cells")
    scenario_rows <- attr(table_data, "scenario_rows")
    dose_header_merge_rows <- attr(table_data, "dose_header_merge_rows")
    summary_merge_rows <- attr(table_data, "summary_merge_rows")
    vertical_merge_ranges <- attr(table_data, "vertical_merge_ranges")
    dose_names <- attr(table_data, "dose_names")
    
    ft <- flextable::flextable(table_data)
    ft <- flextable::delete_part(ft, part = "header")
    ft <- flextable::add_header_lines(ft, values = "Table X. Design operating characteristics for safety. True MTD shown in bold.")
    ft <- flextable::theme_box(ft)
    
    for (row_id in dose_header_merge_rows) ft <- flextable::merge_at(ft, i = row_id, j = dose_names, part = "body")
    for (row_id in summary_merge_rows) ft <- flextable::merge_at(ft, i = row_id, j = dose_names, part = "body")
    for (rows in vertical_merge_ranges) {
      ft <- flextable::merge_at(ft, i = rows, j = "P(Stop)", part = "body")
      ft <- flextable::merge_at(ft, i = rows, j = "Average sample size (95% CI)", part = "body")
    }
    ft <- flextable::valign(ft, valign = "center", part = "body")
    ft <- flextable::align(ft, j = dose_names, align = "center", part = "all")
    ft <- flextable::align(ft, j = c("P(Stop)", "Average sample size (95% CI)"), align = "center", part = "all")
    ft <- flextable::bold(ft, i = scenario_rows, bold = TRUE, part = "body")
    ft <- flextable::fontsize(ft, size = 10, part = "all")
    ft <- flextable::padding(ft, padding = 2, part = "all")
    ft <- flextable::autofit(ft)
    ft <- flextable::fit_to_width(ft, max_width = 6.25)
    ft <- flextable::set_table_properties(
      ft, layout = "autofit",
      opts_word = list(split = FALSE, keep_with_next = TRUE, repeat_headers = TRUE)
    )
    for (cell in cells) {
      ft <- flextable::bold(ft, i = cell$probability_row, j = cell$col, bold = TRUE, part = "body")
      ft <- flextable::bold(ft, i = cell$recommendation_row, j = cell$col, bold = TRUE, part = "body")
    }
    
    doc <- officer::body_add_break(doc)
    doc <- flextable::body_add_flextable(doc, ft)
  }

  # Dose-escalation decision tree (discrete levels or continuous doses).
  if (!is.null(tree_plot_file) && file.exists(tree_plot_file)) {
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_img(doc, src = tree_plot_file, width = 6.25, height = 4.4)
    doc <- officer::body_add_par(doc, "Figure 1. Dose-escalation decision tree generated from the current design inputs.", style = "Normal")
  }
  
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "References", style = "heading 1")
  refs <- phase1_references()
  for (i in seq_along(refs)) doc <- officer::body_add_par(doc, paste0(i, ". ", refs[i]), style = "Normal")
  docx_target <- output_file
  if (!grepl("\\.docx$", docx_target, ignore.case = TRUE)) {
    docx_target <- tempfile(fileext = ".docx")
  }
  print(doc, target = docx_target)
  if (!identical(docx_target, output_file)) {
    file.copy(docx_target, output_file, overwrite = TRUE)
  }
  invisible(output_file)
}

write_phase1_design_html <- function(output_file, design, prior_data, scenarios, tree_plot_file = NULL,
                                     tree_metadata = NULL, report_title = "MEWOC Phase 1 Design Report") {
  if (is.null(prior_data)) stop("Run Prior Calculations before downloading the Phase 1 design document.", call. = FALSE)
  if (!length(scenarios)) stop("Run at least one Trial Simulation scenario before downloading the Phase 1 design document.", call. = FALSE)
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("The htmltools package is required to create the Phase 1 design HTML document.", call. = FALSE)
  }
  
  dose_type <- phase1_value(design$dose_type, fallback = "Discrete")
  
  if(!(dose_type %in% c("Discrete", "Continuous")))
  {
    stop("design$dose_type must be either 'Discrete' or 'Continuous'.", call. = FALSE)
  }
  
  report_title <- trimws(as.character(report_title[1]))
  
  if (length(report_title) != 1L || !nzchar(report_title)) report_title <- "MEWOC Phase 1 Design Report"
  
  html_escape <- function(x) htmltools::htmlEscape(as.character(x))
  df_to_html <- function(df) {
    if (is.null(df) || !nrow(df)) return("<p class='muted'>No table data available.</p>")
    header <- paste0("<tr>", paste0("<th>", html_escape(names(df)), "</th>", collapse = ""), "</tr>")
    rows <- apply(df, 1, function(row) {
      paste0("<tr>", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>")
    })
    paste0("<table>", header, paste(rows, collapse = ""), "</table>")
  }
  
  tree_img <- ""
  
  if(identical(dose_type, "Continuous"))
  {
    trial_design_paragraphs <- continuous_phase1_trial_design_paragraphs(design)
    operating_paragraphs <-continuous_phase1_operating_paragraphs(x = design, prior_data = prior_data, scenarios = scenarios)
    table_data <- build_continuous_phase1_operating_table(scenarios = scenarios)
    table_html <-continuous_phase1_operating_table_html( table_continuous_data = table_data)
  } else {
    trial_design_paragraphs <- phase1_trial_design_paragraphs(design)
    operating_paragraphs <- phase1_operating_paragraphs( x = design, prior_data = prior_data, scenarios = scenarios)
    table_data <- build_phase1_operating_table(scenarios = scenarios, theta = design$theta)
    table_html <- df_to_html(table_data)
  }

  # Dose-escalation decision tree (discrete levels or continuous doses).
  if (!is.null(tree_plot_file) && file.exists(tree_plot_file) && requireNamespace("base64enc", quietly = TRUE)) {
    tree_uri <- base64enc::dataURI(file = tree_plot_file, mime = "image/png")
    tree_img <- paste0(
      "<h2>Dose Escalation Tree</h2>",
      "<img src='", tree_uri, "' alt='Dose escalation tree' class='tree-img'>",
      "<p class='caption'>Figure 1. Dose-escalation decision tree generated from the current design inputs.</p>"
    )
  } else if (!is.null(tree_metadata)) {
    tree_img <- "<h2>Dose Escalation Tree</h2><p class='muted'>Build the dose escalation tree before downloading to embed the figure.</p>"
  }
  
  references <- phase1_references()
  
  
  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>", html_escape(report_title), "</title>",
    "<style>",
    "body{font-family:Georgia,'Times New Roman',serif;line-height:1.55;color:#1f2a2e;background:#f7f4ec;margin:0;padding:32px;}",
    ".wrap{max-width:980px;margin:0 auto;background:#fffdf8;border:1px solid #d7d0c4;padding:32px;}",
    "h1{font-size:2rem;margin-top:0;}h2{margin-top:28px;color:#1f6f78;}p{margin:10px 0;}",
    "table{width:100%;border-collapse:collapse;margin:14px 0 22px;}th,td{border:1px solid #d7d0c4;padding:8px;vertical-align:top;}th{background:#efe8db;}",
    ".tree-img{width:100%;border:1px solid #d7d0c4;margin-top:12px;}.caption,.muted{color:#5a6a70;font-size:.95rem;}",
    ".table-scroll{width:100%;overflow-x:auto;}",
    ".continuous-operating-table table{min-width:1100px;}",
    ".continuous-operating-table .section-row th{",
    "background:#d9e2f3;font-size:1.05rem;text-align:left;}",
    ".continuous-operating-table .section-spacer td{",
    "height:16px;background:#fffdf8;",
    "border-left:none;border-right:none;}",
    ".table-title{margin-top:18px;margin-bottom:8px;}",
    "</style></head><body><div class='wrap'>",
    "<h1>", html_escape(report_title), "</h1>",
    "<h2>Trial Design</h2>",
    paste0("<p>", html_escape(c(trial_design_paragraphs)), "</p>", collapse = ""),
    "<h2>Operating Characteristics</h2>",
    paste0("<p>", html_escape(operating_paragraphs), "</p>", collapse = ""),
    table_html,
    tree_img,
    "<h2>References</h2>",
    paste0("<p>", seq_along(references), ". ", html_escape(references), "</p>", collapse = ""),
    "</div></body></html>"
  )
  writeLines(html, con = output_file, useBytes = TRUE)
  invisible(output_file)
}
