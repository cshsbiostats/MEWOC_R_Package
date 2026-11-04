protocol_draft_replacements <- function(total_sample_size, theta, cohort_size,
                                        lower_feasibility_bound, feasibility_bound_increment) {
  values <- c(
    total_sample_size,
    theta,
    cohort_size,
    lower_feasibility_bound,
    feasibility_bound_increment
  )
  if (any(lengths(values) != 1L) || any(vapply(values, is.null, logical(1)))) {
    stop("All protocol draft replacement values must be supplied.", call. = FALSE)
  }

  stats::setNames(
    as.character(values),
    c(
      "TOTAL SAMPLE SIZE",
      "TARGET PROBABILITY OF DLT",
      "COHORT SIZE",
      "LOWER FEASIBILITY BOUND",
      "FEASIBILITY BOUND INCREMENT"
    )
  )
}

protocol_draft_text <- function() {
  paste0(
    "Escalation with overdose control (EWOC) [1,3] design will be used for dose escalation. ",
    "A maximum of n = TOTAL SAMPLE SIZE evaluable participants will be enrolled. ",
    "The MTD is defined as the dose level at which the probability of dose-limiting toxicity (DLT) is ",
    "\u03b8 = TARGET PROBABILITY OF DLT. The DLT period is DEFINE DLT PERIOD HERE. ",
    "Consecutive cohorts of COHORT SIZE participants will be enrolled the first time a dose level is tried, ",
    "and the dose allocated to the next cohort will be determined so that, based on all available data, ",
    "the probability that it exceeds the MTD equals a feasibility bound \u03b1. This will begin with \u03b1 = ",
    "LOWER FEASIBILITY BOUND and increase \u03b1 in small increments of FEASIBILITY BOUND INCREMENT each time ",
    "a new dose recommendation is calculated until \u03b1 = 0.5. This value is a compromise between the ",
    "therapeutic aspect of the agent and its toxic side effects. Chu et al. [4] showed that the EWOC design ",
    "with varying feasibility bound provides better protection in limiting higher doses for participants than ",
    "the 4 versions of CRM designs with a similar convergence rate. No \u03b1 increment will be used if the ",
    "cumulative observed DLT rate (total DLTs divided by the total number of evaluable participants) is greater than or equal to \u03b8. ",
    "No untried dose level will be skipped. The study will be stopped for safety if the posterior probability that the ",
    "DLT probability at the lowest dose exceeds \u03b8 is greater than DEFINE STOPPING THRESHOLD DELTA HERE; no dose will then be declared the MTD. ",
    "At any time during the study, a participant may be enrolled at a given dose level if no more than ",
    "COHORT SIZE participants have unresolved DLT status at that dose level."
  )
}

render_protocol_draft_text <- function(replacements) {
  text <- protocol_draft_text()
  for (placeholder in names(replacements)) {
    text <- gsub(placeholder, unname(replacements[[placeholder]]), text, fixed = TRUE)
  }
  text
}

write_protocol_draft <- function(output_file, replacements) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The officer package is required to create the protocol draft.", call. = FALSE)
  }
  required_placeholders <- c(
    "TOTAL SAMPLE SIZE",
    "TARGET PROBABILITY OF DLT",
    "COHORT SIZE",
    "LOWER FEASIBILITY BOUND",
    "FEASIBILITY BOUND INCREMENT"
  )
  if (!all(required_placeholders %in% names(replacements))) {
    stop("The protocol replacement list is incomplete.", call. = FALSE)
  }

  paragraph_text <- render_protocol_draft_text(replacements[required_placeholders])
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, value = paragraph_text, style = "Normal")
  print(doc, target = output_file)
  invisible(output_file)
}
