#' MEWOC: Modified Escalation with Overdose Control for Single-Agent Phase I Trials
#'
#' Bayesian dose finding for single-agent phase I trials with escalation with
#' overdose control (EWOC), using the reparameterized model in which clinicians
#' give prior probabilities that the maximum tolerated dose (MTD) lies below the
#' minimum or above the maximum trial dose. Doses may be discrete levels or a
#' continuous range.
#'
#' Main functions:
#' * [run_mewoc_grid_search()] calibrates the prior hyperparameters (BOBYQA).
#' * [build_prior_plot()] displays the prior probability of a dose-limiting
#'   toxicity (DLT) at each dose.
#' * [run_single_trial_simulation()] and [run_trial_simulations_background()]
#'   simulate trials; [summarize_trial_simulation_results()] gives the operating
#'   characteristics.
#' * [build_conduct_design_table()], [conduct_trial_rule_rows()],
#'   [build_conduct_patient_template()] and [write_conduct_workbook()] create the
#'   Trial Conduct workbook; [conduct_evaluate_trial()] evaluates trial data
#'   cohort by cohort and recommends the dose for the next cohort.
#' * [run_mewoc_app()] starts the interactive 'shiny' application.
#'
#' Posterior computation uses 'JAGS' through the 'rjags' package; JAGS must be
#' installed (<https://mcmc-jags.sourceforge.io/>).
#'
#' @references
#' Kabir S, Fan X, Tighiouart M (2026). MEWOC: Modified Escalation with Overdose
#' Control for Single-Agent Phase I Trials. R package version 0.1.0.
#' <https://cshsbiostats.github.io/MEWOC_Shiny_App/>
#'
#' Babb J, Rogatko A, Zacks S (1998). Cancer phase I clinical trials: efficient
#' dose escalation with overdose control. Statistics in Medicine, 17(10),
#' 1103-1120.
#'
#' Tighiouart M, Rogatko A, Babb JS (2005). Flexible Bayesian methods for cancer
#' phase I clinical trials. Dose escalation with overdose control. Statistics in
#' Medicine, 24(14), 2183-2196.
#'
#' Tighiouart M, Rogatko A (2010). Dose finding with escalation with overdose
#' control (EWOC) in cancer clinical trials. Statistical Science, 25(2), 217-226.
#' \doi{10.1214/10-STS333}
#'
#' Wheeler GM, Sweeting MJ, Mander AP (2017). Toxicity-dependent feasibility
#' bounds for the escalation with overdose control approach in phase I cancer
#' trials. Statistics in Medicine, 36(16), 2499-2513. \doi{10.1002/sim.7280}
#'
#' Kabir S, Tighiouart M (2026). Modified EWOC (MEWOC): A Shiny Application for
#' Single-Agent Phase I Dose-Finding Trials. Joint Statistical Meeting 2026,
#' Boston, MA. \doi{10.5281/zenodo.23254550}
#'
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @importFrom stats density median optimize plogis qlogis qnorm quantile rbeta rbinom rgamma runif sd setNames update
#' @importFrom utils capture.output tail write.csv
#' @importFrom graphics abline contour hist legend lines par plot points text
#' @importFrom grDevices colorRampPalette dev.off png
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom rjags coda.samples jags.model
#' @importFrom tidyr pivot_longer
"_PACKAGE"
