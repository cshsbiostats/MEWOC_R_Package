# MEWOC

MEWOC implements the **Modified Escalation with Overdose Control** design for
single-agent phase I dose-finding trials. Clinicians give prior probabilities that
the maximum tolerated dose (MTD) lies below the minimum or above the maximum trial
dose; MEWOC calibrates the prior, simulates the operating characteristics of the
design for discrete dose levels or a continuous dose range, and supports the
conduct of the trial cohort by cohort. An interactive Shiny application covers the
whole workflow.

A web version of the app is available at
<https://cshsbiostats.github.io/MEWOC_Shiny_App/> (no installation needed).

## Installation

MEWOC uses [JAGS](https://mcmc-jags.sourceforge.io/) for posterior computation,
through the `rjags` package. Install JAGS (version 4 or later) first, then MEWOC:

```r
install.packages("MEWOC")
```

## Shiny app

```r
library(MEWOC)
run_mewoc_app()
```

The app needs the suggested packages `future`, `promises` and `readxl`.
Its helpers are loaded privately. Background workers start when the app starts,
and the previous future plan is restored when it exits.

## Example

```r
library(MEWOC)

# Calibrate the prior to P(MTD < minimum dose) = 0.1 and P(MTD > maximum dose) = 0.2
prior <- run_mewoc_grid_search(theta = 0.33, p0 = 0.1, p1 = 0.2, condition = 2, seed = 1)
build_prior_plot(prior, num_doses = 4, seed_value = 1)

# Simulate 100 trials with four dose levels
inputs <- list(
  M = 100, NN = 30, cohort = 3, dose_type = "Discrete",
  num_doses = 4, startdose = 1, doseprobs = c(0.05, 0.15, 0.33, 0.50),
  actual_dose_levels = c(10, 20, 30, 40), dose_unit = "mg",
  theta = 0.33, deltastop = 0.85, lower_f_bound = 0.25, increment = 0.05,
  alpha_increment_cutoff = 0.33, burn = 1000, mm = 1500, seed = 988,
  trial_mode = "Standard EWOC", max_accel_dose_level = NA,
  stopping_rule_selected = FALSE, num_patients_restrict = NA)
sims <- run_trial_simulations_background(inputs, prior, ncores = 2)
summarize_trial_simulation_results(sims, inputs)$combined_table
```

See `vignette("single-agent-workflows", package = "MEWOC")` for continuous doses
and Trial Conduct.

## Design rules

The simulations, the dose escalation tree, Trial Conduct and the generated design
documents use the same rules:

- **Dose recommendation:** the model-based dose is the posterior MTD alpha-quantile.
  Discrete designs round it to the nearest dose level; the rounded dose can have
  a posterior overdose probability greater than alpha. Alpha starts
  at `lower_f_bound` and rises by `increment`, capped at exactly 0.5, after each cohort while
  the observed DLT rate is below the target. No untried discrete level is skipped;
  continuous doses rise by at most the maximum dose increment.
- **Safety stop:** stop, with no MTD, when the posterior probability that the DLT
  probability at the minimum dose exceeds the target is greater than `deltastop`.
- **Efficiency stop (optional):** stop when `num_patients_restrict` evaluable
  patients have already been treated at the dose recommended for the next cohort.
- **Accelerated EWOC (discrete doses, optional):** one patient per dose level,
  one level at a time, until the first DLT or the maximum dose level for
  acceleration.

The API supports one MCMC chain (`chains = 1`). Calibration, prior plots and
posterior fitting use the same beta prior for the maximum-dose toxicity
probability, truncated to `[0.001, 0.999]`. Recalibrate existing prior settings
before using this candidate; calibration and plot results can differ from the
earlier untruncated implementation even with the same seed.

Simulation counts and MCMC controls must be finite integers in their documented
ranges. Standardized dose grids must be strictly increasing within `[0,1]`;
invalid settings are rejected before fitting. A final cohort may be smaller
than the requested cohort size. Explicit `ncores` requests respect
`options(mewoc.max_trial_workers = ...)` and the two-core check limit. During
checks the app uses a single simulation worker inside its background coordinator.

## Citation

Kabir S, Fan X, Tighiouart M (2026). *MEWOC: Modified Escalation with Overdose
Control for Single-Agent Phase I Trials*. R package version 0.1.0.
<https://cshsbiostats.github.io/MEWOC_Shiny_App/>

Use `citation("MEWOC")` for the package citation and BibTeX entry.

Related presentation:

Kabir S, Tighiouart M (2026). Modified EWOC (MEWOC): A Shiny Application for
Single-Agent Phase I Dose-Finding Trials. Joint Statistical Meeting 2026, Boston,
MA. <https://doi.org/10.5281/zenodo.23254550>

## License

GPL-3
