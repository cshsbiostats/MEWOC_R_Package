# MEWOC 0.1.1.9000 (development)

* Provide ten scripted workflow functions and the Shiny launcher. Supporting
  engines are internal and shared with Shiny and its background workers.
* Store the dose design and seed in reusable prior and simulation objects.
  Preserve equally spaced discrete model positions and full numeric precision.
* Add decision trees, Word protocol drafts, empty conduct workbooks, next-dose
  recommendations, and Word/HTML final MTD reports.
* Explain tree depth limits with warnings and max_steps/plotted_steps. The test
  examples show four levels with twelve patients in cohorts of three.
* Apply Shiny's final-report rule in both interfaces: recheck safety only if
  no trial stop is recorded. Efficiency stops proceed to final estimation;
  recorded safety stops yield no MTD.
* Install officer and flextable as required dependencies. Simplify messages,
  help pages and examples; remove unused report scaffolding and stale version text.
* Display two decimal places and use mathematical notation for tree symbols.

# MEWOC 0.1.0

* Finalize package authors as Shaowli Kabir, Xuyan Fan and Mourad Tighiouart,
  with affiliations, Shaowli Kabir as maintainer and the Biostatistics Shared
  Resource Cedars-Sinai Health Sciences University as copyright holder. Add a
  package-specific citation and distinguish it from the related presentation.

* Return absolute target-matching errors and validation Monte Carlo standard
  errors from prior calibration. The optimizer and acceptance behavior are
  unchanged, and no new diagnostics are displayed in the Shiny app.

* Use burn = 1000 in the short batch-engine success-path tests to match the
  intended production setting. Production adaptation, burn-in and sampling
  behavior are unchanged; adaptation warnings are not suppressed.

* Reject invalid simulation counts, dose grids, modes, prior shapes, MCMC settings
  and continuous scenarios before fitting. Guard the cohort loop against
  nonprogress and preserve partial-final-cohort support.
* Construct the app in a private environment without attaching helper packages.
  Defer workers/resource registration to startup and restore the caller's future
  plan and image resource path on normal stop or startup failure.
* Apply worker options and check-time limits to explicit worker requests too.
  Pass the selected worker count into the app's background job; avoid nested
  simulation clusters during checks.

* Restrict the single-trial API and shared posterior fitter to one MCMC chain;
  unsupported chain counts now produce a clear error before sampling.
* Retain nearest-level discrete rounding and document that the rounded dose's
  posterior overdose probability can exceed alpha.
* Retain the JAGS rho1 truncation at [0.001, 0.999] and use that same truncated
  beta prior in calibration and plots. Existing prior settings should be
  recalibrated; prior results can change even with an unchanged random seed.
* Cap the feasibility-bound schedule at exactly 0.5 when an increment would
  cross the cap, matching Trial Conduct's existing update rule.

* Correct continuous-dose summaries when the true curve is specified by the true
  MTD and minimum-dose toxicity probability. Continuous batches retain normalized
  scenario settings and summaries reject conflicting scenario inputs.
* Preserve accelerated EWOC mode and its dose cap in Trial Conduct workbooks
  when using the documented mode names. Workbook Yes/No aliases remain supported;
  unknown modes now produce an error.

* First CRAN submission.
* Prior calibration with the BOBYQA optimizer (`run_mewoc_grid_search()`).
* Trial simulation for discrete dose levels and continuous dose ranges, with
  toxicity-dependent feasibility bounds, the safety stopping rule
  (posterior probability greater than Delta), an optional efficiency stopping
  rule and an optional accelerated lead-in (`run_single_trial_simulation()`,
  `run_trial_simulations_background()`, `summarize_trial_simulation_results()`).
* Trial Conduct workbook and cohort-by-cohort evaluation of trial data
  (`build_conduct_design_table()`, `conduct_trial_rule_rows()`,
  `build_conduct_patient_template()`, `write_conduct_workbook()`,
  `conduct_evaluate_trial()`).
* Interactive Shiny application (`run_mewoc_app()`).
