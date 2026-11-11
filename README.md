# MEWOC

MEWOC implements modified escalation with overdose control for single-agent
phase I trials. A guided Shiny application and ten scripted workflow functions
support prior calibration, discrete- or continuous-dose simulation, decision
trees, protocol drafts, trial conduct, and final reporting.

This repository contains development version **0.1.1.9000**. Its simplified
scripted API differs from version 0.1.0; see [NEWS.md](NEWS.md) and the examples
below. This development version is separate from the earlier CRAN submission.

Project website: <https://cshsbiostats.github.io/MEWOC_Shiny_App/>.

## Shared calculation helpers

Shiny, scripted functions and background workers use the same helpers in `R/`.
After editing a helper, reload or reinstall MEWOC and restart Shiny. Both
interfaces then use the updated logic. Display rounding preserves stored values.

## Installation

Install [JAGS](https://mcmc-jags.sourceforge.io/) version 4 or later first.
MEWOC requires R 4.1.0 or later. Download or clone this repository and run the
following from the package root (the directory containing `DESCRIPTION`):

```r
install.packages("remotes")  # once, if needed
remotes::install_local(".", dependencies = TRUE, build_vignettes = TRUE)
library(MEWOC)
```

`officer` and `flextable` are required dependencies and install with MEWOC in a
normal repository installation. Shiny also uses `future`, `promises` and `readxl`.
`pkgload` supports background workers launched from source development.
Building the HTML vignettes also requires Pandoc. Use `build_vignettes = FALSE`
if you only need to install and run the package.

## Shiny application

```r
library(MEWOC)
run_mewoc_app()
```

The browser interface guides users through Trial Setup, Trial Design, Trial
Documents and Trial Conduct. It provides dose-specific inputs, contextual help,
prior plots, simulation summaries, decision trees, and document downloads.
Routine analyses can be performed through the interface without writing
analysis code. The scripted functions below support saved objects, automation
and reproducible analyses using the same calculation helpers.

The app uses the suggested packages `future`, `promises` and `readxl`. The
installation command above requests suggested dependencies too. If they are
missing from an existing installation, install them before launching Shiny:

```r
install.packages(c("future", "promises", "readxl"))
```

## Public functions

| Function | Purpose |
| --- | --- |
| `run_mewoc_grid_discrete()` | Calibrate a discrete prior and store its design/seed |
| `run_mewoc_grid_cont()` | Calibrate a continuous prior and store its design/seed |
| `build_prior_plot()` | Plot the stored prior using clinical dose labels |
| `run_trial_sim_discrete()` | Simulate and summarize discrete trials |
| `run_trial_sim_continuous()` | Simulate and summarize continuous trials |
| `Decision_tree()` | Enumerate decisions from the starting design |
| `Build_protocol_draft()` | Write an app-format Word protocol draft |
| `Trial_conduct_template()` | Write prefilled settings with empty patient rows |
| `Run_trial_conduct()` | Evaluate entered patients and recommend the next dose |
| `Final_mtd_report()` | Write Word and/or HTML final reports |
| `run_mewoc_app()` | Launch Shiny |

See `help(package = "MEWOC")` for detailed help and examples, and `?mewoc_results`
for print, summary and plot methods. Both vignettes cover the complete workflow.

## Scripted example

```r
library(MEWOC)

prior <- run_mewoc_grid_discrete(.30, .10, .10, num_doses = 3,
  dose_values = c(10, 25, 60), dose_unit = "mg", seed = 988)
build_prior_plot(prior)
sim <- run_trial_sim_discrete(prior, 1000, 30, 3, c(.05, .20, .50), ncores = 1)
sim
tree <- Decision_tree(sim, steps = 4)
Build_protocol_draft(prior, sim, tree, "outputs/protocol.docx")
Trial_conduct_template(sim, "outputs/trial.xlsx")
```

Fill and save the workbook's Data Input sheet in Excel. After each cohort:

```r
conduct <- Run_trial_conduct("outputs/trial.xlsx")
print(conduct)
```

At the end of the trial, resolve pending outcomes and use the last conduct result:

```r
final <- Final_mtd_report(conduct, c("outputs/final.docx", "outputs/final.html"))
```

For continuous doses:

```r
prior <- run_mewoc_grid_cont(.30, .10, .10, 10, 100, "mg", seed = 988)
build_prior_plot(prior)
sim <- run_trial_sim_continuous(prior, 1000, 30, 3,
  true_mtd = 60, true_p_min = .05, ncores = 1)
# The tree, protocol, workbook, conduct and report functions are the same.
```

The prior constructor stores the workflow seed; later functions inherit it.
Use `saveRDS()`/`readRDS()` to retain complete objects. Elicited p0/p1 concern
the MTD lying outside the dose range, not scenario toxicity probabilities.
Discrete clinical doses retain equally spaced model positions. The posterior
MTD alpha-quantile is mapped to a permitted dose under the no-skipping rule;
the mapped level can have overdose probability above alpha. Continuous designs
start at the minimum and limit escalation by a percentage of the dose range.

Matching numerical results requires matching inputs, seeds, MCMC settings and
R/JAGS versions. API workbooks store their own configuration; use Shiny workbooks
through Shiny. Existing output files are protected unless `overwrite = TRUE`.
Function-help examples use small synthetic trials. Assess a study design with
appropriate MCMC settings and many more simulated trials.
Both final-report interfaces preserve Shiny's rule: a recorded safety stop means
no MTD; an efficiency stop proceeds to final estimation. The final safety recheck
is applied only if there is no recorded trial stop.

## Development and checks

The GitHub Actions workflow checks R-release and R-devel on Ubuntu, installing
JAGS before the R dependencies. It omits the PDF manual; a full local check
remains part of release preparation. Workflow results become available after
the repository is pushed and the jobs run.

From a local checkout with development dependencies installed:

```r
devtools::load_all(".", export_all = FALSE)
devtools::test(".")
devtools::check(".")
```

The package includes detailed help and two workflow vignettes. Build archives,
check directories and local session files are excluded from version control.
The workflow follows the [r-lib/actions examples](https://github.com/r-lib/actions/tree/v2/examples).

Use `citation("MEWOC")` for the current citation and BibTeX entry. License: GPL-3.
