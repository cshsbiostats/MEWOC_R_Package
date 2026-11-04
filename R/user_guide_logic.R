guide_input_explanations <- list(
  treatment_and_dose_type = "Dose Type selects discrete dose levels or a continuous dose range. These choices determine which inputs, prior calculations, simulation logic, and Trial Conduct workflow the app displays.",
  trial_setup_core_inputs = "Enter the planned total number of patients, cohort size, target toxicity probability, and dose unit. For a categorical design, also enter the number of dose levels and starting dose level. For a continuous design, enter the minimum and maximum dose values. Use protocol-approved values and verify all dose units.",
  dose_and_level_inputs = "Dose Unit identifies the clinical unit used for reporting. For a discrete design, enter the number of dose levels, starting dose level, and actual dose values in increasing order. For a continuous design, enter the minimum and maximum dose values.",
  discrete_actual_dose_values = "For a discrete design, enter the actual clinical dose amount assigned to each dose level. Enter values in increasing order, use the displayed dose unit, and ensure the values match the protocol dose table. These values support dose labeling, reporting, and document generation.",
  prior_dose_type = "Dose Type determines whether the design uses a fixed set of dose levels or a continuous dose range. This choice affects prior inputs, simulation truth settings, document generation, and Trial Conduct fields.",
  NN = "Total Number of Patients is the planned maximum sample size for the phase I design. It is used in simulations and trial documents to define how many patients may be accrued under the design.",
  cohort = "Cohort Size is the number of patients treated before the next model update or dose decision. It affects the dose escalation tree and trial workflow timing.",
  theta = "Target Toxicity is the target probability of dose-limiting toxicity (DLT). The MTD is the dose associated with this target toxicity level in the model.",
  dose_unit = "Dose Unit is the clinical unit used to display actual doses, such as mg or mg/m2. It is for labeling and reporting.",
  actual_dose = "Actual Dose Value is the real clinical dose amount assigned to a discrete dose level. These values are used for reporting and document generation.",
  num_doses = "Number of Dose Levels is the count of available discrete dose levels in the trial. The app uses this to build dose inputs, simulations, and documents.",
  startdose = "Starting Dose Level is the first dose level used when the trial begins. It should match the protocol-specified starting level.",
  min_dose = "Minimum Dose Value is the lower bound of the continuous dose range. It anchors the available dose interval for continuous-dose designs.",
  max_dose = "Maximum Dose Value is the upper bound of the continuous dose range. It anchors the available dose interval for continuous-dose designs.",
  p0 = "P(MTD < Min Dose) is the clinician prior probability that the MTD is below the minimum dose. Larger values express more prior concern that all available doses may be too toxic.",
  p1 = "P(MTD > Max Dose) is the clinician prior probability that the MTD is above the maximum dose. Larger values express more prior belief that the available range may be conservative.",
  condition = "The total sum of the shape parameters of each beta prior (a0 + b0 for rho1, and a1 + b1 for rho0/rho1). It controls the effective sample size of the priors: smaller values represent weaker prior information. Default 2: each prior carries about as much information as 2 patients, so the trial data quickly outweigh it (e.g. about 20% weight after 8 patients and 6% after 30).",
  M = "Total number of simulated trials. Larger values give more stable results but take longer to run.",
  trial_simulation_scenarios = "Total number of simulated trials. Larger values give more stable results but take longer to run.",
  max_dose_increment = "Maximum Dose Increment limits how large a continuous-dose escalation step can be between decisions: the next dose is at most the current dose plus this percentage of the dose range. (Categorical doses: no untried dose level is skipped.)",
  deltastop = "Threshold to stop the trial when there is substantial evidence that the minimum dose is too toxic: the trial stops when Pr{ Pr(DLT | X = Xmin) > theta | data } is greater than Delta. The rule is checked at every dose decision and once more on the final data; after a safety stop no dose is recommended as the MTD.",
  trial_mode = "Accelerated EWOC (categorical doses only) starts with one patient at a time, one dose level up per patient, with no dose skipped and no model fit. The lead-in ends at the first DLT or after the patient treated at the Maximum Dose Level for Acceleration. EWOC then uses all data to recommend the next cohort's dose, starting from the starting alpha. Lead-in patients count toward the total sample size.",
  max_accel_dose_level = "Maximum Dose Level for Acceleration is the highest dose level given during the accelerated lead-in. After the patient at this level (or the first DLT), the design switches to EWOC.",
  true_mtd = "True MTD Dose is a simulation-only assumption for continuous-dose scenarios. It defines the dose treated as the true target dose when estimating operating characteristics.",
  true_p_min = "True P(DLT) at Minimum Dose is a simulation-only assumption for continuous-dose scenarios. It anchors the assumed toxicity curve at the minimum dose.",
  dose_probability = "True Probability of DLT is a simulation-only assumption for a discrete dose level. It defines the scenario used to test operating characteristics.",
  seed = "Seed Value makes the prior calculation and the trial simulations reproducible by controlling the random-number stream. Default 988.",
  burn = "Burn-in is the number of early MCMC samples discarded before posterior summaries are computed. Default 1000: in the MCMC settings study (October 2026) burn-in 1000, 1500 and 2000 gave the same results, so the shortest was kept.",
  mm = "MCMC Samples is the number of retained posterior samples used for model summaries and dose calculations. Default 1500: it gave the same operating characteristics as 2000 samples in about two thirds of the run time, and steadier single dose decisions than 1000 samples.",
  lower_f_bound = "Lower Feasibility Bound alpha is the EWOC overdose-control quantile used for the first EWOC cohort.",
  increment = "Feasibility Bound Increment is the amount alpha increases after each EWOC cohort, up to 0.5, when the toxicity-dependent rule allows it.",
  feasibility_rule = "MEWOC gives the next cohort the alpha-quantile of the posterior distribution of the MTD (categorical doses: the nearest dose level). The feasibility bound alpha starts at the Lower Feasibility Bound and may increase by the Feasibility Bound Increment, up to 0.5, only if the observed DLT rate (total DLTs / total patients so far) is below the target DLT rate theta. The optional Toxicity-dependent overdose control replaces theta in this rule with a different (lower) toxicity threshold.",
  alpha_increment_rule = "Toxicity-dependent overdose control decides when alpha may increase. By default the bound is theta: alpha increases only while the observed DLT rate is below theta. Select Yes to use a custom toxicity bound instead; it must be greater than 0 and lower than theta.",
  alpha_increment_cutoff = "Custom Toxicity Bound is used only when toxicity-dependent overdose control is set to a custom threshold lower than theta.",
  stopping_rule = "Trial efficiency stopping is an optional rule: the trial stops when the maximum number of evaluable patients has already been treated at the dose EWOC recommends for the next cohort (continuous doses: within plus or minus half the dose-region width of it). The MTD is then estimated from the data. It is separate from the Delta safety stop.",
  num_patients_restrict = "Maximum number of patients at a dose: the trial stops for efficiency when this many evaluable patients have already been treated at the recommended next dose.",
  efficiency_stop_tolerance = "Dose-region width (continuous doses, % of the dose range): doses within plus or minus half this width of the recommended dose count as the same dose for the efficiency stop.",
  tree_steps = "Decision Steps to Expand controls how many future decision steps are shown in the dose escalation tree. Cohort sizes 2 and 3 branch faster than cohort size 1; 3 to 4 steps are recommended.",
  report_title = "Optional title for the trial documents. Readiness shows whether they can be downloaded.",
  phase1_design_download = "Download the Phase 1 design (Word or HTML) as supporting material for the protocol.",
  conduct_workbook_download = "Excel template for recording patient data. Upload the completed workbook under Trial Conduct to get the next dose recommendation.",
  conduct_design_entry_mode = "Upload the completed Trial Conduct workbook (template from Trial Documents) or enter the trial information manually.",
  conduct_workbook_upload = "Upload Trial Conduct Workbook imports the workbook generated from Trial Documents so conduct settings match the simulated design.",
  conduct_p0 = "P(MTD < Min) in Trial Conduct should match the prior belief used in the finalized design.",
  conduct_p1 = "P(MTD > Max) in Trial Conduct should match the prior belief used in the finalized design.",
  conduct_beta = "Beta hyperparameters in Trial Conduct should come from prior calibration. They define the fitted prior distribution used in conduct calculations.",
  conduct_dose_type = "Dose Type in Trial Conduct should match the finalized trial design: discrete levels or continuous dose range.",
  conduct_actual_dose_values = "Actual Dose Values are the real clinical dose amounts for each discrete dose level, entered in increasing order.",
  conduct_theta = "Theta in Trial Conduct is the target DLT probability from the finalized protocol.",
  conduct_deltastop = "Threshold to stop the trial when there is substantial evidence that the minimum dose is too toxic: the trial stops when Pr{ Pr(DLT | X = Xmin) > theta | data } is greater than Delta. The rule is checked at every dose decision and once more on the final data; after a safety stop no dose is recommended as the MTD.",
  conduct_lower_f_bound = "\u03b1 in Trial Conduct is the starting lower feasibility bound used for the first EWOC cohort; it rises by the increment as the design rules allow.",
  conduct_increment = "Increment in Trial Conduct is the protocol-specified increase in \u03b1 when escalation-control rules allow it.",
  conduct_seed = "Seed in Trial Conduct supports reproducibility for posterior model calculations.",
  conduct_burn = "Burn-in in Trial Conduct is the discarded early MCMC sample count for posterior calculations.",
  conduct_mm = "MCMC Samples in Trial Conduct is the retained posterior sample count used in the conduct calculation.",
  conduct_patient_rows = "Current Trial Data contains treated-patient dose and DLT outcomes. Verify these data carefully before evaluating the conduct recommendation."
)

## Help formulas shown in the ? pop-ups (HTML, written with entities so the source stays ASCII).
## gamma is the MTD on the standardized dose scale x (x = 0 at the minimum dose, x = 1 at the maximum dose).
guide_formula_safety_stop <- "Stop for safety (no MTD) when Pr{ Pr(DLT | X = X<sub>min</sub>) &gt; &theta; | D<sub>n</sub> } &gt; &Delta;"
guide_formula_model <- c(
  "P(DLT | x) = F( logit &rho;<sub>0</sub> + x (logit &rho;<sub>1</sub> &minus; logit &rho;<sub>0</sub>) ),&nbsp; F(u) = 1 / (1 + e<sup>&minus;u</sup>)",
  "&rho;<sub>0</sub> = P(DLT at the minimum dose, x = 0),&nbsp; &rho;<sub>1</sub> = P(DLT at the maximum dose, x = 1)",
  "MTD: &gamma; = (logit &theta; &minus; logit &rho;<sub>0</sub>) / (logit &rho;<sub>1</sub> &minus; logit &rho;<sub>0</sub>),&nbsp; so P(DLT | &gamma;) = &theta;",
  "Priors: &rho;<sub>1</sub> ~ Beta(a<sub>0</sub>, b<sub>0</sub>) truncated to [0.001, 0.999],&nbsp; &rho;<sub>0</sub>/&rho;<sub>1</sub> ~ Beta(a<sub>1</sub>, b<sub>1</sub>)",
  "P(&gamma; &lt; 0) = p<sub>0</sub>,&nbsp; P(&gamma; &gt; 1) = p<sub>1</sub>"
)
guide_formula_alpha <- c(
  "Model-based dose: the posterior MTD &alpha;-quantile, subject to dose limits. Categorical doses: nearest dose level; rounding can give posterior overdose probability above &alpha;.",
  "&alpha;<sub>1</sub> = lower feasibility bound;&nbsp; &alpha;<sub>k+1</sub> = min(&alpha;<sub>k</sub> + increment, 0.5) if (total DLTs / total patients) &lt; toxicity bound, otherwise &alpha;<sub>k+1</sub> = &alpha;<sub>k</sub>"
)
guide_formula_efficiency <- "Stop for efficiency when n(x<sub>next</sub>) &ge; N<sub>max</sub>, where n(x<sub>next</sub>) = evaluable patients already treated at the recommended dose (continuous doses: within &plusmn; w/2 of it, w = dose-region width)"
guide_formula_mtd <- "Final MTD = median of the posterior of &gamma;&nbsp; (categorical doses: the nearest dose level)"

guide_input_formulas <- list(
  theta = "MTD: the dose &gamma; with P(DLT | &gamma;) = &theta;",
  p0 = "p<sub>0</sub> = P(&gamma; &lt; 0) = P(MTD &lt; minimum dose)",
  p1 = "p<sub>1</sub> = P(&gamma; &gt; 1) = P(MTD &gt; maximum dose)",
  max_dose_increment = "x<sub>next</sub> &le; x<sub>current</sub> + d / 100&nbsp; (d = maximum dose increment, % of the dose range)",
  lower_f_bound = guide_formula_alpha,
  increment = guide_formula_alpha,
  alpha_increment_rule = "&alpha; increases only if (total DLTs / total patients) &lt; toxicity bound;&nbsp; toxicity bound = &theta; (default) or a custom c with 0 &lt; c &lt; &theta;",
  stopping_rule = guide_formula_efficiency,
  num_patients_restrict = guide_formula_efficiency,
  efficiency_stop_tolerance = guide_formula_efficiency
)

guide_group_formulas <- list(
  trial_simulation_inputs = c(guide_formula_safety_stop, guide_formula_alpha[1], guide_formula_mtd)
)

## Formatted pop-up bodies (HTML written with entities so the source stays ASCII). Where a topic or panel has one,
## it is shown instead of the plain explanation (and before the panel's formula block, if any).
guide_eq <- function(...) paste0("<div style='text-align: center; margin: 8px 0 10px 0; font-family: Cambria Math, Cambria, Georgia, serif; font-size: 16px;'>", ..., "</div>")
guide_frac <- function(num, den) paste0("<span style='display: inline-flex; flex-direction: column; vertical-align: middle; text-align: center;'><span style='border-bottom: 1px solid; padding: 0 4px;'>", num, "</span><span style='padding: 0 4px;'>", den, "</span></span>")
guide_p <- function(...) paste0("<p>", ..., "</p>")
guide_html_delta <- paste0(
  guide_p("Threshold to stop the trial when there is substantial evidence that the minimum dose is too toxic:"),
  guide_eq("Pr{ Pr(DLT | X = X<sub>min</sub>) &gt; &theta; | D<sub>n</sub> } &gt; &Delta;"),
  guide_p("where D<sub>n</sub> is the data from the n patients treated so far. The rule is checked at every dose decision and once more on the final data; after a safety stop no dose is recommended as the MTD.")
)
guide_input_html <- list(
  condition = guide_p("The total sum of the shape parameters of each beta prior (a<sub>0</sub> + b<sub>0</sub> for &rho;<sub>1</sub>, and a<sub>1</sub> + b<sub>1</sub> for &rho;<sub>0</sub>/&rho;<sub>1</sub>). It controls the effective sample size of the priors: smaller values represent weaker prior information. Default 2: each prior carries about as much information as 2 patients, so the trial data quickly outweigh it (e.g. about 20% weight after 8 patients and 6% after 30)."),
  deltastop = guide_html_delta,
  conduct_deltastop = guide_html_delta,
  feasibility_rule = paste0(
    guide_p("MEWOC selects the next dose as"),
    guide_eq("x<sub>next</sub>:&nbsp; Pr(&gamma; &le; x<sub>next</sub> | data) = &alpha;"),
    guide_p("(the &alpha;-quantile of the posterior distribution of the MTD; for categorical doses, the nearest dose level)."),
    guide_p("The feasibility bound starts at the <b>Lower Feasibility Bound (&alpha;)</b> and may increase for the next dose selection by the <b>Feasibility Bound Increment</b>, up to a maximum of 0.5, only if the observed DLT rate is below the target DLT rate &theta;:"),
    guide_eq("P&#770;<sub>DLT</sub> &lt; &theta;"),
    guide_p("where P&#770;<sub>DLT</sub> = total DLTs / total patients treated so far. The optional <b>Toxicity-dependent overdose control</b> lets you replace &theta; in this rule with a different (lower) toxicity threshold.")
  )
)
guide_group_html <- list(
  prior_inputs = paste0(
    guide_p("Based on the monotone logistic dose&ndash;toxicity model"),
    guide_eq("Pr(Y<sub>i</sub> = 1 | x<sub>i</sub>) = F(&beta;<sub>0</sub> + &beta;<sub>1</sub>x<sub>i</sub>),&emsp;&emsp; &beta;<sub>1</sub> &gt; 0"),
    guide_p("where F(u) = {1 + exp(&minus;u)}<sup>&minus;1</sup>. The MTD &gamma; satisfies Pr(Y = 1 | &gamma;) = &theta;. With the reparameterization &beta;<sub>0</sub> = F<sup>&minus;1</sup>(&rho;<sub>0</sub>), &beta;<sub>1</sub> = F<sup>&minus;1</sup>(&rho;<sub>1</sub>) &minus; F<sup>&minus;1</sup>(&rho;<sub>0</sub>),"),
    guide_eq("&gamma; = ", guide_frac("F<sup>&minus;1</sup>(&theta;) &minus; F<sup>&minus;1</sup>(&rho;<sub>0</sub>)", "F<sup>&minus;1</sup>(&rho;<sub>1</sub>) &minus; F<sup>&minus;1</sup>(&rho;<sub>0</sub>)"), "."),
    guide_p("where &rho;<sub>0</sub> = Pr(DLT | X<sub>min</sub>) and &rho;<sub>1</sub> = Pr(DLT | X<sub>max</sub>). Assuming &rho;<sub>1</sub> &sim; Beta(a<sub>0</sub>, b<sub>0</sub>) and, conditional on &rho;<sub>1</sub>, &rho;<sub>0</sub>/&rho;<sub>1</sub> &sim; Beta(a<sub>1</sub>, b<sub>1</sub>), these DLT priors are calibrated so that"),
    guide_eq("Pr(&gamma; &lt; X<sub>min</sub>) = Pr(&rho;<sub>0</sub> &gt; &theta;),&emsp;&emsp; Pr(&gamma; &gt; X<sub>max</sub>) = Pr(&rho;<sub>1</sub> &lt; &theta;)."),
    guide_p("The prior calibration module uses the BOBYQA optimization algorithm to find the beta hyperparameters that match these elicited probabilities.")
  ),
  trial_documents_inputs = guide_p("Model-based dose assignments for each possible DLT outcome. STOP TRIAL: &Delta; safety stop; EFF STOP: efficiency stop (if selected). Accelerated EWOC: the tree starts at the first EWOC decision, after the lead-in reaches the maximum acceleration dose with no DLT."),
  conduct_data_inputs = guide_p("Review the entered data, then tick the check box to proceed."),
  trial_simulation_inputs = guide_p("The Trial Simulation subtab lets users define candidate true toxicity scenarios and evaluate the design's operating characteristics under those assumptions. Discrete-dose scenarios are entered as the true DLT probabilities at each dose level; continuous-dose scenarios are entered as the true MTD and the true DLT probability at the minimum dose.")
)

## Formula lines as a highlighted block (NULL when the topic has no formula).
guide_formula_ui <- function(formulas) {
  if (is.null(formulas) || length(formulas) == 0L) return(NULL)
  tags$div(
    class = "guide-formula",
    style = "background-color: #f6f8fa; border-left: 3px solid #2f7ea8; padding: 8px 12px; margin: 4px 0 12px 0; font-family: 'Cambria Math', Cambria, Georgia, serif; font-size: 15px; line-height: 1.7;",
    lapply(formulas, function(line) tags$div(HTML(line)))
  )
}

guide_input_title <- function(topic_key) {
  key <- if (length(topic_key) == 1L && !is.null(topic_key)) as.character(topic_key) else ""
  names_map <- c(
    treatment_and_dose_type = "Dose Type",
    trial_setup_core_inputs = "Trial Setup Inputs",
    dose_and_level_inputs = "Dose and Dose-Level Inputs",
    discrete_actual_dose_values = "Actual Dose Values",
    prior_dose_type = "Dose Type",
    NN = "Total Number of Patients",
    cohort = "Cohort Size",
    theta = "Target Toxicity",
    dose_unit = "Dose Unit",
    actual_dose = "Actual Dose Value",
    num_doses = "Number of Dose Levels",
    startdose = "Starting Dose Level",
    min_dose = "Minimum Dose Value",
    max_dose = "Maximum Dose Value",
    p0 = "P(MTD < Min Dose)",
    p1 = "P(MTD > Max Dose)",
    condition = "Condition (a + b)",
    M = "Total Number of Trials",
    trial_simulation_scenarios = "Total Number of Trials",
    max_dose_increment = "Maximum Dose Increment",
    deltastop = "Threshold Delta",
    trial_mode = "Accelerated EWOC",
    max_accel_dose_level = "Maximum Dose Level for Acceleration",
    true_mtd = "True MTD Dose",
    true_p_min = "True P(DLT) at Minimum Dose",
    dose_probability = "True Probability of DLT",
    seed = "Seed Value",
    burn = "Burn-in",
    mm = "MCMC Samples",
    lower_f_bound = "Lower Feasibility Bound",
    increment = "Feasibility Bound Increment",
    feasibility_rule = "Feasibility Information",
    alpha_increment_rule = "Toxicity-Dependent Overdose Control",
    alpha_increment_cutoff = "Custom Toxicity Bound",
    stopping_rule = "Trial Efficiency Stopping",
    num_patients_restrict = "Maximum Number of Patients at a Dose",
    efficiency_stop_tolerance = "Dose-Region Width (%)",
    tree_steps = "Decision Steps to Expand",
    report_title = "Name of the Trial",
    phase1_design_download = "Phase 1 Design",
    conduct_workbook_download = "Trial Conduct Workbook",
    conduct_design_entry_mode = "Trial Conduct Input Method",
    conduct_workbook_upload = "Upload Trial Conduct Workbook",
    conduct_p0 = "P(MTD < Min)",
    conduct_p1 = "P(MTD > Max)",
    conduct_beta = "Beta Hyperparameters",
    conduct_dose_type = "Dose Type",
    conduct_actual_dose_values = "Actual Dose Values",
    conduct_theta = "Theta",
    conduct_deltastop = "Stopping Threshold Delta",
    conduct_lower_f_bound = "\u03b1",
    conduct_increment = "Increment",
    conduct_seed = "Seed",
    conduct_burn = "Burn-in",
    conduct_mm = "MCMC Samples",
    conduct_patient_rows = "Current Trial Data"
  )
  if (nzchar(key) && key %in% names(names_map)) {
    names_map[[key]]
  } else if (length(topic_key) == 1L && !is.null(topic_key)) {
    as.character(topic_key)
  } else {
    "Input"
  }
}

guide_input_explanation_text <- function(topic_key) {
  key <- if (length(topic_key) == 1L && !is.null(topic_key)) as.character(topic_key) else ""
  if (nzchar(key) && !is.null(guide_input_explanations[[key]])) {
    guide_input_explanations[[key]]
  } else {
    "This input is part of the MEWOC workflow. Review the protocol and app user guide for how it should be completed."
  }
}

guide_help_icon <- function(topic_key) {
  tags$button(
    type = "button",
    class = "guide-help-icon",
    title = paste("Explain", guide_input_title(topic_key)),
    onclick = sprintf(
      "Shiny.setInputValue('guide_help_topic', '%s', {priority: 'event'});",
      topic_key
    ),
    "?"
  )
}

guide_input_label <- function(label, topic_key) {
  tagList(
    tags$span(label),
    guide_help_icon(topic_key)
  )
}

guide_group_explanations <- list(
  trial_setup_inputs = list(
    title = "Trial Setup Inputs",
    items = c(
      "Dose Type: choose discrete dose levels or a continuous dose range.",
      "Total Number of Patients: planned maximum sample size for the trial.",
      "Cohort Size: number of patients treated before each model update.",
      "Target Toxicity: target DLT probability used to define the MTD.",
      "Number of Dose Levels: count of planned discrete dose levels.",
      "Starting Dose Level: protocol-specified dose level where the trial begins."
    )
  ),
  actual_dose_inputs = list(
    title = "Actual Dose Values and Units",
    items = c(
      "Enter the real clinical dose amount for each discrete dose level.",
      "Enter the measurement unit beside each dose value, such as mg or mg/m2.",
      "Dose values should be entered in increasing order and should match the protocol dose table.",
      "These values are used for reporting, documents, and conduct workbooks; they do not replace protocol review."
    )
  ),
  prior_inputs = list(
    title = "Design-Specific Prior Information",
    items = c(
      "DLT: dose-limiting toxicity. rho0: probability of DLT at the minimum dose. rho1: probability of DLT at the maximum dose.",
      "P(MTD < Min Dose): prior belief that even the minimum dose may be too toxic.",
      "P(MTD > Max Dose): prior belief that the available dose range may be conservative.",
      "Condition (a + b): total sum of the shape parameters of the beta priors of rho1 and rho0/rho1.",
      "Run Prior Calculations: finds, by BOBYQA search, the beta hyperparameters that give the two prior probabilities; they are used by the simulation, document and conduct steps."
    )
  ),
  trial_simulation_inputs = list(
    title = "Required Trial Information",
    items = c(
      "Total Number of Trials: number of virtual trials used to estimate operating characteristics.",
      "Scenario truth inputs: assumed true DLT probabilities or true continuous toxicity curve, for simulation only.",
      "Threshold Delta: safety stop when the minimum dose is too toxic (formula above); no MTD is recommended after this stop.",
      "No untried dose level is skipped (categorical doses); continuous doses increase by at most the maximum dose increment.",
      "Accelerated EWOC: optional one-patient-per-level lead-in for categorical doses, ending at the first DLT or the maximum dose level for acceleration.",
      "At the end of the trial (if it is not stopped for safety), the MTD is the posterior median of the MTD, rounded to the nearest dose level for categorical doses.",
      "Seed, Burn-in, and MCMC Samples: reproducibility and posterior computation settings."
    )
  ),
  feasibility_inputs = list(
    title = "Feasibility Inputs",
    items = c(
      "Lower Feasibility Bound alpha: starting EWOC overdose-control quantile.",
      "Feasibility Bound Increment: amount alpha can increase during the trial.",
      "Toxicity-dependent overdose control: optional rule controlling when alpha may increase.",
      "Custom Toxicity Bound: optional threshold used when custom toxicity-dependent control is selected."
    )
  ),
  stopping_inputs = list(
    title = "Stopping Rule Inputs",
    items = c(
      "Efficiency Stopping Rule: optional; the trial stops when the maximum number of patients has already been treated at the recommended next dose.",
      "Maximum number of patients at a dose: the limit used by the efficiency stopping rule (evaluable patients).",
      "Dose-region width (continuous doses): doses within plus or minus half this width of the recommended dose count as the same dose."
    )
  ),
  trial_documents_inputs = list(
    title = "Dose Escalation Tree",
    items = "Model-based dose assignments for each possible DLT outcome. STOP TRIAL: Delta safety stop; EFF STOP: efficiency stop (if selected). Accelerated EWOC: the tree starts at the first EWOC decision, after the lead-in reaches the maximum acceleration dose with no DLT."
  ),
  conduct_upload_inputs = list(
    title = "Trial Conduct Upload Inputs",
    items = c(
      "Trial Conduct Input Method: choose workbook upload or manual design entry.",
      "Upload Trial Conduct Workbook: imports design settings from the workbook generated under Trial Documents.",
      "Design upload status confirms whether the workbook was read and applied."
    )
  ),
  conduct_manual_inputs = list(
    title = "Trial Conduct Manual Inputs",
    items = c(
      "Prior fields and beta hyperparameters should match the finalized design.",
      "Dose Type, dose range or dose levels, theta, Delta, alpha, and increment should match the protocol.",
      "Seed, Burn-in, and MCMC Samples support reproducible posterior calculations."
    )
  ),
  conduct_data_inputs = list(
    title = "Current Trial Data",
    items = "Review the entered data, then tick the check box to proceed."
  )
)

guide_group_title <- function(group_key) {
  group <- guide_group_explanations[[group_key]]
  if (!is.null(group$title)) group$title else group_key
}

guide_group_help_icon <- function(group_key) {
  tags$button(
    type = "button",
    class = "guide-panel-help-icon",
    title = paste("Explain", guide_group_title(group_key)),
    onclick = sprintf(
      "Shiny.setInputValue('guide_help_group', '%s', {priority: 'event'});",
      group_key
    ),
    "?"
  )
}

guide_panel_header <- function(title, group_key, level = 4) {
  header_tag <- if (identical(as.integer(level), 3L)) tags$h3 else tags$h4
  div(
    class = "guide-panel-header",
    header_tag(title),
    guide_group_help_icon(group_key)
  )
}

guide_subsection_header <- function(title, group_key) {
  div(
    class = "guide-subsection-header",
    tags$span(title),
    guide_group_help_icon(group_key)
  )
}

## Help pop-ups opened by the "?" icons (fixed text; nothing is sent outside the app).
guide_input_help_modal <- function(topic_key) {
  modalDialog(
    title = guide_input_title(topic_key),
    if (!is.null(guide_input_html[[topic_key]])) HTML(guide_input_html[[topic_key]]) else guide_formula_ui(guide_input_formulas[[topic_key]]),
    if (is.null(guide_input_html[[topic_key]])) tags$p(guide_input_explanation_text(topic_key)),
    easyClose = TRUE,
    footer = modalButton("Close")
  )
}

guide_group_help_modal <- function(group_key) {
  group <- guide_group_explanations[[group_key]]
  if (is.null(group)) {
    group <- list(
      title = guide_group_title(group_key),
      items = "This section contains MEWOC workflow inputs. Review the protocol and app user guide before using outputs."
    )
  }
  modalDialog(
    title = group$title,
    if (!is.null(guide_group_html[[group_key]])) HTML(guide_group_html[[group_key]]),
    guide_formula_ui(guide_group_formulas[[group_key]]),
    if (is.null(guide_group_html[[group_key]])) tags$ul(lapply(group$items, tags$li)),
    easyClose = TRUE,
    footer = modalButton("Close")
  )
}

## Help tab (29 Oct 2026): About, App Workflow, Design & Conduct. HTML is written with entities so the source stays ASCII.
help_page_css <- "
.mewoc-help { font-size: 14px; line-height: 1.5; }
.mewoc-help .help-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 14px; }
.mewoc-help .help-card { background: #fff; border: 1px solid #d9dee3; border-top: 5px solid #2f7ea8; border-radius: 8px; padding: 12px 16px 14px; }
.mewoc-help .help-card h3 { font-size: 16.5px; margin: 0 0 8px; display: flex; align-items: center; gap: 8px; }
.mewoc-help .help-num { display: inline-flex; width: 26px; height: 26px; border-radius: 50%; align-items: center; justify-content: center; background: #2f7ea8; color: #fff; font-size: 13px; font-weight: 700; flex: none; }
.mewoc-help .help-card dl { margin: 0; display: grid; grid-template-columns: 70px 1fr; row-gap: 5px; column-gap: 8px; }
.mewoc-help .help-card dt { font-weight: 700; color: #5d6670; font-size: 12.5px; text-transform: uppercase; letter-spacing: .02em; padding-top: 1px; }
.mewoc-help .help-card dd { margin: 0; }
.mewoc-help .c-setup { border-top-color: #2a78d6; } .mewoc-help .c-setup .help-num { background: #2a78d6; }
.mewoc-help .c-prior { border-top-color: #6f42c1; } .mewoc-help .c-prior .help-num { background: #6f42c1; }
.mewoc-help .c-sim { border-top-color: #17a2b8; } .mewoc-help .c-sim .help-num { background: #17a2b8; }
.mewoc-help .c-docs { border-top-color: #28a745; } .mewoc-help .c-docs .help-num { background: #28a745; }
.mewoc-help .c-conduct { border-top-color: #eb6834; } .mewoc-help .c-conduct .help-num { background: #eb6834; }
.mewoc-help .help-section-title { font-size: 17px; font-weight: 700; margin: 24px 0 10px; padding-bottom: 4px; border-bottom: 2px solid #e6ebef; }
.mewoc-help .help-workflow-img { display: block; width: 666px; max-width: 100%; height: auto; margin: 0 auto 8px; border: 1px solid #d9dee3; border-radius: 6px; }
.mewoc-help details.help-box { background: #fff; border: 1px solid #d9dee3; border-left: 5px solid #2f7ea8; border-radius: 8px; margin: 0 0 12px; }
.mewoc-help details.help-box > summary { cursor: pointer; list-style: none; padding: 11px 16px; font-weight: 700; font-size: 16px; display: flex; align-items: center; gap: 10px; }
.mewoc-help details.help-box > summary::-webkit-details-marker { display: none; }
.mewoc-help details.help-box > summary::after { content: '\\25BE'; color: #5d6670; font-size: 14px; margin-left: auto; transition: transform .15s; }
.mewoc-help details.help-box[open] > summary::after { transform: rotate(180deg); }
.mewoc-help details.help-box .help-body { padding: 0 18px 14px; }
.mewoc-help .b-design { border-left-color: #6f42c1; } .mewoc-help .b-conduct { border-left-color: #eb6834; }
.mewoc-help .b-end { border-left-color: #28a745; } .mewoc-help .b-ref { border-left-color: #9aa5b1; }
.mewoc-help .help-plain { margin: 0 0 8px; }
.mewoc-help .help-eq { text-align: center; margin: 8px 0 10px; padding: 9px 12px; background: #f6f8fa; border-left: 3px solid #2f7ea8; border-radius: 0 4px 4px 0; font-family: 'Cambria Math', Cambria, Georgia, serif; font-size: 16.5px; }
.mewoc-help .help-frac { display: inline-flex; flex-direction: column; vertical-align: middle; text-align: center; }
.mewoc-help .help-frac > span:first-child { border-bottom: 1px solid; padding: 0 4px; } .mewoc-help .help-frac > span:last-child { padding: 0 4px; }
.mewoc-help ul.help-tight { margin: 4px 0 6px; padding-left: 20px; } .mewoc-help ul.help-tight li, .mewoc-help ol.help-tight li { margin: 3px 0; }
.mewoc-help .help-pill { display: inline-block; font-size: 11.5px; font-weight: 700; border-radius: 10px; padding: 1px 8px; margin-right: 4px; vertical-align: 1px; }
.mewoc-help .p-disc { background: #e7f0fb; color: #1d4f91; } .mewoc-help .p-cont { background: #e9f7f1; color: #1d6b48; } .mewoc-help .p-opt { background: #f1f3f5; color: #4a5560; }
.mewoc-help .help-note { font-size: 13px; color: #5d6670; }
.mewoc-help .help-notation { background: #fbfcfd; border: 1px solid #d9dee3; border-radius: 8px; padding: 10px 16px 12px; margin-bottom: 14px; }
.mewoc-help .help-notation h3 { font-size: 16px; margin: 0 0 8px; }
.mewoc-help dl.help-sym { margin: 0; display: grid; grid-template-columns: 78px 1fr 78px 1fr; row-gap: 4px; column-gap: 8px; }
.mewoc-help dl.help-sym dt { font-family: 'Cambria Math', Cambria, Georgia, serif; font-size: 16px; font-weight: 400; }
.mewoc-help dl.help-sym dd { margin: 0; }
.mewoc-help .help-phase { font-weight: 700; color: #b2491f; font-size: 13px; text-transform: uppercase; letter-spacing: .03em; margin: 12px 0 6px; }
.mewoc-help ol.help-steps { list-style: none; padding: 0; margin: 0; }
.mewoc-help ol.help-steps > li { position: relative; padding: 4px 0 8px 40px; }
.mewoc-help ol.help-steps > li > .help-step-num { position: absolute; left: 4px; top: 4px; width: 24px; height: 24px; border-radius: 50%; border: 2px solid #eb6834; color: #b2491f; font-weight: 700; font-size: 12.5px; display: flex; align-items: center; justify-content: center; }
.mewoc-help ol.help-steps b.help-t { display: block; }
.mewoc-help .help-cycle { border: 1px solid #f3cdb3; background: #fffaf6; border-radius: 8px; padding: 6px 14px 4px; margin: 2px 0 10px 40px; }
.mewoc-help .help-rule { border-left: 3px solid #f3cdb3; padding: 2px 0 2px 12px; margin: 8px 0; }
.mewoc-help .help-rule > b { color: #b2491f; }
.mewoc-help table.help-t { width: 100%; border-collapse: collapse; background: #fff; }
.mewoc-help table.help-t th, .mewoc-help table.help-t td { border-bottom: 1px solid #eef0f2; padding: 5px 6px; vertical-align: top; text-align: left; }
.mewoc-help table.help-t th { font-size: 12.5px; color: #5d6670; border-bottom: 2px solid #d9dee3; }
.mewoc-help table.help-t td.help-val { white-space: nowrap; font-weight: 600; }
.mewoc-help table.help-t tr.help-grp td { font-weight: 700; color: #1f2d3a; padding-top: 9px; font-size: 12.5px; text-transform: uppercase; letter-spacing: .03em; }
.mewoc-help .help-two-col { display: grid; grid-template-columns: 1fr 1fr; gap: 0 24px; }
.mewoc-help .help-msg { font-style: italic; }
.mewoc-help .help-sev { display: inline-block; width: 58px; text-align: center; font-size: 11px; font-weight: 700; border-radius: 10px; padding: 1px 0; }
.mewoc-help .sev-err { background: #fdecec; color: #b42318; } .mewoc-help .sev-warn { background: #fff6dd; color: #9a6700; } .mewoc-help .sev-stop { background: #eceff3; color: #33404d; }
.mewoc-help .help-search { width: 100%; max-width: 360px; padding: 6px 10px; border: 1px solid #ccc; border-radius: 4px; margin: 0 0 8px; font-size: 13.5px; }
.mewoc-help .help-about { background: #f8fff9; border: 2px solid #28a745; border-radius: 8px; padding: 14px 20px; }
.mewoc-help .help-about h3 { margin: 4px 0 6px; font-size: 17px; }
.mewoc-help .help-about h4 { margin: 16px 0 4px; font-size: 15px; }
.mewoc-help .help-about a { color: #1e6b3a; }
.mewoc-help .help-meta { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 6px; }
.mewoc-help .help-meta span { background: #fff; border: 1px solid #bfe3cb; border-radius: 14px; padding: 3px 12px; font-size: 13px; }
.mewoc-help .help-meta a.help-weblink { background: #28a745; color: #fff; border-radius: 14px; padding: 3px 12px; font-size: 13px; font-weight: 600; text-decoration: none; }
.mewoc-help .help-meta a.help-weblink:hover { background: #1e7e34; }
.mewoc-help .help-cite { background: #fff; border: 1px solid #bfe3cb; border-left: 4px solid #28a745; border-radius: 6px; padding: 8px 12px; margin: 0 0 8px; }
@media (max-width: 760px) {
  .mewoc-help dl.help-sym { grid-template-columns: 78px 1fr; } .mewoc-help .help-two-col { grid-template-columns: 1fr; }
  .mewoc-help .help-card dl { grid-template-columns: 1fr; } .mewoc-help .help-cycle { margin-left: 0; }
}
"

## Search box in Common messages: hide table rows that do not contain the typed text.
help_page_js <- "
$(document).on('input', '.mewoc-help .help-search', function () {
  var q = $(this).val().trim().toLowerCase();
  $('#' + $(this).data('filter') + ' tr').each(function (i) {
    if (i === 0) return;
    $(this).toggle(!q || $(this).text().toLowerCase().indexOf(q) >= 0);
  });
});
"

help_web_app_url <- "https://cshsbiostats.github.io/MEWOC_Shiny_App/"

help_about_ui <- function() {
  div(
    class = "help-about",
    HTML(paste0(
      "<h3>About MEWOC</h3>",
      "<div class='help-meta'><span>Version 0.0.0.9000</span><span>29 Oct 2026</span><span>License GPL-3</span><span>R / Shiny, JAGS</span>",
      "<a class='help-weblink' href='", help_web_app_url, "' target='_blank' rel='noopener'>Web app &#8599;</a></div>",
      "<h4>Purpose</h4>",
      "<p class='help-plain'>Open-source R Shiny application for Bayesian phase I dose finding with escalation with overdose control (EWOC), for categorical dose levels or a continuous dose range. Its outputs support, but do not replace, review by the study statistician and clinical team as specified in the protocol.</p>",
      "<p class='help-plain'>MEWOC can be used online without installation at <a href='", help_web_app_url, "' target='_blank' rel='noopener'>", help_web_app_url, "</a>, or installed locally as an R package.</p>",
      "<h4>Method</h4>",
      "<p class='help-plain'>MEWOC uses the reparameterized EWOC model in which clinicians give prior probabilities that the MTD lies below the minimum or above the maximum trial dose. Beta prior hyperparameters are calibrated with the BOBYQA optimization algorithm, and the prior DLT probabilities of the planned doses are displayed. Posterior computation uses MCMC (JAGS).</p>",
      "<h4>How to cite</h4>",
      "<p class='help-plain'>If you use MEWOC, please cite:</p>",
      "<p class='help-cite'>Kabir, S., Fan, X., &amp; Tighiouart, M. (2026). <i>MEWOC: Modified Escalation with Overdose Control for Single-Agent Phase I Trials</i>. R package version 0.1.0.</p>",
      "<p class='help-plain'>Related presentation: Kabir, S., &amp; Tighiouart, M. (2026). <i>Modified EWOC (MEWOC): A Shiny Application for Single-Agent Phase I Dose-Finding Trials</i>. Joint Statistical Meeting 2026, Boston, MA. ",
      "<a href='https://doi.org/10.5281/zenodo.23254550' target='_blank' rel='noopener'>https://doi.org/10.5281/zenodo.23254550</a></p>",
      "<h4>References</h4>",
      "<ol class='help-tight'>",
      "<li>Babb, J., Rogatko, A., and Zacks, S. Cancer phase I clinical trials: efficient dose escalation with overdose control. <i>Statistics in Medicine</i>, 1998; 17: 1103&ndash;1120.</li>",
      "<li>Tighiouart, M., Rogatko, A., and Babb, J. S. Flexible Bayesian methods for cancer phase I clinical trials: dose escalation with overdose control. <i>Statistics in Medicine</i>, 2005; 24: 2183&ndash;2196.</li>",
      "<li>Tighiouart, M. and Rogatko, A. Dose finding with escalation with overdose control in cancer clinical trials. <i>Statistical Science</i>, 2010; 25: 217&ndash;226.</li>",
      "<li>Wheeler, G. M., Sweeting, M. J., and Mander, A. P. Toxicity-dependent feasibility bounds for the escalation with overdose control approach in phase I cancer trials. <i>Statistics in Medicine</i>, 2017; 36: 2499&ndash;2513.</li>",
      "</ol>"
    ))
  )
}

help_workflow_ui <- function() {
  card <- function(cls, n, title, input_text, output_text) {
    paste0("<div class='help-card ", cls, "'><h3><span class='help-num'>", n, "</span>", title, "</h3><dl>",
           "<dt>Input</dt><dd>", input_text, "</dd><dt>Output</dt><dd>", output_text, "</dd></dl></div>")
  }
  HTML(paste0(
    "<div class='help-grid'>",
    card("c-setup", 1, "Trial Setup",
         "Dose type (categorical levels or continuous range), total patients N, cohort size, target toxicity &theta;, and the doses with units.",
         "The setup used by every later tab (click <b>Submit</b>)."),
    card("c-prior", 2, "Prior Information",
         "Clinician probabilities that the MTD is below the minimum and above the maximum dose; condition a + b; seed.",
         "Beta prior hyperparameters and the prior DLT probability plot. Check that the prior is clinically plausible."),
    card("c-sim", 3, "Trial Simulation",
         "True toxicity scenarios, number of trials, &Delta;, &alpha; and its increment, optional rules.",
         "Operating characteristics: MTD selection, patient allocation, DLT rates, stopping summaries; downloadable report."),
    card("c-docs", 4, "Trial Documents",
         "Decision steps for the tree; optional name of the trial.",
         "Dose escalation tree, Phase 1 Design (Word/HTML) and the Trial Conduct workbook (Excel)."),
    card("c-conduct", 5, "Trial Conduct",
         "The Trial Conduct workbook (or design values by hand) and each patient's dose and DLT status.",
         "Next-cohort dose, warnings, updated workbook, and the end-of-trial MTD report."),
    "</div>",
    "<div class='help-section-title'>Workflow at a glance</div>",
    "<img class='help-workflow-img' src='mewoc-images/mewoc_workflow.png' alt='MEWOC workflow: Trial Setup, Prior Information, Trial Simulation, Trial Documents, Trial Conduct'>"
  ))
}

help_design_conduct_ui <- function() {
  step <- function(n, title, body) paste0("<li><span class='help-step-num'>", n, "</span><b class='help-t'>", title, "</b>", body, "</li>")
  rule <- function(title, body) paste0("<div class='help-rule'><b>", title, "</b>", body, "</div>")
  eq <- function(...) paste0("<div class='help-eq'>", ..., "</div>")
  disc <- "<span class='help-pill p-disc'>Categorical</span>"
  cont <- "<span class='help-pill p-cont'>Continuous</span>"
  opt <- "<span class='help-pill p-opt'>if selected</span>"
  sym <- function(s, meaning) paste0("<dt>", s, "</dt><dd>", meaning, "</dd>")
  msg <- function(sev, label, text, todo) paste0("<tr><td><span class='help-sev sev-", sev, "'>", label, "</span></td><td class='help-msg'>", text, "</td><td>", todo, "</td></tr>")
  dflt <- function(item, value) paste0("<tr><td>", item, "</td><td class='help-val'>", value, "</td></tr>")
  grp <- function(title) paste0("<tr class='help-grp'><td colspan='2'>", title, "</td></tr>")
  HTML(paste0(

    ## Notation
    "<div class='help-notation'><h3>Notation</h3><dl class='help-sym'>",
    sym("&theta;", "Target toxicity (DLT probability at the MTD)"),
    sym("&gamma;, &gamma;&#770;", "MTD and its estimate"),
    sym("X<sub>min</sub>, X<sub>max</sub>", "Minimum and maximum trial dose"),
    sym("&rho;<sub>0</sub>, &rho;<sub>1</sub>", "DLT probability at X<sub>min</sub> and X<sub>max</sub>"),
    sym("a, b", "Beta prior hyperparameters"),
    sym("&alpha;", "Feasibility bound (at most 0.5)"),
    sym("&Delta;", "Safety-stop threshold"),
    sym("D<sub>n</sub>", "Data of the n evaluable patients"),
    sym("P&#770;<sub>DLT</sub>", "Observed DLT rate"),
    sym("N, M<sub>max</sub>", "Maximum sample size; maximum patients at a dose"),
    "</dl></div>",

    ## 2. Design
    "<details class='help-box b-design' open><summary>Before the trial: design</summary><div class='help-body'>",
    "<p class='help-plain'><b>Model.</b> Monotone logistic model for the probability of a DLT at dose x:</p>",
    eq("Pr(DLT | x) = F(&beta;<sub>0</sub> + &beta;<sub>1</sub>x),&emsp; &beta;<sub>1</sub> &gt; 0,&emsp; F(u) = {1 + exp(&minus;u)}<sup>&minus;1</sup>"),
    "<p class='help-plain'>The MTD &gamma; is the dose with Pr(DLT | &gamma;) = &theta;. Written with &rho;<sub>0</sub> = Pr(DLT | X<sub>min</sub>) and &rho;<sub>1</sub> = Pr(DLT | X<sub>max</sub>):</p>",
    eq("&gamma; = <span class='help-frac'><span>F<sup>&minus;1</sup>(&theta;) &minus; F<sup>&minus;1</sup>(&rho;<sub>0</sub>)</span><span>F<sup>&minus;1</sup>(&rho;<sub>1</sub>) &minus; F<sup>&minus;1</sup>(&rho;<sub>0</sub>)</span></span>"),
    "<p class='help-plain'><b>Prior.</b></p>",
    eq("&rho;<sub>1</sub> ~ Beta(a<sub>0</sub>, b<sub>0</sub>) truncated to [0.001, 0.999],&emsp; &rho;<sub>0</sub>/&rho;<sub>1</sub> ~ Beta(a<sub>1</sub>, b<sub>1</sub>)"),
    "<ul class='help-tight'>",
    "<li>BOBYQA calibrates the hyperparameters to the clinician's Pr(MTD &lt; X<sub>min</sub>) and Pr(MTD &gt; X<sub>max</sub>).</li>",
    "<li>Condition a + b (default 2): the prior weighs about as much as 2 patients, so the data soon dominate.</li>",
    "</ul>",
    "<p class='help-plain'><b>Fixed in advance</b> (used during the trial): starting &alpha; and its increment, &Delta;, the maximum dose increment (continuous), and the optional rules: toxicity bound for the &alpha; increase, efficiency stop (M<sub>max</sub>, dose-region width), and accelerated lead-in (maximum dose level for acceleration).</p>",
    "</div></details>",

    ## 3. Conduct
    "<details class='help-box b-conduct' open><summary>During the trial: each cohort</summary><div class='help-body'>",
    "<div class='help-phase'>Before evaluating</div><ol class='help-steps'>",
    step(1, "Choose the input method", "Upload the completed Trial Conduct workbook (template from Trial Documents), or choose <b>Manual entry</b>: the Trial Design Information then opens for the design values."),
    step(2, "Check the Trial Design Information", "All values must match the final design (see Before the trial)."),
    step(3, "Enter or review the patient rows", paste0(
      "One row per patient: cohort, dose given, DLT status. Only evaluable patients form the data; the model uses the dose actually given:",
      eq("D<sub>n</sub> = { (x<sub>i</sub>, y<sub>i</sub>) : i = 1, &hellip;, n },&emsp; y<sub>i</sub> = 1 (DLT) or 0 (No DLT)"),
      "<span class='help-note'>Pending (blank) and Unevaluable patients are shown separately and not counted.</span>")),
    "</ol>",
    "<div class='help-phase'>For each cohort, in this order (after ticking the review box)</div><div class='help-cycle'>",
    rule("a. Accelerated lead-in ", paste0(disc, opt, "<br>One patient at a time, one level up per patient, until the first DLT or the maximum dose level for acceleration. No model fit and no &Delta; check; lead-in patients count toward N.")),
    rule("b. Safety check", paste0(
      eq("Stop when&nbsp; Pr{ Pr(DLT | X = X<sub>min</sub>) &gt; &theta; | D<sub>n</sub> } &gt; &Delta;"),
      "<span class='help-note'>Default &Delta; = 0.85. No dose is recommended as the MTD after this stop.</span>")),
    rule("c. Update &alpha;", paste0(
      eq("&alpha; &larr; min(&alpha; + increment, 0.5)&nbsp; only if&nbsp; P&#770;<sub>DLT</sub> = <span class='help-frac'><span>total DLTs</span><span>evaluable patients</span></span> &lt; &theta;"),
      "<span class='help-note'>The first EWOC decision uses the starting &alpha;. A custom toxicity bound below &theta; can replace &theta;.</span>")),
    rule("d. Next dose", paste0(
      eq("Model-based next dose = posterior MTD &alpha;-quantile, subject to dose limits"),
      "<span class='help-note'>", disc, "nearest dose level; rounding can give posterior overdose probability above &alpha;. No untried level is skipped. ", cont,
      "x<sub>next</sub> &le; x<sub>current</sub> + the maximum dose increment (% of the dose range).</span>")),
    rule(paste0("e. Efficiency check ", opt), paste0(
      eq("Stop when&nbsp; n(x<sub>next</sub>) &ge; M<sub>max</sub>"),
      "<span class='help-note'>n(x<sub>next</sub>) = evaluable patients already treated at x<sub>next</sub>. ", cont,
      "doses within &plusmn; half the dose-region width count as x<sub>next</sub>. The MTD is then estimated from the data.</span>")),
    "</div>",
    "<div class='help-phase'>After evaluating</div><ol class='help-steps'>",
    step(4, "Read the recommendation and warnings", paste0(
      "The panel shows x<sub>next</sub> and &alpha; for the next cohort, or the stop.<ul class='help-tight'>",
      "<li><b>Incomplete cohort data:</b> fewer than the minimum cohort size are evaluable; x<sub>next</sub> and &alpha; may change once results are entered.</li>",
      "<li><b>Pending DLT status:</b> pending patients would trigger the efficiency stop once evaluable (no stop yet).</li>",
      "<li><b>Dose checks:</b> dose given differs from the recorded recommendation, a skipped level in the lead-in, or a continuous dose above the maximum increment.</li>",
      "<li><b>Invalid dose</b> (not a protocol dose, text, or blank with a DLT result) stops the evaluation until corrected; an unknown DLT status leaves that row out.</li>",
      "</ul>")),
    step(5, "Download the updated workbook", "Same input columns plus a Warning column; recalculated &alpha; and blank recommendations are filled in, and the last row is the next cohort. After a stop, later rows are marked <i>not used</i>."),
    "</ol></div></details>",

    ## 4. End of trial
    "<details class='help-box b-end'><summary>End of trial: MTD</summary><div class='help-body'>",
    "<p class='help-plain'>When N evaluable patients are reached, select <b>trial completed</b>. MEWOC repeats the safety check (rule b) on all data D<sub>N</sub>; if it does not stop, the MTD estimate is the posterior median:</p>",
    eq("&gamma;&#770; = median(&gamma; | D<sub>N</sub>)"),
    "<span class='help-note'>", disc, "reported as the nearest dose level. ", cont, "reported as a dose in the trial's unit.</span>",
    "</div></details>",

    ## 5. Defaults
    "<details class='help-box b-ref'><summary>Default values</summary><div class='help-body help-two-col'>",
    "<table class='help-t'>",
    grp("Trial Setup"), dflt("&theta; / N / cohort size", "0.33 / 30 / 1"), dflt("Dose levels / starting level", "4 / 1"),
    grp("Prior Information"), dflt("P(MTD &lt; Min) / P(MTD &gt; Max)", "0.1 / 0.2"), dflt("Condition a + b / seed", "2 / 988"),
    grp("Trial Documents"), dflt("Tree decision steps", "4"),
    "</table><table class='help-t'>",
    grp("Trial Simulation"), dflt("Number of trials / &Delta;", "10 / 0.85"), dflt("&alpha; / increment / toxicity bound", "0.25 / 0.05 / &theta;"),
    dflt("Burn-in / MCMC samples", "1000 / 1500"), dflt("Max dose increment (continuous)", "10%"), dflt("Efficiency stop / accelerated EWOC", "Off / No"),
    "</table></div></details>",

    ## 6. Common messages
    "<details class='help-box b-ref'><summary>Common messages</summary><div class='help-body'>",
    "<input class='help-search' type='search' placeholder='Search, e.g. prior, pending, dose' data-filter='help_msg_table'>",
    "<table class='help-t' id='help_msg_table'><tr><th style='width: 70px;'>Type</th><th>Message</th><th>What to do</th></tr>",
    msg("err", "Error", "Complete Trial Setup and click Submit&hellip;", "Fill in Trial Setup, click Submit."),
    msg("err", "Error", "Run 'Run Prior Calculations' before&hellip;", "Run the prior in Prior Information."),
    msg("err", "Error", "Run at least one Trial Simulation scenario&hellip;", "Run one scenario, then download."),
    msg("err", "Error", "Accelerated EWOC is selected. Please provide the maximum dose level&hellip;", "Enter the level, or set Accelerated EWOC to No."),
    msg("err", "Error", "Confirm that you reviewed the trial design information and current trial data&hellip;", "Tick the review box."),
    msg("err", "Error", "Dose &hellip; not one of the doses listed in the protocol", "Correct the dose."),
    msg("warn", "Warning", "Incomplete cohort data&hellip; / Pending DLT status&hellip;", "Re-evaluate when results are in."),
    msg("warn", "Warning", "Dose given exceeds the maximum dose increment&hellip; / A dose level was skipped&hellip;", "Check the dose against the record."),
    msg("warn", "Warning", "Please rebuild the tree after changing the cohort size.", "Rebuild the tree."),
    msg("stop", "Stop", "&hellip;greater than Delta&hellip; No dose is recommended as the MTD.", "Follow the protocol's safety review."),
    msg("stop", "Stop", "Stopped for efficiency (MTD estimated from the data)", "Review the MTD report with the team."),
    "</table></div></details>"
  ))
}

## Whole Help tab: About first, then App Workflow and Design & Conduct.
help_page_ui <- function() {
  div(
    class = "mewoc-help",
    tags$style(HTML(help_page_css)),
    tags$script(HTML(help_page_js)),
    tabsetPanel(
      id = "help_tabs",
      tabPanel("About", br(), help_about_ui()),
      tabPanel("App Workflow", br(), help_workflow_ui()),
      tabPanel("Design & Conduct", br(), help_design_conduct_ui())
    )
  )
}
