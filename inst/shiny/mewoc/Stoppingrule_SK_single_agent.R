## Single-agent app generated from Combined/Stoppingrule_SK_merged.R.
## Combined source is intentionally left unchanged.

required_pkgs <- c(
  "shiny",
  "dplyr",
  "tidyr",
  "ggplot2",
  "ggtext",
  "rjags",
  "parallel",
  "coda",
  "future",
  "promises",
  "readxl",
  "openxlsx"
)

load_required_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      paste0(
        "Missing required packages: ", paste(missing, collapse = ", "), ".\n",
        "Install them before starting the app. For reproducible setup, prefer ",
        "a version-locked workflow such as renv::restore()."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_required_packages(required_pkgs)
# Make unqualified app dependencies available without attaching them to the
# caller's search path. Later packages take precedence, as with library().
app_environment <- environment()
app_imports <- new.env(parent = parent.env(app_environment))
for (pkg in required_pkgs) {
  exports <- getNamespaceExports(pkg)
  list2env(stats::setNames(lapply(exports, function(name) getExportedValue(pkg, name)), exports), app_imports)
}
parent.env(app_environment) <- app_imports
# SK 2026-08-26: By default trial simulations use all available logical cores
# except one. Set options(mewoc.max_trial_workers = <n>) before launching the app
# if a fixed lower cap is needed for a shared workstation. (The option is not reset here, so a cap
# set before launch is kept.)
# Simulations run in a background R session so the app stays responsive (progress text, Stop button).
# multisession with workers = 1 falls back to sequential evaluation in this session, which blocks the
# app until the run ends, so two background sessions are used (a run occupies one of them).

app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)

# Images shown in the app (e.g. the workflow figure on the Help > About tab).

helper_files <- file.path(
  app_dir,
  "R",
  c(
    "app_lifecycle.R",
    "trial_logic.R",
    "prior_logic.R",
    "conduct_logic.R",
    "user_guide_logic.R",
    "protocol_draft_logic.R",
    "phase1_design_logic.R",
    "tree_logic.R",
    "report_logic.R",
    "validation_helpers.R"
  )
)
invisible(lapply(helper_files, sys.source, envir = app_environment))

build_tree_plot_helper <- build_tree_plot
report_plot_to_data_uri_helper <- report_plot_to_data_uri
report_named_list_to_df_helper <- report_named_list_to_df
report_df_to_html_table_helper <- report_df_to_html_table
build_prior_interpretation_helper <- build_prior_interpretation
build_trial_interpretation_helper <- build_trial_interpretation
build_tree_interpretation_helper <- build_tree_interpretation
build_report_html_helper <- build_report_html
draw_report_pdf_text_page_helper <- draw_report_pdf_text_page
draw_report_pdf_table_page_helper <- draw_report_pdf_table_page
build_report_pdf_helper <- build_report_pdf

# rjags also requires the JAGS system library installed on the machine.
# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      input[type=number]::-webkit-outer-spin-button,
      input[type=number]::-webkit-inner-spin-button {
        -webkit-appearance: none;
        margin: 0;
      }
      input[type=number] {
        -moz-appearance: textfield;
        appearance: textfield;
      }
      .guide-help-icon,
      .guide-panel-help-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 22px;
        height: 22px;
        min-width: 22px;
        border-radius: 50%;
        border: 1px solid #2f7ea8;
        background: #eef7fb;
        color: #245f7f;
        font-size: 14px;
        font-weight: 700;
        line-height: 1;
        cursor: pointer;
        margin-left: 6px;
        padding: 0;
      }
      .guide-help-icon:hover,
      .guide-help-icon:focus,
      .guide-panel-help-icon:hover,
      .guide-panel-help-icon:focus {
        background: #d9edf7;
        outline: none;
      }
      .guide-panel-header,
      .guide-subsection-header {
        display: flex;
        align-items: center;
        gap: 6px;
        margin-bottom: 8px;
      }
      .guide-panel-header h3,
      .guide-panel-header h4,
      .guide-subsection-header span {
        margin: 0;
      }
      .guide-setup-help-row {
        display: flex;
        align-items: flex-start;
        gap: 8px;
      }
      .guide-setup-bar {
        display: flex;
        align-items: flex-start;
        gap: 12px;
        padding: 10px 0;
        border-bottom: 1px solid #e3e3e3;
      }
      .guide-setup-bar > .shiny-input-container,
      .guide-setup-bar > .guide-setup-field-group {
        flex: 1 1 0;
        min-width: 0;
      }
      .guide-setup-bar > .guide-help-icon {
        margin-top: 22px;
      }
      .guide-setup-bar .form-group {
        margin-bottom: 0;
      }
      .guide-setup-bar .row {
        width: 100%;
        margin-left: 0;
        margin-right: 0;
      }
      .guide-setup-dose-bar {
        align-items: flex-start;
      }
      .guide-setup-dose-content {
        flex: 1 1 auto;
        min-width: 0;
      }
      .guide-setup-dose-content > .form-group {
        margin-bottom: 10px;
      }
      .guide-setup-dose-content > .shiny-input-container {
        width: 100%;
      }
      .guide-setup-help-row > .shiny-input-container {
        flex: 1 1 auto;
        min-width: 0;
      }
      .guide-setup-help-row > .guide-help-icon {
        margin-top: 22px;
      }
      .guide-setup-help-row.setup-core-help > .guide-help-icon {
        margin-top: 4px;
      }
      .guide-setup-section-help {
        display: flex;
        align-items: center;
        gap: 8px;
        margin: 8px 0 6px 0;
      }
      #conduct_upload_ui .form-group,
      #conduct_upload_ui .shiny-input-container {
        width: 100%;
        max-width: none;
      }
      #conduct_upload_ui .input-group {
        width: 100%;
        min-height: 120px;
        align-items: center;
        border: 2px dashed #9fb7c9;
        border-radius: 8px;
        background-color: #f7fbff;
        padding: 24px 16px;
      }
      #conduct_upload_ui input[type='text'],
      #conduct_upload_ui .btn-file {
        min-height: 52px;
      }
      #conduct_patient_table {
        width: 100%;
        border-collapse: collapse;
      }
      #conduct_patient_table th {
        background-color: #f0f0f0;
        border: 1px solid #ccc;
        padding: 6px 8px;
        font-weight: 600;
        white-space: nowrap;
        text-align: center;
      }
      #conduct_patient_table td {
        border: 1px solid #ccc;
        padding: 4px 6px;
        vertical-align: middle;
        text-align: center;
      }
      #conduct_patient_table .form-group,
      #conduct_patient_table .shiny-input-container {
        margin-bottom: 0;
      }
      #conduct_patient_table input[type='text'],
      #conduct_patient_table input[type='number'],
      #conduct_patient_table select {
        height: 32px;
        padding: 2px 6px;
        font-size: 13px;
        width: 100%;
      }
      #conduct_design_manual_panel .form-group,
      #conduct_design_upload_panel .form-group {
        margin-bottom: 8px;
      }
      #conduct_design_manual_panel label,
      #conduct_design_upload_panel label {
        font-size: 12px;
        font-weight: 600;
        color: #444;
        margin-bottom: 2px;
      }
      #conduct_design_manual_panel input.form-control,
      #conduct_design_upload_panel input.form-control {
        height: 30px;
        padding: 3px 8px;
        font-size: 13px;
      }
      #conduct_design_manual_panel .radio label,
      #conduct_design_manual_panel .radio-inline label,
      #conduct_design_upload_panel .radio label,
      #conduct_design_upload_panel .radio-inline label {
        font-size: 13px;
        font-weight: normal;
        color: #333;
      }
      #conduct_design_manual_panel .radio-inline,
      #conduct_design_upload_panel .radio-inline {
        margin-right: 10px;
      }
      .design-section-label {
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        color: #666;
        margin-bottom: 8px;
        padding-bottom: 4px;
        border-bottom: 1px solid #e0e0e0;
      }
      .conduct-beta-formula {
        display: flex;
        align-items: center;
        gap: 4px;
        margin-bottom: 8px;
        white-space: nowrap;
        font-family: Consolas, 'Courier New', monospace;
        font-size: 13px;
      }
      .conduct-beta-formula .form-group,
      .conduct-beta-formula .shiny-input-container {
        width: 58px !important;
        margin: 0 !important;
      }
      .conduct-beta-formula input.form-control {
        width: 58px;
        text-align: center;
      }
      .run-button-clicked,
      .run-button-clicked:focus,
      .run-button-clicked:hover {
        color: white !important;
        background-color: #6c757d !important;
        border-color: #6c757d !important;
        box-shadow: none !important;
      }
      .conduct-final-mtd-running {
        margin-top: 10px;
        padding: 10px 12px;
        border: 1px solid #9fb7c9;
        border-radius: 8px;
        background-color: #f7fbff;
        color: #2f3a3f;
      }
      .conduct-final-mtd-progress {
        height: 8px;
        overflow: hidden;
        border-radius: 999px;
        background-color: #d9e7ef;
        margin-top: 8px;
      }
      .conduct-final-mtd-progress div {
        width: 40%;
        height: 100%;
        border-radius: 999px;
        background-color: #28a745;
        animation: conductFinalMtdProgress 1.1s linear infinite;
      }
      .details-summary-row {
        cursor: pointer;
        font-weight: 600;
      }
      .details-summary-row .details-toggle-label {
        display: inline-block;
        margin-left: 8px;
        padding: 2px 8px;
        border: 1px solid #ccc;
        border-radius: 4px;
        background-color: #fff;
        color: #111;
        font-size: 12px;
        font-weight: 600;
      }
      @keyframes conductFinalMtdProgress {
        0% { transform: translateX(-110%); }
        100% { transform: translateX(260%); }
      }
    ")),
    tags$script(HTML("
      (function() {
        var runButtonIds = {
          runPrior: true,
          runSim: true,
          runScenario: true,
          evaluateConduct: true,
          runConductFinalMtd: true
        };
        var clickedRunButtons = {};
        var shinyHandlersRegistered = false;

        // Show the simulation status box and a moving bar as soon as Run Simulations is clicked,
        // before the server has checked the inputs and started the background run.
        var simStatusBefore = null;
        function showSimulationStarting() {
          var box = document.getElementById('trial_run_status_ui');
          if (!box) return;
          if (!document.getElementById('trial-run-starting')) simStatusBefore = box.innerHTML;
          var wrap = document.createElement('div');
          wrap.id = 'trial-run-starting';
          var msg = document.createElement('div');
          msg.style.cssText = 'border: 1px solid #7db7ff; background-color: #eef7ff; color: #1f4f82; border-radius: 8px; padding: 10px 12px; font-weight: 500;';
          msg.textContent = 'Starting trial simulations: checking the inputs and starting the background run...';
          var outer = document.createElement('div');
          outer.className = 'progress';
          outer.style.cssText = 'height: 20px; border-radius: 6px; margin-top: 6px;';
          var bar = document.createElement('div');
          bar.className = 'progress-bar progress-bar-striped active';
          bar.style.cssText = 'width: 100%; background-color: #4a90d9; opacity: 0.45;';
          outer.appendChild(bar);
          wrap.appendChild(msg);
          wrap.appendChild(outer);
          box.innerHTML = '';
          box.appendChild(wrap);
        }

        function markRunButtonClicked(button) {
          if (!button || !button.id || !runButtonIds[button.id] || button.disabled) return;
          clickedRunButtons[button.id] = true;
          button.classList.add('run-button-clicked');
          if (button.id === 'runSim') showSimulationStarting();
        }

        function clearRunButtonClicked(id) {
          if (!id) return;
          delete clickedRunButtons[id];
          var button = document.getElementById(id);
          if (button) button.classList.remove('run-button-clicked');
          if (id === 'runSim') {
            // The run did not start (for example an input error): put back the previous status.
            var starting = document.getElementById('trial-run-starting');
            if (starting && simStatusBefore !== null) starting.parentNode.innerHTML = simStatusBefore;
            simStatusBefore = null;
          }
        }

        function restoreRunButtonClickedStates() {
          Object.keys(clickedRunButtons).forEach(function(id) {
            var button = document.getElementById(id);
            if (button) button.classList.add('run-button-clicked');
          });
        }

        function updateDetailsToggleLabels(root) {
          (root || document).querySelectorAll('details').forEach(function(detailsEl) {
            var label = detailsEl.querySelector('.details-toggle-label');
            if (!label) return;
            label.innerHTML = detailsEl.open ? 'Hide Details &#9652;' : 'Show Details &#9662;';
          });
        }

        function showFinalMtdRunningStatus() {
          var status = document.getElementById('conduct_final_mtd_status_ui');
          if (!status) return;
          status.innerHTML = [
            '<div class=\"conduct-final-mtd-running\">',
            '<strong>End-of-trial report status:</strong> Starting final posterior model...',
            '<div class=\"conduct-final-mtd-progress\"><div></div></div>',
            '</div>'
          ].join('');
        }

        function conductValue(row, selector) {
          var el = row.querySelector(selector);
          return el ? el.value : '';
        }

        function isConductDoseInput(el) {
          return el && el.id && /^(conduct_rec_dose|conduct_admin_dose)_\\d+$/.test(el.id);
        }

        function showConductRunningStatus(targetId, label) {
          var status = document.getElementById(targetId);
          if (!status) return;
          status.innerHTML = [
            '<div class=\"conduct-final-mtd-running\">',
            '<strong>' + label + '</strong> Starting...',
            '<div class=\"conduct-final-mtd-progress\"><div></div></div>',
            '</div>'
          ].join('');
        }

        function cleanWholeNumberInput(el) {
          if (!isConductDoseInput(el)) return;
          var cleaned = String(el.value || '').replace(/[^0-9]/g, '');
          if (cleaned !== el.value) el.value = cleaned;
        }

        function captureConductRows() {
          var table = document.getElementById('conduct_patient_table');
          if (!table || !window.Shiny) return [];
          var rows = [];
          table.querySelectorAll('tbody tr').forEach(function(row) {
            rows.push({
              Cohort: conductValue(row, '[id^=\"conduct_cohort_\"]'),
              Patient: conductValue(row, '[id^=\"conduct_patient_\"]'),
              Recommended_Dose: conductValue(row, '[id^=\"conduct_rec_dose_\"]'),
              Administered_Dose: conductValue(row, '[id^=\"conduct_admin_dose_\"]'),
              DLT_Status: conductValue(row, '[id^=\"conduct_dlt_\"]'),
              Alpha_Used: conductValue(row, '[id^=\"conduct_alpha_\"]')
            });
          });
          Shiny.setInputValue('conduct_row_count_client', rows.length, {priority: 'event'});
          Shiny.setInputValue('conduct_manual_snapshot', JSON.stringify(rows), {priority: 'event'});
          return rows;
        }

        function setConductRowIndex(row, index) {
          row.querySelectorAll('.shiny-bound-input').forEach(function(el) {
            el.classList.remove('shiny-bound-input');
          });
          row.querySelectorAll('[id]').forEach(function(el) {
            el.id = el.id.replace(/(conduct_(?:cohort|patient|rec_dose|admin_dose|dlt|alpha)_)\\d+$/, '$1' + index);
          });
        }

        function clearConductRow(row) {
          row.querySelectorAll('input, select').forEach(function(el) {
            el.value = '';
          });
        }

        document.addEventListener('wheel', function(event) {
          if (
            event.target &&
            event.target.matches &&
            event.target.matches('input[type=\"number\"]')
          ) {
            event.preventDefault();
          }
        }, { passive: false });

        function addConductRow() {
          var table = document.getElementById('conduct_patient_table');
          if (!table) return;
          var tbody = table.querySelector('tbody');
          if (!tbody) return;
          var currentRows = tbody.querySelectorAll('tr');
          if (currentRows.length >= 200) return;
          var newRow = currentRows[currentRows.length - 1].cloneNode(true);
          clearConductRow(newRow);
          setConductRowIndex(newRow, currentRows.length + 1);
          tbody.appendChild(newRow);
          if (window.Shiny && Shiny.bindAll) Shiny.bindAll(newRow);
          captureConductRows();
        }

        function deleteConductRow() {
          var table = document.getElementById('conduct_patient_table');
          if (!table) return;
          var rows = table.querySelectorAll('tbody tr');
          if (rows.length <= 1) return;
          var lastRow = rows[rows.length - 1];
          if (window.Shiny && Shiny.unbindAll) Shiny.unbindAll(lastRow);
          lastRow.remove();
          captureConductRows();
        }

        function registerShinyHandlers() {
          if (shinyHandlersRegistered || !window.Shiny || !Shiny.addCustomMessageHandler) return;
          shinyHandlersRegistered = true;
          Shiny.addCustomMessageHandler('clearConductUploadInput', function(message) {
            var id = message && message.id ? message.id : 'conduct_upload';
            var input = document.getElementById(id);
            if (input) input.value = '';
            var container = input && input.closest ? input.closest('.form-group') : document.getElementById('conduct_upload_ui');
            if (container) {
              container.querySelectorAll('input[type=\"text\"]').forEach(function(el) {
                el.value = '';
              });
              container.querySelectorAll('.progress-bar').forEach(function(el) {
                el.style.width = '0%';
                el.textContent = '';
              });
            }
          });
          Shiny.addCustomMessageHandler('clearRunButtonClicked', function(message) {
            clearRunButtonClicked(message && message.id);
          });
          Shiny.addCustomMessageHandler('clearConductManualSnapshot', function(message) {
            var table = document.getElementById('conduct_patient_table');
            if (table) {
              table.querySelectorAll('tbody tr').forEach(function(row, index) {
                if (index > 0) {
                  if (window.Shiny && Shiny.unbindAll) Shiny.unbindAll(row);
                  row.remove();
                  return;
                }
                row.querySelectorAll('input, select').forEach(function(el) {
                  el.value = '';
                  if (el.tagName === 'SELECT') el.selectedIndex = 0;
                });
              });
            }
            if (window.Shiny) {
              Shiny.setInputValue('conduct_row_count_client', 1, {priority: 'event'});
              Shiny.setInputValue('conduct_manual_snapshot', '', {priority: 'event'});
            }
          });
        }

        document.addEventListener('click', function(event) {
          var target = event.target;
          if (!target) return;
          var runButton = target.closest ? target.closest('button') : null;
          markRunButtonClicked(runButton);
        }, true);

        document.addEventListener('click', function(event) {
          var target = event.target;
          if (!target) return;
          var button = target.closest ? target.closest('#conduct_add_row_btn, #conduct_delete_row_btn, #evaluateConduct, #runConductFinalMtd') : null;
          if (!button || !window.Shiny) return;
          if (button.id === 'evaluateConduct' || button.id === 'runConductFinalMtd') {
            captureConductRows();
            if (button.id === 'runConductFinalMtd') showFinalMtdRunningStatus();
            return;
          }
          event.preventDefault();
          event.stopImmediatePropagation();
          if (button.id === 'conduct_add_row_btn') {
            addConductRow();
          } else if (button.id === 'conduct_delete_row_btn') {
            deleteConductRow();
          }
        }, true);

        document.addEventListener('toggle', function(event) {
          if (event.target && event.target.tagName === 'DETAILS') {
            updateDetailsToggleLabels(event.target);
          }
        }, true);

        document.addEventListener('input', function(event) {
          if (!event.target || !event.target.closest) return;
          if (event.target.closest('#conduct_patient_table')) {
            cleanWholeNumberInput(event.target);
            captureConductRows();
          }
        }, true);

        document.addEventListener('change', function(event) {
          if (!event.target || !event.target.closest) return;
          if (event.target.closest('#conduct_patient_table')) {
            cleanWholeNumberInput(event.target);
            captureConductRows();
          }
        }, true);

        registerShinyHandlers();
        updateDetailsToggleLabels(document);
        document.addEventListener('shiny:connected', registerShinyHandlers);
        if (window.MutationObserver) {
          var observer = new MutationObserver(function(mutations) {
            restoreRunButtonClickedStates();
            mutations.forEach(function(mutation) {
              mutation.addedNodes.forEach(function(node) {
                if (node.nodeType === 1) updateDetailsToggleLabels(node);
              });
            });
          });
          observer.observe(document.body, { childList: true, subtree: true });
        }
      })();
    "))
  ),
  
  titlePanel("MEWOC for Single-Agent Phase I Trial"),
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Trial Setup",
             fluidRow(
               column(5,
                      wellPanel(
                        h4("Trial Setup"),
                        tags$hr(style = "border-top: 2px solid black;"),
                        tags$div(
                          class = "guide-setup-bar",
                          radioButtons(
                            "prior_dose_type",
                            "Dose Type",
                            choices = c("Discrete" = "Discrete", "Continuous" = "Continuous"),
                            selected = "Discrete",
                            inline = TRUE
                          ),
                          guide_help_icon("treatment_and_dose_type")
                        ),
                        tags$div(
                          class = "guide-setup-bar",
                          tags$div(
                            class = "guide-setup-field-group",
                            fluidRow(
                              column(4, numericInput("theta", HTML("Target Toxicity (&theta;)"), value = 0.33, min = 0, max = 1)),
                              column(4, numericInput("NN", "Total Number of Patients", value = 30, min = 1), uiOutput("NN_even_warning_ui")),
                              column(4, numericInput("cohort", "Cohort Size", value = 1, step = 1))
                            )
                          ),
                          guide_help_icon("trial_setup_core_inputs")
                        ),
                        tags$div(
                          class = "guide-setup-bar guide-setup-dose-bar",
                          tags$div(
                            class = "guide-setup-dose-content",
                            conditionalPanel(
                              condition = "input.prior_dose_type == 'Continuous'",
                              textInput("dose_unit", "Dose Unit", value = "mg")
                            ),
                            
                            conditionalPanel(
                              condition = "input.prior_dose_type == 'Discrete'",
                              fluidRow(
                                column(6, numericInput("num_doses", "Number of Dose Levels", value = 4, min = 2, max = 12)),
                                column(6, numericInput("startdose", "Starting Dose Level", value = 1, min = 1))
                              ),
                              uiOutput("categorical_actual_dose_values_ui")
                            ),
                            
                            conditionalPanel(
                              condition = "input.prior_dose_type == 'Continuous'",
                              fluidRow(
                                column(6, numericInput("min_dose", "Minimum Dose Value", value = 10, min = 0)),
                                column(6, numericInput("max_dose", "Maximum Dose Value", value = 30, min = 0))
                              )
                            )
                          ),
                          guide_help_icon("dose_and_level_inputs")
                        )
                      )
               ),
               column(7,
                      wellPanel(
                        style = "background-color: #f8fff9; border: 2px solid #28a745; box-shadow: 0 0 0 3px rgba(40,167,69,0.12);",
                        h4("Setup Notes"),
                        tags$hr(style = "border-top: 2px solid #28a745;"),
                        p("These values are shared by the prior calculation, trial simulation, protocol draft, trial conduct workbook, and decision-tree setup."),
                        p("Categorical dose values are collected for reporting and document preparation."),
                        p(tags$strong("If you only intend to use the Trial Conduct tab, you do not need to complete this page or run the trial design steps."))
                      )
               )
             ),
             fluidRow(
               column(
                 12,
                 div(
                   style = "margin-top: 10px;",
                   actionButton(
                     "trial_setup_submit",
                     "Submit",
                     style = "color: white; background-color: #28a745; border-color: #1e7e34; font-weight: bold;"
                   )
                 )
               )
             )
    ),
    
    tabPanel("Trial Design",
             tabsetPanel(
               id = "trial_design_tabs",
               # ---------------- Tab 1: Prior ---------------- #
               tabPanel("Prior Information",
                        # SK: "refresh memory" -- this whole tab is gated on
                        # setup_is_current() (see trial_setup_signature() /
                        # output$trial_setup_current in the server). Editing
                        # Trial Setup without resubmitting flips this to the
                        # lock message below instead of showing/using a
                        # Prior Information page that no longer matches what
                        # Trial Setup currently says.
                        conditionalPanel(
                          condition = "output.trial_setup_current == true",
                          fluidRow(
                            # Left panel: Trial Info
                            column(5,
                                   wellPanel(
                                     guide_panel_header("Design-Specific Prior Information", "prior_inputs"),
                                     tags$hr(style = "border-top: 2px solid black;"),
                                     conditionalPanel(
                                       condition = "true",
                                       fluidRow(
                                         column(6, numericInput("p0", guide_input_label("Probability of (MTD < Min Dose)", "p0"), value = 0.1, min = 0, max = 1)),
                                         column(6, numericInput("p1", guide_input_label("Probability of (MTD > Max Dose)", "p1"), value = 0.2, min = 0, max = 1))
                                       )
                                     ),
                                     
                                     numericInput("condition", guide_input_label("Condition (a + b)", "condition"), value = 2, min = 1, max = 2),
                                     numericInput("seed", guide_input_label("Seed value", "seed"), value = 988, min = 1),
                                     br(),
                                     actionButton("runPrior", "Run Prior Calculations",
                                                  style = "color: white; 
                                            background-color: #28a745; 
                                            border-color: #28a745; 
                                            font-weight: bold;"),
                                   )
                            ),
                            
                            column(7,
                                   wellPanel(
                                     style = "background-color: #f8fff9; border: 2px solid #28a745; box-shadow: 0 0 0 3px rgba(40,167,69,0.12);",
                                     h4("Prior Hyperparameter Estimation"),
                                     tags$hr(style = "border-top: 2px solid #28a745;"),
                                     
                                     # MT: Download buttons (conditionally shown when results available)
                                     uiOutput("prior_download_buttons_ui"),
                                     br(),
                                     
                                     fluidRow(
                                       tags$head(
                                         tags$style(HTML("#PriorResults { font-size: 15px; }"))
                                       ),
                                       verbatimTextOutput('PriorResults')
                                     ),
                                     br(),
                                     uiOutput("pdlt_plot_ui")
                                   )
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "output.trial_setup_current != true",
                          wellPanel(
                            style = "background-color: #fff3f3; border: 2px solid #dc3545;",
                            h4("Trial Setup not submitted"),
                            p("Prior Information uses the Trial Setup that was in place the last time you clicked Submit. Please complete Trial Setup and click Submit before using this page.")
                          )
                        )
               ),#end of prior information tabPanel
               
               # ---------------- Tab 2: Trial Tab ---------------- #
               tabPanel("Trial Simulation",
                        # SK: same gating as Prior Information above.
                        conditionalPanel(
                          condition = "output.trial_setup_current == true",
                          fluidRow(
                            # Left panel: Trial Info
                            column(5,
                                   wellPanel(
                                     guide_panel_header("Required Trial Information", "trial_simulation_inputs"),
                                     tags$hr(style = "border-top: 2px solid black;"),
                                     
                                     fluidRow(
                                       column(6, numericInput("M", guide_input_label("Total Number of Trials", "trial_simulation_scenarios"), value = 10, min = 1))
                                     ),
                                     uiOutput("trial_scenario_label_ui"),
                                     uiOutput("dose_inputs"),
                                     helpText("More scenarios will run sequentially and will increase computation time."),
                                     div(
                                       style = "display: flex; gap: 10px; align-items: center; flex-wrap: wrap; margin-bottom: 12px;",
                                       actionButton("add_trial_scenario", "Add Scenario"),
                                       actionButton("remove_trial_scenario", "Delete Last Scenario")
                                     ),
                                     conditionalPanel(
                                       condition = "input.prior_dose_type == 'Continuous'",
                                       conditionalPanel(
                                         condition = "true",
                                         fluidRow(
                                           # Previous real-dose increment input:
                                           # column(6, numericInput("max_dose_increment", "Maximum Dose Increment", value = 5, min = 0))
                                           column(6, numericInput("max_dose_increment", guide_input_label("Maximum Dose Increment (%)", "max_dose_increment"), value = 10, min = 0, max = 100))
                                         )
                                       )
                                     ),
                                     conditionalPanel(
                                       condition = "true",
                                       fluidRow(
                                         column(6, numericInput("deltastop", guide_input_label("Threshold, \u0394 (dose 1 too toxic)", "deltastop"), value = 0.85))
                                       )
                                     ),
                                     
                                     conditionalPanel(
                                       condition = "input.prior_dose_type == 'Discrete'",
                                       radioButtons(
                                         "trial_mode",
                                         guide_input_label("Accelerated EWOC?", "trial_mode"),
                                         choices = c("Yes", "No"),
                                         selected = "No",
                                         inline = TRUE
                                       )
                                     ),
                                     conditionalPanel(
                                       condition = "input.trial_mode == 'Yes' && input.prior_dose_type == 'Discrete'",
                                       uiOutput("accelerated_cap_ui")
                                     ),
                                     conditionalPanel(
                                       condition = "input.prior_dose_type == 'Discrete'",
                                       helpText("Dose is escalated for each patient under the accelerated phase until the selected capped dose level is reached and no DLT is observed. After that, the trial continues using the regular EWOC dose-escalation rule.")
                                     ),
                                     div(
                                       style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin-top: 10px;",
                                       tags$strong(style = "font-size: 13px;", "Add-on computation settings"),
                                       actionButton(
                                         "trial_addon_toggle",
                                         "Show Details",
                                         style = "padding: 1px 8px; font-size: 11px; height: 24px; line-height: 1.6;"
                                       )
                                     ),
                                     conditionalPanel(
                                       condition = "input.trial_addon_toggle % 2 == 1",
                                       div(
                                         id = "trial_addon_panel",
                                         style = "padding-top: 10px;",
                                         fluidRow(
                                           column(6, numericInput("burn", "Burn-in", value = 1000, min = 0)),
                                           column(6, numericInput("mm", "MCMC Samples", value = 1500, min = 1))
                                         )
                                       )
                                     )
                                   ),
                                   wellPanel(
                                     tags$div(
                                       class = "guide-panel-header",
                                       h4("Feasibility Information"),
                                       guide_help_icon("feasibility_rule")
                                     ),
                                     tags$hr(style = "border-top: 2px solid black;"),
                                     fluidRow(
                                       column(6, numericInput("lower_f_bound", HTML("Lower Feasibility Bound (&alpha;)"), value = 0.25, min = 0, max = 1)),
                                       column(6, numericInput("increment", "Feasibility Bound Increment", value = 0.05, min = 0, max = 1))
                                     ),
                                     div(
                                       style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin-top: 10px;",
                                       tags$strong(style = "font-size: 13px;", "Toxicity dependent overdose control"),
                                       actionButton(
                                         "trial_toxicity_toggle",
                                         "Show Details",
                                         style = "padding: 1px 8px; font-size: 11px; height: 24px; line-height: 1.6;"
                                       )
                                     ),
                                     conditionalPanel(
                                       condition = "input.trial_toxicity_toggle % 2 == 1",
                                       div(
                                         id = "trial_toxicity_panel",
                                         style = "padding-top: 10px;",
                                         radioButtons(
                                           "alpha_increment_rule",
                                           guide_input_label(HTML("Add toxicity-dependent feasibility bound other than &theta;?"), "alpha_increment_rule"),
                                           choices = c("No" = "Theta", "Yes" = "Custom"),
                                           selected = "Theta",
                                           inline = TRUE
                                         ),
                                         helpText(HTML("&alpha; increases only when the observed toxicity rate is below the selected toxicity bound. By default, this bound is &theta;. A custom toxicity bound must be greater than 0 and lower than &theta;.")),
                                         conditionalPanel(
                                           condition = "input.alpha_increment_rule == 'Custom'",
                                           numericInput(
                                             "alpha_increment_cutoff",
                                             "Custom toxicity bound",
                                             value = 0.25,
                                             min = 0,
                                             max = 1,
                                             step = 0.01
                                           )
                                         )
                                       )
                                     )
                                   ),
                                   wellPanel(
                                     tags$div(
                                       class = "guide-panel-header",
                                       h4("Trial Efficiency Stopping Rule"),
                                       guide_help_icon("stopping_rule")
                                     ),
                                     tags$hr(style = "border-top: 2px solid black;"),
                                     radioButtons("stopping_rule", "Apply trial efficiency stopping rule?", choices = c("Yes", "No"), selected = "No", inline = TRUE),
                                     conditionalPanel(
                                       condition = "input.stopping_rule == 'Yes'",
                                       numericInput("num_patients_restrict", "Maximum number of patients at the recommended dose", value = NA, step = 1)
                                     ),
                                     conditionalPanel(
                                       condition = "input.stopping_rule == 'Yes' && input.prior_dose_type == 'Continuous'",
                                       numericInput("efficiency_stop_tolerance", "Dose-region width (% of the dose range)", value = NA, step = 1)
                                     ),
                                     div(
                                       style = "display: flex; gap: 10px; align-items: center; flex-wrap: wrap;",
                                       actionButton("runSim", "Run Simulations",
                                                    style = "color: white; 
                                                   background-color: #28a745; 
                                                   border-color: #28a745; 
                                                   font-weight: bold;"),
                                       actionButton("stopSim", "Stop Simulation",
                                                    style = "color: white;
                                                  background-color: #dc3545;
                                                  border-color: #dc3545;
                                                  font-weight: bold;")
                                     ),
                                     br(),
                                     uiOutput("trial_run_status_ui")
                                   )
                            ),
                            
                            # Right panel: Results workspace
                            column(7,
                                   wellPanel(
                                     style = "background-color: #f8fff9; border: 2px solid #28a745; box-shadow: 0 0 0 3px rgba(40,167,69,0.12);",
                                     h3("Summary Statistics"),
                                     tags$hr(style = "border-top: 2px solid #28a745;"),
                                     br(),
                                     uiOutput("trial_download_buttons_ui"),
                                     uiOutput("scenario_results_tabs"),
                                     uiOutput("eff_table1_block"),
                                     uiOutput("dlt_table2_block"),
                                     h4(" "),
                                     uiOutput("stopping_summary_block"),
                                     h4(" "),
                                     uiOutput("stopped_details_block")
                                   )
                            ),
                            
                          )# close row with 3 panels
                        ),
                        conditionalPanel(
                          condition = "output.trial_setup_current != true",
                          wellPanel(
                            style = "background-color: #fff3f3; border: 2px solid #dc3545;",
                            h4("Trial Setup not submitted"),
                            p("Trial Simulation uses the Trial Setup that was in place the last time you clicked Submit. Please complete Trial Setup and click Submit before using this page.")
                          )
                        )
               )  #close simulation tab
             )
    ),  #close trial design tab
    
    # ---------------- Tab 3: Decision Tree ---------------- #
    
    
    # ---------------- Tab 3: Trial Documents ---------------- #
    tabPanel("Trial Documents",
             fluidRow(
               # Left: dose escalation tree, inputs and output together.
               column(8,
                      wellPanel(
                        guide_panel_header("Dose Escalation Tree", "trial_documents_inputs"),
                        tags$hr(style = "border-top: 2px solid black;"),
                        div(
                          style = "display: flex; align-items: flex-end; gap: 16px; flex-wrap: wrap;",
                          div(
                            style = "width: 220px;",
                            numericInput("tree_steps", guide_input_label("Decision Steps to Expand", "tree_steps"), value = 4, min = 2, max = 5, step = 1)
                          ),
                          div(
                            style = "margin-bottom: 15px;",
                            actionButton("runScenario", "Build Escalation Tree",
                                         style = "color: white;
                                                    background-color: #28a745;
                                                    border-color: #28a745;
                                                    font-weight: bold;")
                          )
                        ),
                        helpText(HTML("<small><i>Cohort sizes 2 and 3 branch faster than cohort size 1. Recommended setting: 3 to 4 steps.</i></small>")),
                        tags$hr(style = "border-top: 1px solid #ddd;"),
                        uiOutput("dose_esc_plot_ui"),
                        uiOutput("tree_download_button_ui")
                      )
               ),
               # Right: three coloured segments -- trial name with readiness, Phase 1 design, Trial Conduct workbook.
               column(4,
                      tags$div(
                        style = "background-color: #eef5fc; border: 1px solid #b9d4f1; border-left: 6px solid #2a78d6; border-radius: 6px; padding: 14px 16px; margin-bottom: 12px;",
                        tags$div(style = "font-size: 20px; font-weight: 700; color: #1a4f91; margin-bottom: 8px;", "Name of the Trial ", guide_help_icon("report_title")),
                        textInput("report_title", NULL, value = "MEWOC Phase 1 Design Report", width = "100%"),
                        tags$div(style = "font-weight: 600; margin: 2px 0 6px 0;", "Readiness"),
                        uiOutput("report_status_ui")
                      ),
                      tags$div(
                        style = "background-color: #eefaf2; border: 1px solid #bfe3cb; border-left: 6px solid #28a745; border-radius: 6px; padding: 14px 16px; margin-bottom: 12px;",
                        tags$div(style = "font-size: 20px; font-weight: 700; color: #1e7e34; margin-bottom: 4px;", "Phase 1 Design ", guide_help_icon("phase1_design_download")),
                        p(style = "font-size: 12px; color: #555; margin-bottom: 10px;",
                          "Full Phase 1 design populated with the current inputs, simulation results, and the generated tree."),
                        uiOutput("phase1_design_download_buttons_ui")
                      ),
                      tags$div(
                        style = "background-color: #fdf3ec; border: 1px solid #f3cdb3; border-left: 6px solid #eb6834; border-radius: 6px; padding: 14px 16px; margin-bottom: 12px;",
                        tags$div(style = "font-size: 20px; font-weight: 700; color: #b2491f; margin-bottom: 4px;", "Trial Conduct Workbook ", guide_help_icon("conduct_workbook_download")),
                        p(style = "font-size: 12px; color: #555; margin-bottom: 10px;",
                          "Excel workbook populated from Prior Information and Trial Simulation, with a patient Data Input worksheet."),
                        uiOutput("trial_conduct_workbook_download_ui")
                      )
               )
             )
    ),
    
    # ---------------- Tab 6: Trial Conduct ---------------- #
    tabPanel("Trial Conduct",
             conditionalPanel(
               condition = "true",
               fluidRow(
                 column(5,
                        wellPanel(
                          style = "background-color: #f7fbff; border: 1px solid #9fb7c9; padding: 10px 14px;",
                          radioButtons(
                            "conduct_design_entry_mode",
                            guide_input_label("Trial Conduct Input Method", "conduct_design_entry_mode"),
                            choices = c("Upload workbook" = "upload", "Manual entry" = "manual"),
                            selected = "upload",
                            inline = TRUE
                          ),
                          conditionalPanel(
                            condition = "input.conduct_design_entry_mode == 'upload'",
                            div(
                              id = "conduct_upload_ui",
                              fileInput(
                                "conduct_workbook_upload",
                                "Upload Trial Conduct workbook",
                                accept = c(".xlsx", ".xlsm"),
                                width = "100%",
                                placeholder = "Upload the workbook from Trial Documents"
                              )
                            ),
                            uiOutput("conduct_design_upload_status_ui")
                          )
                        ),
                        # Design Information panel (collapsible)
                        wellPanel(
                          style = "background-color: #f9f9f9; border: 1px solid #ddd; padding: 10px 14px;",
                          
                          # Always-visible header row
                          div(
                            style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                            tags$strong(style = "font-size: 13px;", "Trial Design Information"),
                            tags$span(style = "color: #999; font-size: 11px; font-style: italic;",
                                      "Enter values from the simulation results"),
                            actionButton(
                              "conduct_design_toggle",
                              "Show Details",
                              style = "padding: 1px 8px; font-size: 11px; height: 24px; line-height: 1.6;"
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.conduct_design_entry_mode == 'upload' && output.conduct_design_open == 'open'",
                            div(
                              id = "conduct_design_upload_panel",
                              style = "margin-top: 10px;",
                              uiOutput("conduct_design_upload_summary_ui")
                            )
                          ),
                          
                          # Collapsible detail panel
                          conditionalPanel(
                            condition = "input.conduct_design_entry_mode == 'manual' && output.conduct_design_open == 'open'",
                            div(
                              id = "conduct_design_manual_panel",
                              style = "margin-top: 10px;",
                              tags$hr(style = "border-top: 1px solid #ddd; margin: 8px 0 10px 0;"),
                              
                              # Accelerated EWOC
                              conditionalPanel(
                                condition = "input.conduct_dose_type != 'Continuous'",
                                div(
                                  style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px; flex-wrap: wrap;",
                                  tags$span(style = "font-size: 12px; font-weight: 600; color: #444;",
                                            "Accelerated EWOC?"),
                                  radioButtons("conduct_trial_mode", NULL,
                                               choices = c("No", "Yes"), selected = "No", inline = TRUE),
                                  uiOutput("conduct_design_accel_cap_ui")
                                )
                              ),
                              
                              tags$hr(style = "border-top: 1px solid #eee; margin: 4px 0 10px 0;"),
                              
                              # Three-column layout
                              fluidRow(
                                column(4,
                                       tags$p(class = "design-section-label", "Prior"),
                                       numericInput("conduct_p0", HTML("P(MTD < Min)"), value = NA, min = 0, max = 1, step = 0.01),
                                       numericInput("conduct_p1", HTML("P(MTD > Max)"), value = NA, min = 0, max = 1, step = 0.01),
                                       div(
                                         class = "conduct-beta-formula",
                                         tags$span(HTML("&rho;<sub>1</sub> &sim; beta(")),
                                         numericInput("conduct_a0", NULL, value = NA, min = 0, step = 0.01, width = "58px"),
                                         tags$span(","),
                                         numericInput("conduct_b0", NULL, value = NA, min = 0, step = 0.01, width = "58px"),
                                         tags$span(") truncated to [0.001, 0.999]")
                                       ),
                                       div(
                                         class = "conduct-beta-formula",
                                         tags$span(HTML("&rho;<sub>0</sub>/&rho;<sub>1</sub> &sim; beta(")),
                                         numericInput("conduct_a1", NULL, value = NA, min = 0, step = 0.01, width = "58px"),
                                         tags$span(","),
                                         numericInput("conduct_b1", NULL, value = NA, min = 0, step = 0.01, width = "58px"),
                                         tags$span(")")
                                       )
                                ),
                                column(4,
                                       tags$p(class = "design-section-label", "Design"),
                                       radioButtons(
                                         "conduct_dose_type",
                                         "Dose Type",
                                         choices = c("Categorical" = "Discrete", "Continuous" = "Continuous"),
                                         selected = "Discrete",
                                         inline = TRUE
                                       ),
                                       textInput("conduct_dose_unit", "Dose Unit", value = ""),
                                       conditionalPanel(
                                         condition = "input.conduct_dose_type == 'Discrete'",
                                         numericInput("conduct_num_doses", "Dose Levels", value = NA, min = 2, max = 12, step = 1),
                                         numericInput("conduct_startdose", "Start Dose", value = NA, min = 1, step = 1),
                                         textInput("conduct_actual_dose_values", "Actual Dose Values", value = ""),
                                         # Optional per-level unit or description (e.g. "mg; mg; g; mg + drug X"); blank = Dose Unit for every level.
                                         textInput("conduct_actual_dose_units", "Dose Level Units / Descriptions (separate with ;)", value = "")
                                       ),
                                       conditionalPanel(
                                         condition = "input.conduct_dose_type == 'Continuous'",
                                         numericInput("conduct_min_dose", "Minimum Dose", value = NA, min = 0, step = 0.01),
                                         numericInput("conduct_max_dose", "Maximum Dose", value = NA, min = 0, step = 0.01),
                                         numericInput("conduct_max_dose_increment", "Maximum Dose Increment (%)", value = 100, min = 0, max = 100, step = 1)
                                       ),
                                       numericInput("conduct_theta", HTML("&theta;"), value = NA, min = 0, max = 1, step = 0.01),
                                       numericInput("conduct_deltastop", guide_input_label(HTML("Stopping threshold, &Delta;"), "conduct_deltastop"), value = NA, min = 0, max = 1, step = 0.01),
                                       numericInput("conduct_lower_f_bound", HTML("&alpha;"), value = NA, min = 0, max = 1, step = 0.01),
                                       numericInput("conduct_increment", "Increment", value = NA, min = 0, max = 1, step = 0.01)
                                ),
                                column(4,
                                       tags$p(class = "design-section-label", "Add-on"),
                                       numericInput("conduct_seed", "Seed", value = NA, min = 1),
                                       numericInput("conduct_burn", "Burn-in", value = NA, min = 0),
                                       numericInput("conduct_mm", "MCMC Samples", value = NA, min = 1),
                                       tags$p(class = "design-section-label", "Trial Size and Stopping"),
                                       numericInput("conduct_cohort_size", "Cohort Size (minimum patients per cohort)", value = NA, min = 1, step = 1),
                                       numericInput("conduct_total_n", "Maximum Sample Size (N)", value = NA, min = 1, step = 1),
                                       radioButtons("conduct_stopping_rule", "Efficiency Stopping Rule?", choices = c("No", "Yes"), selected = "No", inline = TRUE),
                                       conditionalPanel(
                                         condition = "input.conduct_stopping_rule == 'Yes'",
                                         numericInput("conduct_num_patients_restrict", "Maximum Patients at Recommended Dose", value = NA, min = 1, step = 1),
                                         conditionalPanel(
                                           condition = "input.conduct_dose_type == 'Continuous'",
                                           numericInput("conduct_efficiency_width", "Dose-Region Width (% of dose range)", value = NA, step = 0.5)
                                         )
                                       )
                                )
                              )
                            )
                          )
                        ),
                        
                        wellPanel(
                          guide_panel_header("Current Trial Data", "conduct_data_inputs"),
                          tags$hr(style = "border-top: 2px solid black;"),
                          uiOutput("conduct_patient_rows"),
                          div(
                            style = "margin-top: 10px; display: flex; align-items: center; gap: 6px;",
                            checkboxInput(
                              "conduct_verify_addon",
                              "I have reviewed the trial design information and current trial data",
                              value = FALSE
                            ),
                            uiOutput("conduct_addon_verify_ui")
                          ),
                          uiOutput("evaluate_conduct_button_ui"),
                          uiOutput("conduct_trial_completed_ui"),
                          uiOutput("conduct_final_mtd_run_ui")
                        )
                 ),
                 column(7,
                        wellPanel(
                          style = "background-color: #f8fff9; border: 2px solid #28a745; box-shadow: 0 0 0 3px rgba(40,167,69,0.12);",
                          h3("Posterior Assessment and Recommendation"),
                          tags$hr(style = "border-top: 2px solid #28a745;"),
                          uiOutput("conduct_status_ui"),
                          br(),
                          uiOutput("conduct_recommendation_ui"),
                          br(),
                          uiOutput("conduct_final_mtd_status_ui"),
                          uiOutput("conduct_final_mtd_summary_ui"),
                          br(),
                          uiOutput("conduct_download_table_ui")
                        )
                 )
               )
             )
    ),
    
    tabPanel("Help", help_page_ui())

  )
)



server <- function(input, output, session) {
  
  # "?" help icons open a pop-up with the built-in explanation.
  observeEvent(input$guide_help_topic, {
    showModal(guide_input_help_modal(input$guide_help_topic))
  })

  observeEvent(input$guide_help_group, {
    showModal(guide_group_help_modal(input$guide_help_group))
  })
  
  opt1_val <- reactiveVal(NULL)# to call it in other plot parts of the code
  conduct_opt1_val <- reactive({
    data.frame(
      a0 = suppressWarnings(as.numeric(input$conduct_a0)),
      b0 = suppressWarnings(as.numeric(input$conduct_b0)),
      a1 = suppressWarnings(as.numeric(input$conduct_a1)),
      b1 = suppressWarnings(as.numeric(input$conduct_b1))
    )
  })
  
  categorical_actual_dose_values <- reactive({
    num_doses <- suppressWarnings(as.integer(input$num_doses))
    if (length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 2L) {
      return(numeric(0))
    }
    vapply(seq_len(num_doses), function(i) {
      suppressWarnings(as.numeric(input[[paste0("actual_dose_", i)]]))
    }, numeric(1))
  })
  # SK: each discrete dose level now carries its own unit (e.g. dose level 1
  # may be dosed in mg while dose level 2 is dosed in mg/m^2) instead of one
  # shared "Dose Unit" field for the whole drug -- see actual_dose_unit_<i>
  # inputs added alongside each Dose Level value in categorical_actual_dose_values_ui.
  categorical_actual_dose_units <- reactive({
    num_doses <- suppressWarnings(as.integer(input$num_doses))
    if (length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 2L) {
      return(character(0))
    }
    vapply(seq_len(num_doses), function(i) {
      unit_value <- trimws(as.character(input[[paste0("actual_dose_unit_", i)]]))
      if (length(unit_value) != 1L || is.na(unit_value)) "" else unit_value
    }, character(1))
  })
  categorical_actual_dose_labels <- reactive({
    num_doses <- suppressWarnings(as.integer(input$num_doses))
    if (length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 2L) {
      return(character(0))
    }
    build_discrete_dose_labels(
      categorical_actual_dose_values(),
      categorical_actual_dose_units(),
      num_doses
    )
  })
  combined_table_val<-reactiveVal(NULL)
  stopped_details_val <- reactiveVal(data.frame())
  scenario_data_val <- reactiveVal(NULL)
  prior_report_val <- reactiveVal(NULL)
  trial_report_val <- reactiveVal(NULL)
  trial_scenario_count <- reactiveVal(1L)
  trial_scenario_results_val <- reactiveVal(NULL)
  tree_report_val <- reactiveVal(NULL)
  trial_run_state <- reactiveValues(
    running = FALSE,
    stop_requested = FALSE,
    completed = 0L,
    requested = 0L,
    ncores = 1L,
    status = "Idle. Run the simulations to generate trial results.",
    status_type = "idle",
    results = list(),
    scenario_specs = list(),
    scenario_results = list(),
    scenario_index = 1L,
    trial_inputs = NULL,
    run_id = NULL,
    promise = NULL,
    stop_flag_path = NULL,
    progress_path = NULL,
    started_at = NULL
  )
  conduct_row_count <- reactiveVal(1L)
  conduct_uploaded_rows <- reactiveVal(NULL)
  conduct_upload_edit_mode <- reactiveVal(FALSE)
  conduct_upload_error <- reactiveVal(NULL)
  conduct_design_upload_error <- reactiveVal(NULL)
  conduct_design_uploaded <- reactiveVal(NULL)
  conduct_eval_requested <- reactiveVal(FALSE)
  conduct_final_mtd_result_val <- reactiveVal(NULL)
  conduct_final_mtd_status_val <- reactiveVal(list(state = "idle", message = ""))
  conduct_reset_token <- reactiveVal(0L)
  conduct_has_result <- reactive({
    isTRUE(conduct_eval_requested())
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$trial_addon_toggle, {
    label <- if (input$trial_addon_toggle %% 2L == 1L) "Hide Details" else "Show Details"
    updateActionButton(session, "trial_addon_toggle", label = label)
  }, ignoreInit = TRUE)
  
  observeEvent(input$trial_toxicity_toggle, {
    label <- if (input$trial_toxicity_toggle %% 2L == 1L) "Hide Details" else "Show Details"
    updateActionButton(session, "trial_toxicity_toggle", label = label)
  }, ignoreInit = TRUE)
  
  # Trial Design Information: closed at start; Show/Hide Details toggles it, and choosing Manual entry opens it
  # so the design fields are shown together with the manual entry.
  conduct_design_open <- reactiveVal(FALSE)
  output$conduct_design_open <- renderText(if (conduct_design_open()) "open" else "closed")
  outputOptions(output, "conduct_design_open", suspendWhenHidden = FALSE)
  observeEvent(input$conduct_design_toggle, {
    conduct_design_open(!conduct_design_open())
  }, ignoreInit = TRUE)
  observeEvent(input$conduct_design_entry_mode, {
    if (identical(input$conduct_design_entry_mode, "manual")) conduct_design_open(TRUE)
  }, ignoreInit = TRUE)
  observeEvent(conduct_design_open(), {
    updateActionButton(session, "conduct_design_toggle", label = if (conduct_design_open()) "Hide Details" else "Show Details")
  }, ignoreInit = TRUE)
  
  observeEvent(input$conduct_dose_type, {
    if (identical(input$conduct_dose_type, "Continuous")) {
      updateRadioButtons(session, "conduct_trial_mode", selected = "No")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$prior_dose_type, input$dose_unit, input$num_doses, input$min_dose, input$max_dose), {
    if (!identical(input$conduct_design_entry_mode, "manual")) return(invisible(NULL))
    setup_type <- normalize_conduct_dose_type(input$prior_dose_type)
    updateRadioButtons(session, "conduct_dose_type", selected = setup_type)
    if (!is.null(input$dose_unit)) updateTextInput(session, "conduct_dose_unit", value = input$dose_unit)
    if (identical(setup_type, "Continuous")) {
      setup_min_dose <- conduct_numeric_value(input$min_dose)
      setup_max_dose <- conduct_numeric_value(input$max_dose)
      if (conduct_is_finite_scalar(setup_min_dose)) {
        updateNumericInput(session, "conduct_min_dose", value = setup_min_dose)
      }
      if (conduct_is_finite_scalar(setup_max_dose)) {
        updateNumericInput(session, "conduct_max_dose", value = setup_max_dose)
      }
    } else {
      values <- categorical_actual_dose_values()
      if (length(values) > 0L && all(is.finite(values))) {
        updateTextInput(session, "conduct_actual_dose_values", value = paste(values, collapse = ", "))
      }
    }
  }, ignoreInit = FALSE)
  
  # ---- Accelerated cap input inside Design Information ----
  output$conduct_design_accel_cap_ui <- renderUI({
    req(identical(input$conduct_trial_mode, "Yes"))
    num_doses  <- suppressWarnings(as.integer(input$conduct_num_doses))
    startdose  <- suppressWarnings(as.integer(input$conduct_startdose))
    if (!is.finite(num_doses) || num_doses < 2L || !is.finite(startdose) || startdose < 1L) return(NULL)
    uploaded_design <- conduct_design_uploaded()
    uploaded_cap <- if (!is.null(uploaded_design)) suppressWarnings(as.integer(uploaded_design$max_accel_dose_level)) else NA_integer_
    # Provided by the user (or the uploaded workbook); blank by default.
    numericInput("conduct_max_accel_dose_level",
                 "Max Dose Level for Acceleration",
                 value = if (is.finite(uploaded_cap)) uploaded_cap else isolate(input$conduct_max_accel_dose_level), step = 1)
  })
  
  output$conduct_design_upload_status_ui <- renderUI({
    upload_error <- conduct_design_upload_error()
    uploaded_design <- conduct_design_uploaded()
    if (is.character(upload_error) && nzchar(upload_error)) {
      return(div(
        style = "padding: 8px 10px; border: 1px solid #de8f8f; border-radius: 6px; background-color: #fdecec; color: #8b1e1e; margin-bottom: 8px;",
        upload_error
      ))
    }
    if (!is.null(uploaded_design)) {
      return(div(
        style = "padding: 8px 10px; border: 1px solid #91c99b; border-radius: 6px; background-color: #edf8ef; color: #256b38; margin-bottom: 8px;",
        "Trial Design worksheet loaded successfully. Review the imported values below."
      ))
    }
  })
  
  output$conduct_design_upload_summary_ui <- renderUI({
    design <- conduct_design_uploaded()
    if (is.null(design)) {
      return(div(
        style = "padding: 10px; color: #666; font-style: italic;",
        "Upload a valid Trial Conduct workbook to display the trial design information."
      ))
    }
    display_value <- function(field_name) {
      value <- design[[field_name]]
      if (length(value) == 0L || is.na(value) || !nzchar(as.character(value))) return("\u2014")
      as.character(value)
    }
    formula_value <- function(field_name) {
      tags$span(
        style = "display: inline-block; min-width: 48px; padding: 5px 7px; border: 1px solid #ccc; border-radius: 4px; background-color: #f7f7f7; text-align: center;",
        display_value(field_name)
      )
    }
    div(
      id = "conduct_design_upload_summary_inner",
      tags$hr(style = "border-top: 1px solid #ddd; margin: 8px 0 10px 0;"),
      div(
        style = "display: flex; align-items: flex-end; gap: 12px; margin-bottom: 8px; flex-wrap: wrap;",
        conduct_display_box("Dose Type", display_value("dose_type")),
        conduct_display_box("Dose Unit", display_value("dose_unit")),
        conduct_display_box("Accelerated EWOC?", display_value("trial_mode")),
        if (identical(design$trial_mode, "Yes")) {
          conduct_display_box("Max Dose Level for Acceleration", display_value("max_accel_dose_level"))
        }
      ),
      tags$hr(style = "border-top: 1px solid #eee; margin: 4px 0 10px 0;"),
      fluidRow(
        column(
          4,
          tags$p(class = "design-section-label", "Prior"),
          conduct_display_box("P(MTD < Min)", display_value("p0")),
          conduct_display_box("P(MTD > Max)", display_value("p1")),
          div(class = "conduct-beta-formula", tags$span(HTML("&rho;<sub>1</sub> &sim; beta(")), formula_value("a0"), tags$span(","), formula_value("b0"), tags$span(") truncated to [0.001, 0.999]")),
          div(class = "conduct-beta-formula", tags$span(HTML("&rho;<sub>0</sub>/&rho;<sub>1</sub> &sim; beta(")), formula_value("a1"), tags$span(","), formula_value("b1"), tags$span(")"))
        ),
        column(
          4,
          tags$p(class = "design-section-label", "Design"),
          if (identical(design$dose_type, "Continuous")) {
            tagList(
              conduct_display_box("Minimum Dose", display_value("min_dose")),
              conduct_display_box("Maximum Dose", display_value("max_dose")),
              conduct_display_box("Maximum Dose Increment (%)", display_value("max_dose_increment"))
            )
          } else {
            tagList(
              conduct_display_box("Dose Levels", display_value("num_doses")),
              conduct_display_box("Start Dose", display_value("startdose")),
              conduct_display_box("Actual Dose Values", paste(design$actual_dose_levels, collapse = ", "))
            )
          },
          conduct_display_box(HTML("&theta;"), display_value("theta")),
          conduct_display_box(HTML("&Delta;"), display_value("deltastop")),
          conduct_display_box(HTML("&alpha;"), display_value("lower_f_bound")),
          conduct_display_box("Increment", display_value("increment"))
        ),
        column(
          4,
          tags$p(class = "design-section-label", "Add-on"),
          conduct_display_box("Seed", display_value("seed")),
          conduct_display_box("Burn-in", display_value("burn")),
          conduct_display_box("MCMC Samples", display_value("mm")),
          tags$p(class = "design-section-label", "Trial Size and Stopping"),
          # Older workbooks have no Cohort Size / Maximum Sample Size rows: enter them here.
          if (conduct_is_finite_scalar(design$cohort_size)) {
            conduct_display_box("Cohort Size", display_value("cohort_size"))
          } else {
            numericInput("conduct_upload_cohort_size", "Cohort Size (not in workbook - enter it)", value = NA, min = 1, step = 1)
          },
          if (conduct_is_finite_scalar(design$total_sample_size)) {
            conduct_display_box("Maximum Sample Size (N)", display_value("total_sample_size"))
          } else {
            numericInput("conduct_upload_total_n", "Maximum Sample Size N (not in workbook - enter it)", value = NA, min = 1, step = 1)
          },
          conduct_display_box("Efficiency Stopping Rule", display_value("efficiency_rule")),
          if (identical(design$efficiency_rule, "Yes")) conduct_display_box("Maximum Patients at Recommended Dose", display_value("num_patients_restrict")),
          if (identical(design$efficiency_rule, "Yes") && identical(design$dose_type, "Continuous")) conduct_display_box("Dose-Region Width (%)", display_value("efficiency_width"))
        )
      )
    )
  })
  
  normalize_trial_design_parameter <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("\u03b1", "alpha", x, fixed = TRUE)
    x <- gsub("<", " less than ", x, fixed = TRUE)
    x <- gsub(">", " greater than ", x, fixed = TRUE)
    x <- gsub("[^a-z0-9]+", "_", x)
    gsub("^_|_$", "", x)
  }
  
  normalize_conduct_dose_type <- function(x) {
    value <- tolower(trimws(as.character(x)))
    if (length(value) != 1L || is.na(value) || !nzchar(value)) return("Discrete")
    if (value %in% c("continuous", "cont")) "Continuous" else "Discrete"
  }
  
  parse_conduct_actual_dose_values <- function(x) {
    if (length(x) != 1L || is.na(x) || !nzchar(trimws(as.character(x)))) return(numeric(0))
    parts <- unlist(strsplit(as.character(x), "[,;[:space:]]+"))
    parts <- parts[nzchar(trimws(parts))]
    suppressWarnings(as.numeric(parts))
  }
  
  parse_conduct_numeric_with_unit <- function(x) {
    x <- trimws(as.character(x))
    if (length(x) != 1L || is.na(x) || !nzchar(x)) return(NA_real_)
    match <- regexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", x, perl = TRUE)
    if (match[1] < 0) return(NA_real_)
    suppressWarnings(as.numeric(regmatches(x, match)[1]))
  }
  
  extract_categorical_dose_level_rows <- function(parameter, value, num_doses) {
    if (!is.finite(num_doses) || num_doses < 1L) return(numeric(0))
    out <- rep(NA_real_, num_doses)
    matches <- regexec("^dose_level_([0-9]+)_actual_dose", parameter)
    pieces <- regmatches(parameter, matches)
    for (i in seq_along(pieces)) {
      if (length(pieces[[i]]) < 2L) next
      dose_idx <- suppressWarnings(as.integer(pieces[[i]][2]))
      if (is.finite(dose_idx) && dose_idx >= 1L && dose_idx <= num_doses) {
        out[dose_idx] <- parse_conduct_numeric_with_unit(value[i])
      }
    }
    out[is.finite(out)]
  }

  # Unit or description of each dose level: the text after the number in its
  # "Dose Level k Actual Dose" row (e.g. "10 mg BID + drug X 50 mg/m2" -> "mg BID + drug X 50 mg/m2").
  extract_categorical_dose_level_units <- function(parameter, value, num_doses) {
    if (!is.finite(num_doses) || num_doses < 1L) return(character(0))
    out <- rep(NA_character_, num_doses)
    pieces <- regmatches(parameter, regexec("^dose_level_([0-9]+)_actual_dose", parameter))
    for (i in seq_along(pieces)) {
      if (length(pieces[[i]]) < 2L) next
      dose_idx <- suppressWarnings(as.integer(pieces[[i]][2]))
      if (is.finite(dose_idx) && dose_idx >= 1L && dose_idx <= num_doses) {
        out[dose_idx] <- trimws(sub("^\\s*[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?\\s*", "", as.character(value[i])))
      }
    }
    if (any(is.na(out))) character(0) else out
  }

  parse_trial_design_sheet <- function(path) {
    sheets <- readxl::excel_sheets(path)
    sheet_match <- sheets[tolower(trimws(sheets)) == "trial design"]
    if (length(sheet_match) == 0L) {
      stop("The workbook must contain a worksheet named 'Trial Design'.", call. = FALSE)
    }
    design_df <- as.data.frame(
      readxl::read_excel(path, sheet = sheet_match[1], col_names = TRUE, .name_repair = "minimal"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (ncol(design_df) < 2L) {
      stop("The Trial Design worksheet must contain Trial Design Parameter and Value columns.", call. = FALSE)
    }
    parameter <- normalize_trial_design_parameter(design_df[[1]])
    value <- trimws(as.character(design_df[[2]]))
    aliases <- list(
      p0 = c("p_mtd_min", "p_mtd_less_than_min"),
      p1 = c("p_mtd_max", "p_mtd_greater_than_max"),
      a0 = "a0", b0 = "b0", a1 = "a1", b1 = "b1",
      dose_type = c("dose_type", "type_of_dose"),
      dose_unit = c("dose_unit", "unit", "unit_of_dose"),
      min_dose = c("minimum_dose", "min_dose"),
      max_dose = c("maximum_dose", "max_dose"),
      actual_dose_values = c("actual_dose_values", "actual_doses", "dose_values"),
      num_doses = c("dose_levels", "number_of_dose_levels"),
      startdose = c("starting_dose", "start_dose"),
      theta = c("target_dlt_probability_theta", "theta"),
      deltastop = c("stopping_threshold_delta", "threshold_delta", "delta", "deltastop"),
      lower_f_bound = c("starting_alpha", "alpha"),
      increment = c("alpha_increment", "increment"),
      max_dose_increment = c("maximum_dose_increment", "maximum_dose_increment_percent", "max_dose_increment_percent", "dose_increment_percent"),
      trial_mode = c("accelerated_ewoc", "trial_mode"),
      max_accel_dose_level = c("maximum_accelerated_dose", "max_dose_level_for_acceleration"),
      seed = "seed", burn = c("burn_in", "burnin"), mm = c("mcmc_samples", "samples"),
      # Read only to reject workbooks from versions that used the (now removed) flexible dose range.
      flexible_dose_range = "flexible_dose_range",
      cohort_size = "cohort_size",
      total_sample_size = c("maximum_sample_size", "maximum_sample_size_n", "total_sample_size"),
      efficiency_rule = c("efficiency_stopping_rule", "trial_efficiency_stopping_rule"),
      num_patients_restrict = c("maximum_patients_at_recommended_dose", "maximum_patients_per_dose"),
      efficiency_width = c("dose_region_width", "dose_region_width_of_dose_range")
    )
    get_value <- function(alias_values) {
      idx <- which(parameter %in% alias_values)[1]
      if (length(idx) == 0L || is.na(idx)) {
        idx <- which(vapply(parameter, function(p) any(startsWith(p, paste0(alias_values, "_"))), logical(1)))[1]
      }
      if (length(idx) == 0L || is.na(idx)) NA_character_ else value[idx]
    }
    parsed <- lapply(aliases, get_value)
    character_names <- c("trial_mode", "dose_type", "dose_unit", "actual_dose_values", "flexible_dose_range",
                         "efficiency_rule")
    numeric_names <- setdiff(names(parsed), character_names)
    parsed[numeric_names] <- lapply(parsed[numeric_names], function(x) suppressWarnings(as.numeric(x)))
    parsed$dose_type <- normalize_conduct_dose_type(parsed$dose_type)
    parsed$dose_unit <- trimws(as.character(parsed$dose_unit))
    if (length(parsed$dose_unit) != 1L || is.na(parsed$dose_unit)) parsed$dose_unit <- ""
    required_numeric <- c("p0", "p1", "a0", "b0", "a1", "b1",
                          "theta", "deltastop", "lower_f_bound", "increment", "seed", "burn", "mm")
    if (identical(parsed$dose_type, "Discrete")) {
      required_numeric <- c(required_numeric, "num_doses", "startdose")
    } else {
      required_numeric <- c(required_numeric, "min_dose", "max_dose")
    }
    missing_values <- required_numeric[!vapply(parsed[required_numeric], function(x) length(x) == 1L && is.finite(x), logical(1))]
    if (length(missing_values) > 0L) {
      stop(paste0("Complete all required Trial Design values before upload. Missing or invalid: ",
                  paste(missing_values, collapse = ", "), "."), call. = FALSE)
    }
    if (any(unlist(parsed[c("p0", "p1")]) < 0 | unlist(parsed[c("p0", "p1")]) > 1)) {
      stop("P(MTD < Min) and P(MTD > Max) must be between 0 and 1.", call. = FALSE)
    }
    if (any(unlist(parsed[c("a0", "b0", "a1", "b1")]) <= 0)) {
      stop("a0, b0, a1, and b1 must all be positive.", call. = FALSE)
    }
    if (identical(parsed$dose_type, "Discrete")) {
      if (parsed$num_doses < 2 || parsed$num_doses > 12 || parsed$num_doses != floor(parsed$num_doses)) {
        stop("Dose Levels must be a whole number from 2 through 12.", call. = FALSE)
      }
      if (parsed$startdose < 1 || parsed$startdose >= parsed$num_doses || parsed$startdose != floor(parsed$startdose)) {
        stop("Starting Dose must be a whole number between 1 and one level below the highest dose.", call. = FALSE)
      }
      parsed$actual_dose_levels <- parse_conduct_actual_dose_values(parsed$actual_dose_values)
      if (length(parsed$actual_dose_levels) != parsed$num_doses || any(!is.finite(parsed$actual_dose_levels))) {
        parsed$actual_dose_levels <- extract_categorical_dose_level_rows(parameter, value, parsed$num_doses)
      }
      parsed$actual_dose_units <- extract_categorical_dose_level_units(parameter, value, parsed$num_doses)
      if (length(parsed$actual_dose_levels) != parsed$num_doses || any(!is.finite(parsed$actual_dose_levels))) {
        stop("Actual Dose Values must contain one numeric actual dose for each categorical dose level.", call. = FALSE)
      }
      if (anyDuplicated(parsed$actual_dose_levels) > 0L) {
        stop("Each dose level needs a different Actual Dose Value (it identifies the dose given).", call. = FALSE)
      }
    } else {
      parsed$num_doses <- NA_real_
      parsed$startdose <- NA_real_
      parsed$actual_dose_levels <- numeric(0)
      if (parsed$min_dose < 0 || parsed$max_dose <= parsed$min_dose) {
        stop("For continuous dose, Maximum Dose must be greater than Minimum Dose.", call. = FALSE)
      }
    }
    if (parsed$theta <= 0 || parsed$theta >= 1) {
      stop("Target DLT Probability (theta) must be strictly between 0 and 1.", call. = FALSE)
    }
    if (parsed$deltastop <= 0 || parsed$deltastop >= 1) {
      stop("Stopping threshold, Delta, must be strictly between 0 and 1.", call. = FALSE)
    }
    if (parsed$lower_f_bound < 0 || parsed$lower_f_bound > 0.5) {
      stop("\u03b1 must be between 0 and 0.5.", call. = FALSE)
    }
    if (parsed$increment <= 0) stop("\u03b1 Increment must be positive.", call. = FALSE)
    if (identical(parsed$dose_type, "Continuous") &&
        (!is.finite(parsed$max_dose_increment) || parsed$max_dose_increment <= 0)) {
      parsed$max_dose_increment <- 100
    }
    if (parsed$seed < 1 || parsed$seed != floor(parsed$seed)) stop("Seed must be a positive whole number.", call. = FALSE)
    if (parsed$burn < 0 || parsed$burn != floor(parsed$burn)) stop("Burn-in must be a non-negative whole number.", call. = FALSE)
    if (parsed$mm < 1 || parsed$mm != floor(parsed$mm)) stop("MCMC Samples must be a positive whole number.", call. = FALSE)
    mode_value <- tolower(trimws(as.character(parsed$trial_mode)))
    if (!mode_value %in% c("yes", "no")) {
      stop("Accelerated EWOC must be entered as Yes or No.", call. = FALSE)
    }
    parsed$trial_mode <- if (identical(mode_value, "yes")) "Yes" else "No"
    if (identical(parsed$dose_type, "Continuous") && identical(parsed$trial_mode, "Yes")) {
      parsed$trial_mode <- "No"
    }
    if (identical(parsed$trial_mode, "Yes") && !is.finite(parsed$max_accel_dose_level)) {
      stop("Maximum Accelerated Dose is required when Accelerated EWOC is Yes.", call. = FALSE)
    }
    if (identical(parsed$trial_mode, "Yes") &&
        (parsed$max_accel_dose_level < parsed$startdose ||
         parsed$max_accel_dose_level >= parsed$num_doses ||
         parsed$max_accel_dose_level != floor(parsed$max_accel_dose_level))) {
      stop("Maximum Accelerated Dose must be a whole number from the Starting Dose up to one level below the highest dose level.", call. = FALSE)
    }
    # Workbooks from earlier versions may have a "Flexible Dose Range" row: "No" is ignored, "Yes" is
    # not supported because the flexible dose range is not available in this version.
    flexible_value <- tolower(trimws(as.character(parsed$flexible_dose_range)))
    if (length(flexible_value) == 1L && identical(flexible_value, "yes")) {
      stop("This workbook uses the flexible dose range, which is not available in this version of MEWOC. Download a new Trial Conduct workbook from Trial Documents.", call. = FALSE)
    }
    parsed$flexible_dose_range <- NULL
    # Trial size and efficiency stopping: older workbooks without these rows have the efficiency
    # rule off; their cohort size and maximum sample size are entered under the design summary.
    efficiency_value <- tolower(trimws(as.character(parsed$efficiency_rule)))
    parsed$efficiency_rule <- if (length(efficiency_value) == 1L && identical(efficiency_value, "yes")) "Yes" else "No"
    for (field_name in c("cohort_size", "total_sample_size")) {
      value <- parsed[[field_name]]
      if (length(value) == 1L && is.finite(value) && (value < 1 || value != floor(value))) {
        stop("Cohort Size and Maximum Sample Size must be positive whole numbers.", call. = FALSE)
      }
    }
    if (identical(parsed$efficiency_rule, "Yes")) {
      if (!is.finite(parsed$num_patients_restrict) || parsed$num_patients_restrict < 1 ||
          parsed$num_patients_restrict != floor(parsed$num_patients_restrict)) {
        stop("Maximum Patients at Recommended Dose must be a positive whole number when the efficiency stopping rule is used.", call. = FALSE)
      }
      if (identical(parsed$dose_type, "Continuous") &&
          (!is.finite(parsed$efficiency_width) || parsed$efficiency_width <= 0 || parsed$efficiency_width > 100)) {
        stop("Dose-Region Width (%) must be greater than 0 and at most 100.", call. = FALSE)
      }
    }
    parsed
  }
  
  observeEvent(input$conduct_workbook_upload, {
    req(input$conduct_workbook_upload)
    conduct_design_upload_error(NULL)
    parsed <- tryCatch(
      {
        design <- parse_trial_design_sheet(input$conduct_workbook_upload$datapath)
        workbook_sheets <- readxl::excel_sheets(input$conduct_workbook_upload$datapath)
        data_sheet <- workbook_sheets[tolower(trimws(workbook_sheets)) == "data input"]
        if (length(data_sheet) == 0L) {
          stop("The workbook must contain a worksheet named 'Data Input'.", call. = FALSE)
        }
        raw_patient_data <- as.data.frame(
          readxl::read_excel(input$conduct_workbook_upload$datapath, sheet = data_sheet[1], col_names = FALSE, .name_repair = "minimal"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        normalize_conduct_upload(raw_patient_data)
        design
      },
      error = function(e) e
    )
    if (inherits(parsed, "error")) {
      conduct_design_upload_error(conditionMessage(parsed))
      return(invisible(NULL))
    }
    conduct_design_uploaded(parsed)
    input_map <- c(
      p0 = "conduct_p0", p1 = "conduct_p1", a0 = "conduct_a0", b0 = "conduct_b0",
      a1 = "conduct_a1", b1 = "conduct_b1", num_doses = "conduct_num_doses",
      startdose = "conduct_startdose", theta = "conduct_theta",
      deltastop = "conduct_deltastop",
      lower_f_bound = "conduct_lower_f_bound", increment = "conduct_increment",
      min_dose = "conduct_min_dose", max_dose = "conduct_max_dose", max_dose_increment = "conduct_max_dose_increment",
      seed = "conduct_seed", burn = "conduct_burn", mm = "conduct_mm",
      cohort_size = "conduct_cohort_size",
      total_sample_size = "conduct_total_n",
      num_patients_restrict = "conduct_num_patients_restrict",
      efficiency_width = "conduct_efficiency_width"
    )
    updateRadioButtons(session, "conduct_stopping_rule", selected = parsed$efficiency_rule)
    for (field_name in names(input_map)) {
      if (!is.null(parsed[[field_name]]) && length(parsed[[field_name]]) == 1L && is.finite(parsed[[field_name]])) {
        updateNumericInput(session, input_map[[field_name]], value = parsed[[field_name]])
      }
    }
    updateRadioButtons(session, "conduct_dose_type", selected = parsed$dose_type)
    updateTextInput(session, "conduct_dose_unit", value = parsed$dose_unit)
    if (identical(parsed$dose_type, "Discrete")) {
      updateTextInput(session, "conduct_actual_dose_values", value = paste(parsed$actual_dose_levels, collapse = ", "))
      updateTextInput(session, "conduct_actual_dose_units", value = paste(parsed$actual_dose_units, collapse = "; "))
    }
    updateRadioButtons(session, "conduct_trial_mode", selected = parsed$trial_mode)
    conduct_eval_requested(FALSE)
  })
  
  # ---- Confirmation status (green only) ----
  output$conduct_addon_verify_ui <- renderUI({
    if (isTRUE(input$conduct_verify_addon)) {
      tags$small(style = "color: #256b38; margin-left: 4px;", "\u2713 Confirmed")
    } else {
    }
  })
  
  
  output$evaluate_conduct_button_ui <- renderUI({
    trial_completed <- identical(input$conduct_trial_completed, "Yes")
    enabled <- isTRUE(input$conduct_verify_addon) && !isTRUE(trial_completed)
    actionButton(
      "evaluateConduct",
      "Evaluate Next Dose Recommendation",
      disabled = if (enabled) NULL else "disabled",
      style = if (enabled) {
        "color: white; background-color: #28a745; border-color: #28a745; font-weight: bold;"
      } else {
        "color: #777; background-color: #e9ecef; border-color: #ced4da; font-weight: bold; cursor: not-allowed;"
      }
    )
  })
  
  
  
  
  clear_trial_outputs <- function() {
    output$scenario_results_tabs <- renderUI(NULL)
    output$eff_table1_block <- renderUI(NULL)
    output$dlt_table2_block <- renderUI(NULL)
    output$stopping_summary_block <- renderUI(NULL)
    output$stopped_details_block <- renderUI(NULL)
    output$table1 <- renderTable(NULL)
    output$table2 <- renderTable(NULL)
    output$stopped_trial_details <- renderText(NULL)
  }
  
  scenario_dose_input_id <- function(scenario_index, dose_index) {
    if (identical(as.integer(scenario_index), 1L)) {
      paste0("dose_", dose_index)
    } else {
      paste0("scenario_", scenario_index, "_dose_", dose_index)
    }
  }
  
  validate_extra_scenario_dose_probabilities <- function(scenario_index, num_doses) {
    doseprobs <- vapply(seq_len(num_doses), function(i) {
      value <- suppressWarnings(as.numeric(input[[scenario_dose_input_id(scenario_index, i)]]))
      if (is.na(value)) NA_real_ else value
    }, numeric(1))
    
    scenario_label <- paste("Scenario", scenario_index)
    if (any(is.na(doseprobs))) {
      show_validation_error(
        paste("Missing dose probabilities for", scenario_label),
        paste("Please enter a numeric probability (0-1) for each dose level in", scenario_label, ".")
      )
      return(NULL)
    }
    if (any(!is.finite(doseprobs)) || any(doseprobs < 0 | doseprobs > 1)) {
      show_validation_error(
        paste("Invalid dose probabilities for", scenario_label),
        paste("Each dose probability in", scenario_label, "must be a numeric value between 0 and 1.")
      )
      return(NULL)
    }
    
    as.numeric(doseprobs)
  }
  
  validate_continuous_scenario_truth <- function(scenario_index, min_dose, max_dose, theta) {
    scenario_label <- paste("Scenario", scenario_index)
    true_mtd <- suppressWarnings(as.numeric(input[[paste0("cont_true_mtd_", scenario_index)]]))
    true_p_min <- suppressWarnings(as.numeric(input[[paste0("cont_true_p_min_", scenario_index)]]))
    
    if (!is.finite(true_mtd) || true_mtd < 0) {
      show_validation_error(
        paste("Invalid true MTD for", scenario_label),
        paste("True MTD dose in", scenario_label, "must be numeric and greater than or equal to 0.")
      )
      return(NULL)
    }
    if (!is.finite(true_p_min) || true_p_min <= 0 || true_p_min >= 1) {
      show_validation_error(
        paste("Invalid minimum-dose truth for", scenario_label),
        paste("True DLT probability at the minimum dose in", scenario_label, "must be strictly between 0 and 1.")
      )
      return(NULL)
    }
    if (abs(true_mtd - min_dose) < sqrt(.Machine$double.eps)) {
      show_validation_error(
        paste("Invalid true MTD for", scenario_label),
        paste("True MTD dose in", scenario_label, "cannot equal the minimum dose when the minimum-dose DLT probability is used as the curve anchor.")
      )
      return(NULL)
    }
    if (true_mtd > min_dose && true_p_min >= theta) {
      show_validation_error(
        paste("Incoherent true curve for", scenario_label),
        paste("When true MTD is above the minimum dose, true DLT probability at the minimum dose must be below theta.")
      )
      return(NULL)
    }
    if (true_mtd < min_dose && true_p_min <= theta) {
      show_validation_error(
        paste("Incoherent true curve for", scenario_label),
        paste("When true MTD is below the minimum dose, true DLT probability at the minimum dose must be above theta.")
      )
      return(NULL)
    }
    
    true_mtd_std <- (true_mtd - min_dose) / (max_dose - min_dose)
    beta0 <- qlogis(true_p_min)
    beta1 <- (qlogis(theta) - beta0) / true_mtd_std
    true_p_max <- plogis(beta0 + beta1)
    if (!is.finite(beta1) || beta1 <= 0 || !is.finite(true_p_max) || true_p_max <= 0 || true_p_max >= 1) {
      show_validation_error(
        paste("Incoherent true curve for", scenario_label),
        paste("The selected true MTD and minimum-dose DLT probability do not define a valid increasing logistic toxicity curve.")
      )
      return(NULL)
    }
    
    list(
      true_mtd = true_mtd,
      true_p_min = true_p_min,
      true_p_max = true_p_max,
      true_mtd_std = true_mtd_std,
      true_beta0 = beta0,
      true_beta1 = beta1
    )
  }
  
  collect_trial_scenario_specs <- function(trial_inputs) {
    scenario_count <- suppressWarnings(as.integer(trial_scenario_count()))
    if (length(scenario_count) != 1L || !is.finite(scenario_count) || scenario_count < 1L) {
      scenario_count <- 1L
    }
    
    specs <- vector("list", scenario_count)
    if (identical(trial_inputs$dose_type, "Continuous")) {
      for (scenario_index in seq_len(scenario_count)) {
        truth <- validate_continuous_scenario_truth(
          scenario_index = scenario_index,
          min_dose = trial_inputs$min_dose,
          max_dose = trial_inputs$max_dose,
          theta = trial_inputs$theta
        )
        if (is.null(truth)) {
          return(NULL)
        }
        specs[[scenario_index]] <- c(
          list(
            label = paste("Scenario", scenario_index),
            doseprobs = numeric(0)
          ),
          truth
        )
      }
      return(specs)
    }
    
    specs[[1]] <- list(
      label = "Scenario 1",
      doseprobs = as.numeric(trial_inputs$doseprobs)
    )

    if (scenario_count >= 2L) {
      for (scenario_index in 2:scenario_count) {
        doseprobs <- validate_extra_scenario_dose_probabilities(
          scenario_index = scenario_index,
          num_doses = trial_inputs$num_doses
        )
        if (is.null(doseprobs)) {
          return(NULL)
        }
        specs[[scenario_index]] <- list(
          label = paste("Scenario", scenario_index),
          doseprobs = doseprobs
        )
      }
    }

    specs
  }
  
  summary_metric_row_ui <- function(label, value) {
    div(
      style = "display: flex; justify-content: space-between; gap: 12px; padding: 4px 0;",
      span(label),
      tags$strong(as.character(value))
    )
  }
  
  summary_card_ui <- function(title, rows) {
    div(
      style = paste(
        "border: 1px solid #d9d9d9;",
        "border-radius: 8px;",
        "padding: 10px 12px;",
        "margin-bottom: 10px;",
        "background-color: #fafafa;"
      ),
      div(style = "font-weight: 600; margin-bottom: 6px;", title),
      rows
    )
  }
  
  table_tag_ui <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    header <- tags$tr(lapply(names(df), tags$th))
    body <- lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(df[i, , drop = FALSE], function(value) tags$td(as.character(value))))
    })
    
    tags$table(
      class = "table table-striped table-bordered table-condensed",
      tags$thead(header),
      tags$tbody(body)
    )
  }
  
  stopped_trial_details_text <- function(stopped_df) {
    if (is.null(stopped_df) || nrow(stopped_df) == 0) {
      return("Stopping rule was not triggered in any trial.")
    }
    
    n_total <- nrow(stopped_df)
    n_show <- min(3, n_total)
    lines <- c(
      paste("Stopping rule triggered in", n_total, "trial(s)."),
      paste("Showing first", n_show, "trial(s) below."),
      ""
    )
    for (ii in seq_len(n_show)) {
      lines <- c(
        lines,
        paste0("Trial ", stopped_df$Trial[ii], ": Number of dose values created = ", stopped_df$N_Dose_Values[ii]),
        paste0("Dose values: ", stopped_df$Dose_Values[ii]),
        paste0("DLT values: ", stopped_df$DLT_Values[ii]),
        ""
      )
    }
    paste(lines, collapse = "\n")
  }
  
  trial_summary_ui <- function(trial_summary) {
    if (is.null(trial_summary)) {
      return(NULL)
    }
    trial_summary$combined_table <- round_trial_efficiency_percentages(trial_summary$combined_table)
    
    trial_mode <- trial_summary$inputs$trial_mode

    # SK: previously this built its own ad-hoc set of cards (Delta-Based Summary,
    # Patient Allocation Stopping Rule Summary,
    # plus a separate standalone "Overall Sample Size Summary" card) instead of using
    # trial_summary$stopping_summary_table -- the same consolidated table (with the
    # "Overall" row) used by the single-scenario view. That meant the per-scenario tabs
    # showed different, redundant, and not-always-matching numbers for the same info.
    # Now mirrors the single-scenario view: the one stopping_summary_table plus, when
    # applicable, the Accelerated Startup Summary card.
    accelerated_card <- NULL
    if (identical(trial_mode, "Accelerated EWOC")) {
      accel_reason_values <- trial_summary$accel_stop_reason[!is.na(trial_summary$accel_stop_reason) & nzchar(trial_summary$accel_stop_reason)]
      accel_entry_values <- trial_summary$ewoc_entry_dose[is.finite(trial_summary$ewoc_entry_dose)]
      accel_reason <- if (length(accel_reason_values) > 0) names(sort(table(accel_reason_values), decreasing = TRUE))[1] else "Not available"
      accel_entry <- if (length(accel_entry_values) > 0) {
        entry_idx <- as.integer(names(sort(table(accel_entry_values), decreasing = TRUE))[1])
        dose_labels <- trial_summary$inputs$actual_dose_levels
        if (!is.null(dose_labels)) {
          # SK: prefer the per-dose-level units when available (matches the
          # dose count); falls back to the shared-unit-or-"" value for older
          # trial_summary$inputs that predate per-dose units.
          dose_units_for_labels <- trial_summary$inputs$actual_dose_units
          if (is.null(dose_units_for_labels) || length(dose_units_for_labels) != length(dose_labels)) {
            dose_units_for_labels <- trial_summary$inputs$dose_unit
          }
          dose_labels <- build_discrete_dose_labels(dose_labels, dose_units_for_labels, length(dose_labels))
        }
        if (!is.null(dose_labels) && entry_idx >= 1L && entry_idx <= length(dose_labels)) dose_labels[entry_idx] else paste("Dose", entry_idx)
      } else {
        "Not available"
      }
      accelerated_card <- summary_card_ui(
        "Accelerated Startup Summary",
        tagList(
          summary_metric_row_ui("Mean accelerated singleton patients", round(mean(trial_summary$accel_patients, na.rm = TRUE), 3)),
          summary_metric_row_ui("Most common accelerated stop reason", accel_reason),
          summary_metric_row_ui("Most common EWOC entry dose", accel_entry)
        )
      )
    }
    
    tagList(
      h4(tags$u("Table 1: Trial Efficiency Summary")),
      table_tag_ui(data.frame(trial_summary$combined_table, check.names = FALSE)),
      h4(tags$u("Table 2: Safety Summary")),
      table_tag_ui(data.frame(trial_summary$dlt_summary, check.names = FALSE)),
      h4(tags$u("Stopping summary")),
      table_tag_ui(trial_summary$stopping_summary_table),
      accelerated_card,
      if (!is.null(trial_summary$stopped_df) && nrow(trial_summary$stopped_df) > 0L) {
        tagList(
          h4(tags$u("Stopped trial details")),
          tags$pre(stopped_trial_details_text(trial_summary$stopped_df))
        )
      } else {
      }
    )
  }
  
  build_combined_scenario_csv_df <- function(scenario_results) {
    if (is.null(scenario_results) || length(scenario_results) == 0) {
      return(build_trial_summary_csv_df(NULL))
    }
    
    do.call(rbind, lapply(scenario_results, function(item) {
      item$summary$combined_table <- round_trial_efficiency_percentages(item$summary$combined_table)
      scenario_df <- build_trial_summary_csv_df(item$summary)
      data.frame(
        Scenario = item$label,
        scenario_df,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))
  }
  
  
  
  
  clean_excel_sheet_name <- function(sheet_name, fallback_name) {
    sheet_name <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", as.character(sheet_name))
    sheet_name <- trimws(sheet_name)
    if (!nzchar(sheet_name)) {
      sheet_name <- fallback_name
    }
    substr(sheet_name, 1L, 31L)
  }
  
  write_scenario_results_workbook <- function(scenario_results, file) {
    if (is.null(scenario_results) || length(scenario_results) == 0) {
      trial_data <- trial_report_val()
      if (is.null(trial_data) || is.null(trial_data$combined_table)) {
        stop("Trial results are not yet available. Run the simulations first.")
      }
      scenario_results <- list(list(label = "Scenario 1", summary = trial_data))
    }
    
    workbook <- openxlsx::createWorkbook()
    used_sheet_names <- character(0)
    
    for (i in seq_along(scenario_results)) {
      item <- scenario_results[[i]]
      sheet_name <- clean_excel_sheet_name(item$label, paste("Scenario", i))
      if (sheet_name %in% used_sheet_names) {
        sheet_name <- clean_excel_sheet_name(paste0(sheet_name, "_", i), paste("Scenario", i))
      }
      used_sheet_names <- c(used_sheet_names, sheet_name)
      
      openxlsx::addWorksheet(workbook, sheet_name)
      openxlsx::writeData(
        workbook,
        sheet = sheet_name,
        x = build_trial_summary_csv_df(item$summary),
        rowNames = FALSE
      )
      openxlsx::setColWidths(workbook, sheet = sheet_name, cols = 1:3, widths = "auto")
    }
    
    openxlsx::saveWorkbook(workbook, file = file, overwrite = TRUE)
  }
  
  
  reset_trial_run_artifacts <- function() {
    paths_to_clear <- isolate(c(trial_run_state$stop_flag_path, trial_run_state$progress_path))
    for (path_value in paths_to_clear) {
      if (is.character(path_value) && length(path_value) == 1L && nzchar(path_value) && file.exists(path_value)) {
        try(unlink(path_value), silent = TRUE)
      }
    }
    # Per-trial completion markers used for the live progress count.
    done_dir <- trial_progress_done_dir(isolate(trial_run_state$progress_path))
    if (!is.null(done_dir) && dir.exists(done_dir)) try(unlink(done_dir, recursive = TRUE), silent = TRUE)
    isolate({
      trial_run_state$stop_flag_path <- NULL
      trial_run_state$progress_path <- NULL
      trial_run_state$promise <- NULL
    })
  }
  
  session$onSessionEnded(function() {
    # A run still in progress is asked to stop (the background session checks this file between
    # batches); the flag is left in place so it can see it, and is removed when the run returns.
    stop_path <- isolate(trial_run_state$stop_flag_path)
    if (isTRUE(isolate(trial_run_state$running)) && is.character(stop_path) && length(stop_path) == 1L && nzchar(stop_path)) {
      try(file.create(stop_path), silent = TRUE)
      return(invisible(NULL))
    }
    reset_trial_run_artifacts()
  })
  
  update_trial_run_status <- function(message, status_type = "info") {
    trial_run_state$status <- message
    trial_run_state$status_type <- status_type
  }
  
  output$trial_run_status_ui <- renderUI({
    status_type <- trial_run_state$status_type
    completed   <- trial_run_state$completed
    requested   <- trial_run_state$requested
    
    palette <- switch(
      status_type,
      running = list(bg = "#eef7ff", border = "#7db7ff", text = "#1f4f82"),
      success = list(bg = "#edf8f0", border = "#84c49a", text = "#256b38"),
      warning = list(bg = "#fff6e6", border = "#f0ad4e", text = "#8a5a20"),
      error   = list(bg = "#fdecec", border = "#de8f8f", text = "#8b1e1e"),
      idle    = list(bg = "#f6f6f6", border = "#d4d4d4", text = "#4a4a4a"),
      list(bg = "#eef7ff", border = "#7db7ff", text = "#1f4f82")
    )
    
    is_running <- status_type %in% c("running", "warning")
    show_bar   <- is_running &&
      is.numeric(requested) && length(requested) == 1L &&
      is.finite(requested) && requested > 0
    pct <- if (show_bar) min(100L, round(100L * completed / requested)) else 0L
    # Before the first trial finishes, show a full-width moving bar so the run is visibly active.
    starting <- show_bar && pct == 0L

    tagList(
      div(
        style = paste(
          "border: 1px solid", palette$border, ";",
          "background-color:", palette$bg, ";",
          "color:", palette$text, ";",
          "border-radius: 8px;",
          "padding: 10px 12px;",
          "font-weight: 500;"
        ),
        trial_run_state$status
      ),
      if (show_bar) div(
        style = "margin-top: 6px;",
        div(
          class = "progress",
          style = "height: 20px; border-radius: 6px;",
          div(
            class = "progress-bar progress-bar-striped active",
            style = paste0(
              "width: ", if (starting) 100L else pct, "%;",
              if (starting) "opacity: 0.45;" else "",
              "background-color: #4a90d9;",
              "line-height: 20px;",
              "font-size: 13px;",
              "transition: width 0.6s ease;"
            ),
            role = "progressbar",
            `aria-valuenow` = as.character(pct),
            `aria-valuemin` = "0",
            `aria-valuemax` = "100",
            if (pct >= 15L) paste0(pct, "%") else NULL
          )
        )
      )
    )
  })
  
  format_conduct_value <- function(value, digits = 4) {
    if (!conduct_is_finite_scalar(value)) {
      return("NA")
    }
    as.character(round(value, digits))
  }
  
  conduct_is_finite_scalar <- function(value) {
    value_num <- suppressWarnings(as.numeric(value))
    length(value_num) == 1L && isTRUE(is.finite(value_num))
  }
  
  conduct_text_value <- function(value, default) {
    value_chr <- trimws(as.character(value))
    if (length(value_chr) == 1L && !is.na(value_chr) && nzchar(value_chr)) {
      return(value_chr)
    }
    default_chr <- trimws(as.character(default))
    if (length(default_chr) == 1L && !is.na(default_chr) && nzchar(default_chr)) {
      return(default_chr)
    }
    ""
  }
  
  conduct_integer_value <- function(value, default = NA_integer_) {
    value_int <- suppressWarnings(as.integer(value))
    if (length(value_int) == 1L && is.finite(value_int)) {
      return(value_int)
    }
    default_int <- suppressWarnings(as.integer(default))
    if (length(default_int) == 1L && is.finite(default_int)) {
      return(default_int)
    }
    NA_integer_
  }
  
  
  conduct_numeric_value <- function(value, default = NA_real_) {
    value_num <- suppressWarnings(as.numeric(value))
    if (length(value_num) == 1L && is.finite(value_num)) {
      return(value_num)
    }
    default_num <- suppressWarnings(as.numeric(default))
    if (length(default_num) == 1L && is.finite(default_num)) {
      return(default_num)
    }
    NA_real_
  }
  
  
  conduct_manual_actual_dose_values <- function(num_doses) {
    num_doses <- suppressWarnings(as.integer(num_doses))
    if (length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 2L) {
      return(numeric(0))
    }
    values <- parse_conduct_actual_dose_values(input$conduct_actual_dose_values)
    if (length(values) == num_doses && all(is.finite(values))) {
      return(values)
    }
    setup_values <- categorical_actual_dose_values()
    if (length(setup_values) == num_doses && all(is.finite(setup_values))) {
      return(setup_values)
    }
    numeric(0)
  }
  
  conduct_current_design <- function() {
    uploaded <- if (identical(input$conduct_design_entry_mode, "upload")) conduct_design_uploaded() else NULL
    dose_type <- if (!is.null(uploaded)) {
      normalize_conduct_dose_type(uploaded$dose_type)
    } else {
      normalize_conduct_dose_type(input$conduct_dose_type)
    }
    dose_unit <- if (!is.null(uploaded)) uploaded$dose_unit else input$conduct_dose_unit
    dose_unit <- trimws(as.character(dose_unit))
    if (length(dose_unit) != 1L || is.na(dose_unit)) dose_unit <- ""
    if (identical(dose_type, "Continuous")) {
      min_dose <- conduct_numeric_value(if (!is.null(uploaded)) uploaded$min_dose else input$conduct_min_dose)
      max_dose <- conduct_numeric_value(if (!is.null(uploaded)) uploaded$max_dose else input$conduct_max_dose)
      max_dose_increment <- conduct_numeric_value(if (!is.null(uploaded)) uploaded$max_dose_increment else input$conduct_max_dose_increment)
      if (!conduct_is_finite_scalar(max_dose_increment) || max_dose_increment <= 0) max_dose_increment <- 100
      return(list(
        dose_type = "Continuous",
        dose_unit = dose_unit,
        min_dose = min_dose,
        max_dose = max_dose,
        max_dose_increment = max_dose_increment,
        num_doses = NA_integer_,
        startdose = NA_integer_,
        actual_dose_levels = numeric(0),
        dose_units = character(0),
        doselevels = numeric(0)
      ))
    }
    num_doses <- conduct_integer_value(if (!is.null(uploaded)) uploaded$num_doses else input$conduct_num_doses)
    startdose <- conduct_integer_value(if (!is.null(uploaded)) uploaded$startdose else input$conduct_startdose)
    actual_dose_levels <- if (!is.null(uploaded) && "actual_dose_levels" %in% names(uploaded)) {
      suppressWarnings(as.numeric(uploaded$actual_dose_levels))
    } else {
      conduct_manual_actual_dose_values(num_doses)
    }
    # Per-level unit or description (levels may use different units or be combinations).
    dose_units <- if (!is.null(uploaded) && length(uploaded$actual_dose_units) > 0L) {
      as.character(uploaded$actual_dose_units)
    } else {
      manual_units <- trimws(unlist(strsplit(as.character(if (is.null(input$conduct_actual_dose_units)) "" else input$conduct_actual_dose_units), ";", fixed = TRUE)))
      manual_units[is.na(manual_units)] <- ""
      manual_units
    }
    if (!is.finite(num_doses) || length(dose_units) != num_doses) {
      dose_units <- rep(dose_unit, max(0L, if (is.finite(num_doses)) num_doses else 0L))
    }
    dose_units[!nzchar(dose_units)] <- dose_unit
    # Discrete dose levels are ordered steps: equally spaced positions, the same as in the
    # simulations. Actual dose values and units are labels (and identify the dose given).
    doselevels <- if (is.finite(num_doses) && num_doses >= 2L) {
      as.numeric(seq(0, 1, length.out = num_doses))
    } else {
      numeric(0)
    }
    # Display label of each dose level (its value with its own unit or description).
    k <- length(doselevels)
    label_values <- if (length(actual_dose_levels) == k) actual_dose_levels else rep(NA_real_, k)
    label_units <- if (length(dose_units) == k) dose_units else rep("", k)
    list(
      dose_type = "Discrete",
      dose_unit = dose_unit,
      min_dose = NA_real_,
      max_dose = NA_real_,
      max_dose_increment = NA_real_,
      num_doses = num_doses,
      startdose = startdose,
      actual_dose_levels = actual_dose_levels,
      dose_units = dose_units,
      doselevels = doselevels,
      dose_labels = build_discrete_dose_labels(label_values, label_units, k)
    )
  }

  # Dose label for display. For a discrete design, the label of the matching dose level
  # (its value with its own unit or description); otherwise value plus the design's unit.
  conduct_dose_label <- function(value, dose_unit = "", design = NULL) {
    if (!is.finite(value)) return("NA")
    if (!is.null(design) && identical(design$dose_type, "Discrete") && length(design$dose_labels) > 0L) {
      idx <- which(abs(design$actual_dose_levels - value) <= sqrt(.Machine$double.eps) * pmax(1, abs(value)))
      if (length(idx) >= 1L) return(design$dose_labels[idx[1]])
    }
    label <- format_dose_value(value)
    if (nzchar(trimws(as.character(dose_unit)))) paste(label, trimws(as.character(dose_unit))) else label
  }
  
  # conduct_standardize_actual_dose() and conduct_actual_from_standardized() are in R/conduct_logic.R.

  # Trial size and efficiency stopping settings: from the uploaded workbook, otherwise the manual inputs.
  # An older workbook without Cohort Size / Maximum Sample Size uses the inputs shown under its design
  # summary, and its efficiency stopping rule is off.
  conduct_trial_rule_settings <- function() {
    uploaded <- if (identical(input$conduct_design_entry_mode, "upload")) conduct_design_uploaded() else NULL
    pick <- function(field, manual_id, upload_fallback_id = NULL) {
      if (!is.null(uploaded)) {
        value <- uploaded[[field]]
        if (length(value) == 1L && !is.na(value) && nzchar(as.character(value))) return(value)
        return(if (is.null(upload_fallback_id)) NA else input[[upload_fallback_id]])
      }
      input[[manual_id]]
    }
    efficiency_rule <- identical(as.character(pick("efficiency_rule", "conduct_stopping_rule")), "Yes")
    list(
      cohort_size = conduct_integer_value(pick("cohort_size", "conduct_cohort_size", "conduct_upload_cohort_size")),
      total_sample_size = conduct_integer_value(pick("total_sample_size", "conduct_total_n", "conduct_upload_total_n")),
      efficiency_rule = efficiency_rule,
      num_patients_restrict = if (efficiency_rule) conduct_integer_value(pick("num_patients_restrict", "conduct_num_patients_restrict")) else NA_integer_,
      efficiency_width = if (efficiency_rule) conduct_numeric_value(pick("efficiency_width", "conduct_efficiency_width")) else NA_real_
    )
  }

  # Error message for invalid trial size / efficiency settings, or NULL when they are valid.
  conduct_trial_rule_settings_error <- function(rule_settings, is_continuous) {
    if (!conduct_is_finite_scalar(rule_settings$cohort_size) || rule_settings$cohort_size < 1L) {
      return("Enter the Cohort Size (minimum number of patients per cohort).")
    }
    if (!conduct_is_finite_scalar(rule_settings$total_sample_size) || rule_settings$total_sample_size < rule_settings$cohort_size) {
      return("Enter the Maximum Sample Size N (at least the cohort size).")
    }
    if (isTRUE(rule_settings$efficiency_rule)) {
      missing_fields <- c(
        if (!conduct_is_finite_scalar(rule_settings$num_patients_restrict)) "the Maximum Patients at Recommended Dose",
        if (is_continuous && !conduct_is_finite_scalar(rule_settings$efficiency_width)) "the Dose-Region Width (%)"
      )
      if (length(missing_fields)) {
        return(paste0("The efficiency stopping rule is selected. Please provide ", paste(missing_fields, collapse = " and "), ", or set the rule to No."))
      }
      if (!conduct_is_finite_scalar(rule_settings$num_patients_restrict) || rule_settings$num_patients_restrict < 1L ||
          rule_settings$num_patients_restrict > rule_settings$total_sample_size) {
        return("Maximum Patients at Recommended Dose must be a whole number from 1 to the maximum sample size.")
      }
      if (is_continuous && (!conduct_is_finite_scalar(rule_settings$efficiency_width) ||
                            rule_settings$efficiency_width <= 0 || rule_settings$efficiency_width > 100)) {
        return("Dose-Region Width (%) must be greater than 0 and at most 100.")
      }
    }
  }

  build_conduct_dose_metadata_table <- function(dose_type, dose_unit = "", min_dose = "", max_dose = "",
                                                actual_dose_levels = numeric(0), num_doses = "", startdose = "",
                                                max_dose_increment = "", actual_dose_units = character(0)) {
    dose_type <- normalize_conduct_dose_type(dose_type)
    dose_unit <- trimws(as.character(dose_unit))
    if (length(dose_unit) != 1L || is.na(dose_unit)) dose_unit <- ""
    unit_suffix <- if (nzchar(dose_unit)) paste0(" (", dose_unit, ")") else ""
    # SK: Discrete mode now collects one unit per dose level rather than a
    # single overall unit -- when a matching per-dose actual_dose_units
    # vector is supplied, use those instead of the shared dose_unit and skip
    # printing a top-level "Dose Unit" row (each level's row already carries
    # its own unit). Continuous mode, and any caller not passing per-dose
    # units (e.g. the Trial Conduct tab's own single dose_unit field), keep
    # the original shared-unit behavior.
    has_per_dose_units <- identical(dose_type, "Discrete") &&
      length(actual_dose_units) == length(actual_dose_levels) && length(actual_dose_levels) > 0L
    rows <- if (has_per_dose_units) {
      data.frame(
        `Trial Design Parameter` = "Dose Type",
        Value = dose_type,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        `Trial Design Parameter` = c("Dose Type", "Dose Unit"),
        Value = c(dose_type, dose_unit),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    if (identical(dose_type, "Continuous")) {
      rows <- rbind(
        rows,
        data.frame(
          `Trial Design Parameter` = c(
            paste0("Minimum Dose", unit_suffix),
            paste0("Maximum Dose", unit_suffix),
            "Maximum Dose Increment (%)"
          ),
          Value = c(min_dose, max_dose, max_dose_increment),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      )
    } else {
      dose_value_labels <- if (has_per_dose_units) {
        vapply(seq_along(actual_dose_levels), function(i) {
          conduct_dose_label(actual_dose_levels[i], actual_dose_units[i])
        }, character(1))
      } else {
        vapply(actual_dose_levels, function(x) conduct_dose_label(x, dose_unit), character(1))
      }
      level_rows <- data.frame(
        `Trial Design Parameter` = c("Dose Levels", "Starting Dose", "Actual Dose Values"),
        Value = c(num_doses, startdose, paste(actual_dose_levels, collapse = ", ")),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      if (length(actual_dose_levels) > 0L) {
        level_rows <- rbind(
          level_rows,
          data.frame(
            `Trial Design Parameter` = paste0(
              "Dose Level ", seq_along(actual_dose_levels), " Actual Dose",
              if (has_per_dose_units) "" else unit_suffix
            ),
            Value = dose_value_labels,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        )
      }
      rows <- rbind(rows, level_rows)
    }
    rows
  }
  
  conduct_display_box <- function(label, value) {
    div(
      tags$label(label),
      div(
        style = paste(
          "height: 34px; padding: 6px 12px; border: 1px solid #ccc;",
          "border-radius: 4px; background-color: #f7f7f7; overflow: hidden;"
        ),
        value
      )
    )
  }
  
  parse_conduct_dlt <- function(value) {
    value_chr <- trimws(tolower(as.character(value)))
    if (length(value_chr) != 1L || is.na(value_chr) || !nzchar(value_chr)) {
      return(NA_integer_)
    }
    if (value_chr %in% c("1", "yes", "y", "true", "dlt")) {
      return(1L)
    }
    if (value_chr %in% c("0", "no", "n", "false", "no dlt", "nodlt")) {
      return(0L)
    }
    if (value_chr %in% c("unevaluable", "u", "ue", "not evaluable", "not_evaluable", "inevaluable")) {
      return(NA_integer_)
    }
    NA_integer_
  }
  
  is_unevaluable_dlt_str <- function(x) {
    value_chr <- trimws(tolower(as.character(x)))
    length(value_chr) == 1L && !is.na(value_chr) &&
      value_chr %in% c("unevaluable", "u", "ue", "not evaluable", "not_evaluable", "inevaluable")
  }
  
  clean_conduct_upload_name <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("\u03b1", "alpha", x, fixed = TRUE)
    x <- gsub("[^a-z0-9]+", "_", x)
    gsub("^_|_$", "", x)
  }
  
  map_conduct_upload_columns <- function(names_value) {
    clean_names <- clean_conduct_upload_name(names_value)
    aliases <- list(
      cohort = c("cohort", "cohort_number", "cohort_no", "cohort_id"),
      patient = c("patient", "patient_number", "patient_no", "patient_id", "subject", "subject_id"),
      administered_dose = c("administered_dose", "administered_dose_level", "dose_level_administered", "dose_given", "dose_admin", "dose_administered", "dose", "dose_level", "doselevel", "actual_dose", "given_dose"),
      dlt = c("dlt", "dlt_status", "toxicity", "toxicity_status", "dose_limiting_toxicity"),
      recommended_dose = c("recommended_dose", "recommended_dose_level", "dose_level_recommended", "dose_recommended", "recommendation", "dose_recommendation"),
      alpha_used = c("alpha_used", "alpha", "alpha_value", "alpha_for_recommendation"),
      suggested_next_alpha = c("suggested_next_alpha", "next_alpha", "suggested_alpha", "recommended_next_alpha")
    )
    mapped <- lapply(aliases, function(alias_values) {
      match_idx <- which(clean_names %in% alias_values)[1]
      if (length(match_idx) == 0L || is.na(match_idx)) NA_integer_ else match_idx
    })
    required <- c("cohort", "patient", "administered_dose", "dlt")
    if (any(vapply(mapped[required], function(x) length(x) == 0L || is.na(x), logical(1)))) {
      return(NULL)
    }
    unlist(mapped)
  }
  
  normalize_conduct_upload <- function(df) {
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    column_map <- map_conduct_upload_columns(names(df))
    if (is.null(column_map)) {
      search_rows <- seq_len(min(nrow(df), 20L))
      header_row <- NA_integer_
      for (row_idx in search_rows) {
        candidate_map <- map_conduct_upload_columns(unlist(df[row_idx, ], use.names = FALSE))
        if (!is.null(candidate_map)) {
          header_row <- row_idx
          column_map <- candidate_map
          break
        }
      }
      if (is.na(header_row)) {
        found_names <- paste(unique(unlist(df[seq_len(min(3L, nrow(df))), ], use.names = FALSE)), collapse = ", ")
        stop(paste0(
          "Could not find the required columns in your file. ",
          "Need: Cohort, Patient, Dose Given, and DLT Status. ",
          "Values found in first rows: ", found_names, ". ",
          "Download the blank template, fill it in, and re-upload."
        ), call. = FALSE)
      }
      if (header_row >= nrow(df)) {
        stop("Upload did not contain any patient rows after the header.", call. = FALSE)
      }
      df <- df[(header_row + 1L):nrow(df), , drop = FALSE]
    }
    if (nrow(df) == 0L) {
      stop("Upload did not contain any patient rows after the header.", call. = FALSE)
    }
    get_col <- function(name) df[[column_map[[name]]]]
    raw_dlt <- get_col("dlt")
    out <- data.frame(
      Cohort = trimws(as.character(get_col("cohort"))),
      Patient = trimws(as.character(get_col("patient"))),
      Administered_Dose = suppressWarnings(as.numeric(get_col("administered_dose"))),
      Administered_Dose_Text = trimws(as.character(get_col("administered_dose"))),
      DLT = vapply(raw_dlt, parse_conduct_dlt, integer(1)),
      Is_Unevaluable = vapply(raw_dlt, is_unevaluable_dlt_str, logical(1)),
      DLT_Status_Text = trimws(as.character(raw_dlt)),
      check.names = FALSE
    )
    if (!is.na(column_map[["recommended_dose"]])) {
      out$Uploaded_Recommended_Dose <- suppressWarnings(as.numeric(get_col("recommended_dose")))
    }
    if (!is.na(column_map[["alpha_used"]])) {
      out$Uploaded_Alpha_Used <- suppressWarnings(as.numeric(get_col("alpha_used")))
    }
    if (!is.na(column_map[["suggested_next_alpha"]])) {
      out$Uploaded_Suggested_Next_Alpha <- suppressWarnings(as.numeric(get_col("suggested_next_alpha")))
    }
    out <- out[nzchar(out$Cohort) | nzchar(out$Patient) | is.finite(out$Administered_Dose) | is.finite(out$DLT), , drop = FALSE]
    out <- out[seq_len(min(nrow(out), 200L)), , drop = FALSE]
    if (nrow(out) == 0L) {
      stop("Upload did not contain any patient rows.", call. = FALSE)
    }
    out
  }
  
  observeEvent(input$conduct_workbook_upload, {
    req(input$conduct_workbook_upload)
    conduct_upload_error(NULL)
    design_check <- tryCatch(
      parse_trial_design_sheet(input$conduct_workbook_upload$datapath),
      error = function(e) e
    )
    if (inherits(design_check, "error")) {
      conduct_upload_error(conditionMessage(design_check))
      return(invisible(NULL))
    }
    uploaded <- tryCatch({
      ext <- tolower(tools::file_ext(input$conduct_workbook_upload$name))
      if (identical(ext, "csv")) {
        read.csv(input$conduct_workbook_upload$datapath, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
      } else if (ext %in% c("xls", "xlsx", "xlsm")) {
        workbook_sheets <- readxl::excel_sheets(input$conduct_workbook_upload$datapath)
        data_sheet <- workbook_sheets[tolower(trimws(workbook_sheets)) == "data input"]
        sheet_to_read <- if (length(data_sheet) > 0L) data_sheet[1] else workbook_sheets[1]
        as.data.frame(
          readxl::read_excel(input$conduct_workbook_upload$datapath, sheet = sheet_to_read, col_names = FALSE, .name_repair = "minimal"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        stop("Upload must be a CSV, XLS, XLSX, or XLSM file.", call. = FALSE)
      }
    }, error = function(e) e)
    if (inherits(uploaded, "error")) {
      conduct_upload_error(conditionMessage(uploaded))
      return(invisible(NULL))
    }
    normalized <- tryCatch(normalize_conduct_upload(uploaded), error = function(e) e)
    if (inherits(normalized, "error")) {
      conduct_upload_error(conditionMessage(normalized))
      return(invisible(NULL))
    }
    conduct_uploaded_rows(normalized)
    conduct_upload_edit_mode(FALSE)
    conduct_row_count(nrow(normalized))
    conduct_eval_requested(FALSE)
  })
  
  output$download_conduct_template <- downloadHandler(
    filename = function() "conduct_template.csv",
    content = function(file) {
      template <- data.frame(
        Cohort              = c(1L, 1L, 2L),
        Patient             = c(1L, 2L, 3L),
        `Dose Given`        = c(NA_real_, NA_real_, NA_real_),
        `DLT Status`        = c("No DLT", "DLT", "Unevaluable"),
        `Dose Recommended`  = c(NA_real_, NA_real_, NA_real_),
        "\u03b1" = c(NA_real_, NA_real_, NA_real_),
        check.names         = FALSE
      )
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  conduct_manual_row_count <- function() {
    input_values <- reactiveValuesToList(input, all.names = TRUE)
    input_names <- names(input_values)
    input_matches <- regmatches(
      input_names,
      regexec("^conduct_(?:cohort|patient|rec_dose|admin_dose|dlt|alpha)_([0-9]+)$", input_names)
    )
    input_indices <- suppressWarnings(as.integer(vapply(
      input_matches,
      function(match_item) if (length(match_item) >= 2L) match_item[2] else NA_character_,
      character(1)
    )))
    snapshot_rows <- conduct_rows_from_snapshot()
    max(
      c(
        conduct_row_count(),
        suppressWarnings(as.integer(input$conduct_row_count_client)),
        input_indices,
        if (!is.null(snapshot_rows)) nrow(snapshot_rows) else NA_integer_,
        1L
      ),
      na.rm = TRUE
    )
  }
  
  conduct_input_rows <- reactive({
    n <- conduct_manual_row_count()
    uploaded <- conduct_uploaded_rows()
    conduct_design <- conduct_current_design()
    dose_given_header <- if (nzchar(conduct_design$dose_unit) && length(unique(conduct_design$dose_units)) <= 1L) paste0("Dose Given (", conduct_design$dose_unit, ")") else "Dose Given"
    dose_rec_header <- if (nzchar(conduct_design$dose_unit) && length(unique(conduct_design$dose_units)) <= 1L) paste0("Dose Recommended (", conduct_design$dose_unit, ")") else "Dose Recommended"
    entry_mode <- input$conduct_design_entry_mode
    if (identical(entry_mode, "upload")) {
      if (isTRUE(conduct_upload_edit_mode())) {
        snapshot_rows <- conduct_rows_from_snapshot()
        if (conduct_rows_have_content(snapshot_rows)) {
          return(snapshot_rows)
        }
      } else {
        if (is.null(uploaded)) {
          return(data.frame(
            Cohort = character(0),
            Patient = character(0),
            Administered_Dose = numeric(0),
            DLT = integer(0),
            Is_Unevaluable = logical(0),
            Uploaded_Recommended_Dose = numeric(0),
            Uploaded_Alpha_Used = numeric(0),
            Uploaded_Suggested_Next_Alpha = numeric(0),
            check.names = FALSE
          ))
        }
        rows <- uploaded
        if (!"Is_Unevaluable" %in% names(rows)) rows$Is_Unevaluable <- FALSE
        if (!"Uploaded_Recommended_Dose" %in% names(rows)) rows$Uploaded_Recommended_Dose <- NA_real_
        if (!"Uploaded_Alpha_Used" %in% names(rows)) rows$Uploaded_Alpha_Used <- NA_real_
        if (!"Uploaded_Suggested_Next_Alpha" %in% names(rows)) rows$Uploaded_Suggested_Next_Alpha <- NA_real_
        return(rows)
      }
    }
    snapshot_rows <- conduct_rows_from_snapshot()
    if (conduct_rows_have_content(snapshot_rows)) {
      return(snapshot_rows)
    }
    data.frame(
      Cohort = vapply(seq_len(n), function(i) {
        default <- if (!is.null(uploaded) && i <= nrow(uploaded)) uploaded$Cohort[i] else i
        conduct_text_value(input[[paste0("conduct_cohort_", i)]], default)
      }, character(1)),
      Patient = vapply(seq_len(n), function(i) {
        default <- if (!is.null(uploaded) && i <= nrow(uploaded)) uploaded$Patient[i] else i
        conduct_text_value(input[[paste0("conduct_patient_", i)]], default)
      }, character(1)),
      Administered_Dose = vapply(seq_len(n), function(i) {
        default <- if (!is.null(uploaded) && i <= nrow(uploaded)) uploaded$Administered_Dose[i] else NA_real_
        conduct_numeric_value(input[[paste0("conduct_admin_dose_", i)]], default)
      }, numeric(1)),
      DLT = vapply(seq_len(n), function(i) {
        raw <- as.character(input[[paste0("conduct_dlt_", i)]])
        if (identical(raw, "Unevaluable")) return(NA_integer_)
        default <- if (!is.null(uploaded) && i <= nrow(uploaded)) uploaded$DLT[i] else NA_integer_
        conduct_integer_value(parse_conduct_dlt(raw), default)
      }, integer(1)),
      Is_Unevaluable = vapply(seq_len(n), function(i) {
        identical(as.character(input[[paste0("conduct_dlt_", i)]]), "Unevaluable")
      }, logical(1)),
      Uploaded_Recommended_Dose = vapply(seq_len(n), function(i) {
        if (!is.null(uploaded) && "Uploaded_Recommended_Dose" %in% names(uploaded) && i <= nrow(uploaded)) {
          value <- suppressWarnings(as.numeric(uploaded$Uploaded_Recommended_Dose[i]))
          if (is.finite(value)) return(value)
        }
        value <- conduct_numeric_value(input[[paste0("conduct_rec_dose_", i)]])
        if (is.finite(value)) return(value)
        NA_real_
      }, numeric(1)),
      Uploaded_Alpha_Used = vapply(seq_len(n), function(i) {
        if (!is.null(uploaded) && "Uploaded_Alpha_Used" %in% names(uploaded) && i <= nrow(uploaded)) {
          value <- suppressWarnings(as.numeric(uploaded$Uploaded_Alpha_Used[i]))
          if (is.finite(value)) return(value)
        }
        value <- suppressWarnings(as.numeric(input[[paste0("conduct_alpha_", i)]]))
        if (is.finite(value)) return(value)
        NA_real_
      }, numeric(1)),
      Uploaded_Suggested_Next_Alpha = vapply(seq_len(n), function(i) {
        if (!is.null(uploaded) && "Uploaded_Suggested_Next_Alpha" %in% names(uploaded) && i <= nrow(uploaded)) {
          value <- suppressWarnings(as.numeric(uploaded$Uploaded_Suggested_Next_Alpha[i]))
          if (is.finite(value)) return(value)
        }
        NA_real_
      }, numeric(1)),
      check.names = FALSE
    )
  })
  
  blank_conduct_row <- function() {
    data.frame(
      Cohort = "",
      Patient = "",
      Administered_Dose = NA_real_,
      DLT = NA_integer_,
      Is_Unevaluable = FALSE,
      Uploaded_Recommended_Dose = NA_real_,
      Uploaded_Alpha_Used = NA_real_,
      Uploaded_Suggested_Next_Alpha = NA_real_,
      check.names = FALSE
    )
  }
  
  conduct_rows_from_snapshot <- function(raw_snapshot = input$conduct_manual_snapshot) {
    if (!is.character(raw_snapshot) || length(raw_snapshot) != 1L || !nzchar(raw_snapshot)) {
      return(NULL)
    }
    parsed <- tryCatch(
      jsonlite::fromJSON(raw_snapshot, simplifyDataFrame = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed) || nrow(parsed) == 0L) {
      return(NULL)
    }
    get_snapshot_col <- function(name) {
      if (name %in% names(parsed)) parsed[[name]] else rep("", nrow(parsed))
    }
    raw_dlt <- trimws(as.character(get_snapshot_col("DLT_Status")))
    data.frame(
      Cohort = trimws(as.character(get_snapshot_col("Cohort"))),
      Patient = trimws(as.character(get_snapshot_col("Patient"))),
      Administered_Dose = vapply(get_snapshot_col("Administered_Dose"), conduct_numeric_value, numeric(1)),
      DLT = vapply(raw_dlt, parse_conduct_dlt, integer(1)),
      Is_Unevaluable = vapply(raw_dlt, is_unevaluable_dlt_str, logical(1)),
      DLT_Status_Text = trimws(as.character(raw_dlt)),
      Uploaded_Recommended_Dose = vapply(get_snapshot_col("Recommended_Dose"), conduct_numeric_value, numeric(1)),
      Uploaded_Alpha_Used = suppressWarnings(as.numeric(get_snapshot_col("Alpha_Used"))),
      Uploaded_Suggested_Next_Alpha = NA_real_,
      check.names = FALSE
    )
  }
  
  conduct_rows_have_content <- function(rows) {
    if (is.null(rows) || nrow(rows) == 0L) {
      return(FALSE)
    }
    has_recommended <- "Uploaded_Recommended_Dose" %in% names(rows) && any(is.finite(rows$Uploaded_Recommended_Dose), na.rm = TRUE)
    has_alpha <- "Uploaded_Alpha_Used" %in% names(rows) && any(is.finite(rows$Uploaded_Alpha_Used), na.rm = TRUE)
    any(
      nzchar(trimws(as.character(rows$Cohort))) |
        nzchar(trimws(as.character(rows$Patient))) |
        is.finite(rows$Administered_Dose) |
        is.finite(rows$DLT),
      na.rm = TRUE
    ) || has_recommended || has_alpha
  }
  
  preserve_conduct_manual_rows <- function() {
    rows <- conduct_rows_from_snapshot()
    if (is.null(rows)) {
      rows <- conduct_input_rows()
    }
    if (is.null(rows) || nrow(rows) == 0L) {
      rows <- blank_conduct_row()
      rows <- rows[0, , drop = FALSE]
    }
    rows$Uploaded_Recommended_Dose <- if ("Uploaded_Recommended_Dose" %in% names(rows)) rows$Uploaded_Recommended_Dose else NA_real_
    rows$Uploaded_Alpha_Used <- if ("Uploaded_Alpha_Used" %in% names(rows)) rows$Uploaded_Alpha_Used else NA_real_
    rows$Uploaded_Suggested_Next_Alpha <- if ("Uploaded_Suggested_Next_Alpha" %in% names(rows)) rows$Uploaded_Suggested_Next_Alpha else NA_real_
    rows
  }
  
  observeEvent(input$conduct_row_command, {
    command <- tryCatch(
      jsonlite::fromJSON(input$conduct_row_command, simplifyDataFrame = TRUE),
      error = function(e) NULL
    )
    rows <- NULL
    if (!is.null(command) && "rows" %in% names(command)) {
      rows <- conduct_rows_from_snapshot(jsonlite::toJSON(command$rows, auto_unbox = TRUE))
    }
    if (is.null(rows)) {
      rows <- preserve_conduct_manual_rows()
    }
    action <- if (!is.null(command) && "action" %in% names(command)) as.character(command$action) else ""
    if (identical(action, "add") && nrow(rows) < 200L) {
      rows <- rbind(rows, blank_conduct_row())
    } else if (identical(action, "delete") && nrow(rows) > 1L) {
      rows <- rows[-nrow(rows), , drop = FALSE]
    }
    conduct_uploaded_rows(rows)
    conduct_upload_error(NULL)
    conduct_row_count(max(nrow(rows), 1L))
  })
  
  observeEvent(input$conduct_delete_uploaded_data, {
    conduct_uploaded_rows(NULL)
    conduct_design_uploaded(NULL)
    conduct_design_upload_error(NULL)
    conduct_upload_edit_mode(FALSE)
    conduct_upload_error(NULL)
    conduct_eval_requested(FALSE)
    conduct_row_count(1L)
    updateCheckboxInput(session, "conduct_verify_addon", value = FALSE)
    session$sendCustomMessage("clearConductUploadInput", list(id = "conduct_workbook_upload"))
  })
  
  observeEvent(input$conduct_reset_data, {
    conduct_uploaded_rows(NULL)
    conduct_upload_edit_mode(FALSE)
    conduct_upload_error(NULL)
    conduct_eval_requested(FALSE)
    conduct_final_mtd_result_val(NULL)
    conduct_final_mtd_status_val(list(state = "idle", message = ""))
    conduct_row_count(1L)
    conduct_reset_token(conduct_reset_token() + 1L)
    updateCheckboxInput(session, "conduct_verify_addon", value = FALSE)
    updateRadioButtons(session, "conduct_trial_completed", selected = "No")
    session$sendCustomMessage("clearConductManualSnapshot", list())
    session$sendCustomMessage("clearRunButtonClicked", list(id = "evaluateConduct"))
    session$sendCustomMessage("clearRunButtonClicked", list(id = "runConductFinalMtd"))
  }, ignoreInit = TRUE)
  
  
  output$conduct_patient_rows <- renderUI({
    conduct_reset_token()
    entry_mode <- input$conduct_design_entry_mode
    n <- conduct_row_count()
    conduct_design <- conduct_current_design()
    dose_given_header <- if (nzchar(conduct_design$dose_unit) && length(unique(conduct_design$dose_units)) <= 1L) paste0("Dose Given (", conduct_design$dose_unit, ")") else "Dose Given"
    dose_rec_header <- if (nzchar(conduct_design$dose_unit) && length(unique(conduct_design$dose_units)) <= 1L) paste0("Dose Recommended (", conduct_design$dose_unit, ")") else "Dose Recommended"
    if (identical(conduct_design$dose_type, "Continuous") &&
        is.finite(conduct_design$min_dose) && is.finite(conduct_design$max_dose) &&
        conduct_design$max_dose > conduct_design$min_dose) {
      dose_min <- conduct_design$min_dose
      dose_max <- conduct_design$max_dose
      dose_step <- max((dose_max - dose_min) / 100, 0.01)
    } else if (length(conduct_design$actual_dose_levels) > 0L && all(is.finite(conduct_design$actual_dose_levels))) {
      dose_min <- min(conduct_design$actual_dose_levels)
      dose_max <- max(conduct_design$actual_dose_levels)
      dose_step <- "any"
    } else {
      dose_min <- NA_real_
      dose_max <- NA_real_
      dose_step <- "any"
    }
    uploaded <- conduct_uploaded_rows()
    upload_error <- conduct_upload_error()
    upload_notice <- if (is.character(upload_error) && nzchar(upload_error)) {
      div(
        style = "padding: 10px 12px; border: 1px solid #de8f8f; border-radius: 8px; background-color: #fdecec; color: #8b1e1e; margin-bottom: 12px;",
        upload_error
      )
    } else {
    }
    upload_editing <- identical(entry_mode, "upload") && isTRUE(conduct_upload_edit_mode())
    if (identical(entry_mode, "upload") && !upload_editing) {
      uploaded_n <- if (is.null(uploaded)) 0L else nrow(uploaded)
      if (uploaded_n == 0L) {
        return(tagList(
          upload_notice,
          div(style = "padding: 10px 12px; border: 1px solid #d9d9d9; border-radius: 8px; background-color: #fafafa; color: #555; margin-top: 12px;",
              "Upload a trial conduct file to populate patient rows.")
        ))
      }
      ewoc_res  <- if (conduct_has_result()) tryCatch(conduct_ewoc_result(), error = function(e) NULL) else NULL
      res_rows  <- if (!is.null(ewoc_res) && identical(ewoc_res$status, "ok") && !is.null(ewoc_res$rows)) ewoc_res$rows else NULL
      tbl_rows  <- lapply(seq_len(uploaded_n), function(i) {
        row      <- uploaded[i, , drop = FALSE]
        dlt_lbl  <- if (isTRUE(row$Is_Unevaluable)) "Unevaluable" else if (isTRUE(row$DLT == 1L)) "DLT" else "No DLT"
        rec      <- if (!is.null(res_rows) && i <= nrow(res_rows) && conduct_is_finite_scalar(res_rows$Recommended_Dose[i])) {
          res_rows$Recommended_Dose[i]
        } else if ("Uploaded_Recommended_Dose" %in% names(row) && conduct_is_finite_scalar(row$Uploaded_Recommended_Dose)) {
          row$Uploaded_Recommended_Dose
        } else {
          NA_real_
        }
        alp      <- if (!is.null(res_rows) && i <= nrow(res_rows) && conduct_is_finite_scalar(res_rows$Alpha_Used[i])) {
          res_rows$Alpha_Used[i]
        } else if ("Uploaded_Alpha_Used" %in% names(row) && conduct_is_finite_scalar(row$Uploaded_Alpha_Used)) {
          row$Uploaded_Alpha_Used
        } else {
          NA_real_
        }
        tags$tr(
          tags$td(as.character(row$Cohort)),
          tags$td(as.character(row$Patient)),
          tags$td(if (conduct_is_finite_scalar(row$Administered_Dose)) as.character(row$Administered_Dose) else "\u2014"),
          tags$td(dlt_lbl),
          tags$td(if (conduct_is_finite_scalar(rec)) as.character(rec) else "\u2014"),
          tags$td(if (conduct_is_finite_scalar(alp)) as.character(round(alp, 4)) else "\u2014")
        )
      })
      status_div <- if (!is.null(res_rows)) {
        div(style = "color: #256b38; font-size: 12px; margin: 4px 0 6px 0;",
            paste0(uploaded_n, " row(s) loaded and evaluated. See results on the right."))
      } else {
        div(style = "color: #256b38; font-size: 12px; margin: 4px 0 6px 0;",
            paste0(uploaded_n, " row(s) loaded. Click Evaluate Next Dose Recommendation to calculate dose recommendation for next cohort."))
      }
      return(tagList(
        upload_notice,
        status_div,
        tags$table(
          id = "conduct_patient_table",
          tags$thead(tags$tr(
            tags$th("Cohort"), tags$th("Patient"), tags$th(dose_given_header),
            tags$th("DLT Status"), tags$th(dose_rec_header), tags$th("\u03b1")
          )),
          tags$tbody(tbl_rows)
        ),
        div(
          style = "display: flex; gap: 10px; flex-wrap: wrap; margin-top: 12px; margin-bottom: 12px;",
          actionButton("conduct_delete_uploaded_data", "Clear Workbook", class = "btn btn-danger")
        )
      ))
    }
    table_rows <- lapply(seq_len(n), function(i) {
      uploaded_row <- if (!is.null(uploaded) && i <= nrow(uploaded)) uploaded[i, , drop = FALSE] else NULL
      cohort_value <- if (!is.null(uploaded_row)) {
        conduct_text_value(uploaded_row$Cohort, "")
      } else {
        conduct_text_value(NULL, i)
      }
      patient_value <- if (!is.null(uploaded_row)) {
        conduct_text_value(uploaded_row$Patient, "")
      } else {
        conduct_text_value(NULL, i)
      }
      selected_dose <- if (!is.null(uploaded_row) && conduct_is_finite_scalar(uploaded_row$Administered_Dose)) uploaded_row$Administered_Dose else NA_real_
      selected_dlt <- if (!is.null(uploaded_row) && isTRUE(uploaded_row$Is_Unevaluable)) {
        "Unevaluable"
      } else if (!is.null(uploaded_row) && conduct_is_finite_scalar(uploaded_row$DLT) && uploaded_row$DLT == 1L) {
        "DLT"
      } else if (!is.null(uploaded_row) && conduct_is_finite_scalar(uploaded_row$DLT) && uploaded_row$DLT == 0L) {
        "No DLT"
      } else {
        ""
      }
      rec_dose_value <- if (
        !is.null(uploaded_row) &&
        "Uploaded_Recommended_Dose" %in% names(uploaded_row) &&
        conduct_is_finite_scalar(uploaded_row$Uploaded_Recommended_Dose)
      ) {
        as.numeric(uploaded_row$Uploaded_Recommended_Dose)
      } else {
        NA_real_
      }
      alpha_value <- if (
        !is.null(uploaded_row) &&
        "Uploaded_Alpha_Used" %in% names(uploaded_row) &&
        conduct_is_finite_scalar(uploaded_row$Uploaded_Alpha_Used)
      ) {
        as.character(uploaded_row$Uploaded_Alpha_Used)
      } else if (i == 1L) {
        "NA"
      } else {
        ""
      }
      tags$tr(
        tags$td(textInput(paste0("conduct_cohort_", i), NULL, value = cohort_value)),
        tags$td(textInput(paste0("conduct_patient_", i), NULL, value = patient_value)),
        tags$td(numericInput(paste0("conduct_rec_dose_", i), NULL, value = rec_dose_value, min = dose_min, max = dose_max, step = dose_step)),
        tags$td(numericInput(paste0("conduct_admin_dose_", i), NULL, value = selected_dose, min = dose_min, max = dose_max, step = dose_step)),
        tags$td(selectInput(
          paste0("conduct_dlt_", i),
          NULL,
          choices = c("Select" = "", "No DLT", "DLT", "Unevaluable"),
          selected = selected_dlt,
          selectize = FALSE,
          width = "100%"
        )),
        tags$td(textInput(paste0("conduct_alpha_", i), NULL, value = alpha_value))
      )
    })
    tagList(
      upload_notice,
      if (upload_editing) {
        div(
          style = "padding: 10px 12px; border: 1px solid #84c49a; border-radius: 8px; background-color: #edf8f0; color: #256b38; margin-bottom: 12px;",
          "Uploaded data is now editable. Click Evaluate Next Dose Recommendation after making changes."
        )
      },
      tags$hr(style = "border-top: 1px solid #d9d9d9;"),
      h5("Patient Outcomes"),
      tags$div(
        style = "overflow-x: auto;",
        tags$table(
          id = "conduct_patient_table",
          tags$thead(
            tags$tr(
              tags$th("Cohort #"),
              tags$th("Patient #"),
              tags$th(dose_rec_header),
              tags$th(dose_given_header),
              tags$th("DLT Status"),
              tags$th(HTML("&alpha;"))
            )
          ),
          tags$tbody(do.call(tagList, table_rows))
        )
      ),
      div(
        style = "display: flex; gap: 10px; flex-wrap: wrap; margin-top: 12px; margin-bottom: 12px;",
        tags$button(id = "conduct_add_row_btn", type = "button", class = "btn btn-default", "Add Row"),
        tags$button(id = "conduct_delete_row_btn", type = "button", class = "btn btn-default", "Delete Last Row"),
        actionButton("conduct_reset_data", "Reset Trial Data", class = "btn btn-warning"),
        if (upload_editing) actionButton("conduct_delete_uploaded_data", "Clear Workbook", class = "btn btn-danger")
      )
    )
  })
  
  
  calculate_conduct_recommendations <- function(df) {
    conduct_design <- conduct_current_design()
    is_continuous_conduct <- identical(conduct_design$dose_type, "Continuous")
    num_doses <- conduct_design$num_doses
    startdose <- conduct_design$startdose
    theta <- suppressWarnings(as.numeric(input$conduct_theta))
    deltastop <- suppressWarnings(as.numeric(input$conduct_deltastop))
    lower_alpha <- suppressWarnings(as.numeric(input$conduct_lower_f_bound))
    increment <- suppressWarnings(as.numeric(input$conduct_increment))
    burn <- suppressWarnings(as.integer(input$conduct_burn))
    mm <- suppressWarnings(as.integer(input$conduct_mm))
    seed <- suppressWarnings(as.integer(input$conduct_seed))
    trial_mode <- normalize_trial_mode_input(input$conduct_trial_mode)
    max_accel_dose_level <- suppressWarnings(as.integer(input$conduct_max_accel_dose_level))
    entry_mode <- input$conduct_design_entry_mode
    conduct_opt1 <- conduct_opt1_val()
    
    if (!isTRUE(input$conduct_verify_addon)) {
      return(list(status = "error", message = "Please confirm that you reviewed the trial design information and current trial data before evaluating the dose recommendation."))
    }
    if (identical(input$conduct_design_entry_mode, "upload") && is.null(conduct_design_uploaded())) {
      return(list(status = "error", message = "Upload a valid Trial Conduct workbook before evaluating the dose recommendation."))
    }
    if (any(!is.finite(unlist(conduct_opt1))) || any(unlist(conduct_opt1) <= 0)) {
      return(list(status = "error", message = "Enter positive beta hyperparameters for a0, b0, a1, and b1."))
    }
    if (is_continuous_conduct) {
      if (!is.finite(conduct_design$min_dose) || !is.finite(conduct_design$max_dose) || conduct_design$max_dose <= conduct_design$min_dose) {
        return(list(status = "error", message = "Enter valid continuous minimum and maximum doses."))
      }
    } else {
      if (length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 2L) {
        return(list(status = "error", message = "Number of dose levels must be at least 2."))
      }
      if (length(startdose) != 1L || !is.finite(startdose) || startdose < 1L || startdose >= num_doses) {
        return(list(status = "error", message = "Starting dose must be between 1 and one dose below the highest dose."))
      }
      if (length(conduct_design$actual_dose_levels) != num_doses || any(!is.finite(conduct_design$actual_dose_levels))) {
        return(list(status = "error", message = "Enter one actual dose value for each categorical dose level."))
      }
      if (anyDuplicated(conduct_design$actual_dose_levels) > 0L) {
        return(list(status = "error", message = "Each categorical dose level needs a different actual dose value (it identifies the dose given)."))
      }
    }
    if (length(theta) != 1L || !is.finite(theta) || theta <= 0 || theta >= 1) {
      return(list(status = "error", message = "Target probability of DLT must be strictly between 0 and 1."))
    }
    if (length(deltastop) != 1L || !is.finite(deltastop) || deltastop <= 0 || deltastop >= 1) {
      return(list(status = "error", message = "Stopping threshold, Delta, must be strictly between 0 and 1."))
    }
    if (!is.finite(lower_alpha)) {
      return(list(status = "error", message = "Enter \u03b1."))
    }
    lower_alpha <- max(0, min(lower_alpha, 0.5))
    if (!is.finite(increment) || increment <= 0) {
      return(list(status = "error", message = "Enter a positive \u03b1 increment."))
    }
    if (!is.finite(burn) || burn < 0L) {
      return(list(status = "error", message = "Enter a non-negative burn-in value."))
    }
    if (!is.finite(mm) || mm < 1L) {
      return(list(status = "error", message = "Enter a positive number of MCMC samples."))
    }
    if (!is.finite(seed) || seed < 1L) {
      return(list(status = "error", message = "Enter a positive random seed."))
    }
    if (is.null(trial_mode) || is_continuous_conduct) trial_mode <- "Standard EWOC"
    if (identical(trial_mode, "Accelerated EWOC")) {
      if (!is.finite(max_accel_dose_level)) {
        return(list(status = "error", message = "Accelerated EWOC is selected. Please provide the Max Dose Level for Acceleration, or set Accelerated EWOC to No."))
      }
      if (max_accel_dose_level < startdose || max_accel_dose_level >= num_doses) {
        return(list(status = "error", message = "Max Dose Level for Acceleration must be a whole number from the Start Dose up to one level below the highest dose level."))
      }
    }
    if (nrow(df) == 0L) {
      return(list(status = "error", message = "Enter patient outcomes manually or upload a trial conduct file before evaluating the dose recommendation."))
    }
    
    alpha_increment_inputs <- validate_alpha_increment_cutoff(input, theta)
    if (is.null(alpha_increment_inputs)) {
      return(list(status = "error", message = "Custom toxicity bound must be greater than 0 and lower than theta."))
    }
    alpha_increment_cutoff <- alpha_increment_inputs$alpha_increment_cutoff
    rule_settings <- conduct_trial_rule_settings()
    rule_error <- conduct_trial_rule_settings_error(rule_settings, is_continuous_conduct)
    if (!is.null(rule_error)) {
      return(list(status = "error", message = rule_error))
    }
    settings <- list(
      theta = theta,
      deltastop = deltastop,
      lower_alpha = lower_alpha,
      increment = increment,
      alpha_increment_cutoff = alpha_increment_cutoff,
      burn = burn,
      mm = mm,
      seed = seed,
      trial_mode = trial_mode,
      max_accel_dose_level = max_accel_dose_level,
      cohort_size = rule_settings$cohort_size,
      total_sample_size = rule_settings$total_sample_size,
      efficiency_rule = rule_settings$efficiency_rule,
      num_patients_restrict = rule_settings$num_patients_restrict,
      efficiency_width = rule_settings$efficiency_width / 100
    )
    # Every cohort is re-evaluated from the start with the design rules (see conduct_evaluate_trial()).
    result <- tryCatch(
      conduct_evaluate_trial(
        rows = df,
        design = conduct_design,
        settings = settings,
        opt1 = conduct_opt1
      ),
      error = function(e) list(status = "error", message = paste("Trial Conduct evaluation failed:", conditionMessage(e)))
    )
    if (identical(result$status, "ok")) {
      result$alpha_increment_cutoff <- alpha_increment_cutoff
      result$use_custom_alpha_increment_cutoff <- alpha_increment_inputs$use_custom_alpha_increment_cutoff
      result$rule_settings <- rule_settings
      result$last_action <- if (!is.null(result$next_recommendation)) result$next_recommendation$action else "Not evaluated"
    }
    result
  }
  
  conduct_ewoc_result <- eventReactive(
    input$evaluateConduct,
    { calculate_conduct_recommendations(conduct_input_rows()) }
  )
  
  output$conduct_trial_completed_ui <- renderUI({
    div(
      style = "margin-top: 8px; padding: 10px 12px; border: 1px solid #cfd8dc; border-radius: 8px; background-color: #ffffff;",
      radioButtons(
        "conduct_trial_completed",
        "Is the trial completed?",
        choices = c("No", "Yes"),
        selected = "No",
        inline = TRUE
      )
    )
  })
  
  output$conduct_final_mtd_run_ui <- renderUI({
    req(identical(input$conduct_trial_completed, "Yes"))
    enabled <- isTRUE(input$conduct_verify_addon)
    actionButton(
      "runConductFinalMtd",
      "Run End-of-Trial Report",
      disabled = if (enabled) NULL else "disabled",
      style = if (enabled) {
        "margin-top: 8px; color: white; background-color: #28a745; border-color: #28a745; font-weight: bold;"
      } else {
        "margin-top: 8px; color: #777; background-color: #e9ecef; border-color: #ced4da; font-weight: bold; cursor: not-allowed;"
      }
    )
  })
  
  calculate_conduct_final_mtd <- function(result, progress_callback = NULL) {
    if (is.function(progress_callback)) progress_callback(0.10, "Preparing trial conduct data")
    rows <- result$rows
    if (is.null(rows) || nrow(rows) == 0L) {
      return(list(status = "error", message = "No evaluated trial conduct rows are available."))
    }
    conduct_design <- if (!is.null(result$conduct_design)) result$conduct_design else conduct_current_design()
    is_continuous_conduct <- identical(conduct_design$dose_type, "Continuous")
    num_doses <- conduct_design$num_doses
    theta <- suppressWarnings(as.numeric(input$conduct_theta))
    burn <- suppressWarnings(as.integer(input$conduct_burn))
    mm <- suppressWarnings(as.integer(input$conduct_mm))
    seed <- suppressWarnings(as.integer(input$conduct_seed))
    conduct_opt1 <- conduct_opt1_val()
    if (any(!is.finite(unlist(conduct_opt1))) || any(unlist(conduct_opt1) <= 0)) {
      return(list(status = "error", message = "Enter positive beta hyperparameters before calculating the end-of-trial summary."))
    }
    if (!is_continuous_conduct && (!is.finite(num_doses) || num_doses < 2L)) {
      return(list(status = "error", message = "Complete categorical dose values before calculating the end-of-trial summary."))
    }
    if (is_continuous_conduct && (!is.finite(conduct_design$min_dose) || !is.finite(conduct_design$max_dose) || conduct_design$max_dose <= conduct_design$min_dose)) {
      return(list(status = "error", message = "Complete continuous minimum and maximum doses before calculating the end-of-trial summary."))
    }
    if (!is.finite(theta) || theta <= 0 || theta >= 1) {
      return(list(status = "error", message = "Complete the trial design information before calculating the end-of-trial summary."))
    }
    if (!is.finite(burn) || burn < 0L || !is.finite(mm) || mm < 1L) {
      return(list(status = "error", message = "Enter valid burn-in and MCMC sample values before calculating the end-of-trial summary."))
    }
    is_unevaluable <- if ("Is_Unevaluable" %in% names(rows)) rows$Is_Unevaluable else rep(FALSE, nrow(rows))
    evaluable <- !is_unevaluable & is.finite(rows$Standardized_Dose_Given) & is.finite(rows$DLT)
    # Only rows used in the model (rows entered after a trial stop are excluded).
    if ("Used_In_Model" %in% names(rows)) evaluable <- evaluable & rows$Used_In_Model
    X <- rows$Standardized_Dose_Given[evaluable]
    Y <- as.integer(rows$DLT[evaluable])
    if (length(Y) == 0L) {
      return(list(status = "error", message = "At least one evaluable patient outcome is required before calculating the end-of-trial summary."))
    }
    if (is.function(progress_callback)) progress_callback(0.40, "Fitting final posterior model")
    doselevels <- if (is_continuous_conduct) seq(0, 1, by = 0.25) else conduct_design$doselevels
    model_string <- build_jags_model_string()
    # Fixed seed for a reproducible report, without changing the session's random-number state.
    ss <- tryCatch(
      with_local_seed(seed, fit_trial_posterior(
        model_string = model_string,
        Y = Y,
        X = X,
        opt1 = conduct_opt1,
        theta = theta,
        n = length(Y),
        chains = 1L,
        burn = burn,
        mm = mm
      )),
      error = function(e) e
    )
    if (inherits(ss, "error")) {
      return(list(status = "error", message = paste("Final posterior fitting failed:", conditionMessage(ss))))
    }
    mtd_draws <- ss$mtd[is.finite(ss$mtd)]
    if (length(mtd_draws) == 0L) {
      return(list(status = "error", message = "Final posterior fitting did not return valid MTD draws."))
    }
    if (is.function(progress_callback)) progress_callback(0.20, "Calculating posterior MTD summary")
    mtd_draws <- pmin(pmax(mtd_draws, 0), 1)
    # The trial outcome decides the final MTD, with the same rules as the simulation engines:
    # minimum dose too toxic (during the trial, or on the final data) -> no MTD; otherwise
    # the posterior median rounded to the nearest dose.
    stop_type <- if (!is.null(result$trial_stop) && !is.null(result$trial_stop$type)) result$trial_stop$type else NA_character_
    final_delta <- NULL
    if (is.na(stop_type)) {
      final_delta <- safety_stop_check(ss, theta, suppressWarnings(as.numeric(input$conduct_deltastop)))
      if (isTRUE(final_delta$stop)) stop_type <- "lowest_dose_too_toxic_final"
    }
    trial_outcome <- if (identical(stop_type, "lowest_dose_too_toxic")) {
      "Stopped for safety: minimum dose too toxic (no MTD)"
    } else if (identical(stop_type, "lowest_dose_too_toxic_final")) {
      paste0("Stopped for safety at the end of the trial: P(DLT at the minimum dose > theta) = ",
             format_conduct_value(final_delta$probability), " is greater than Delta (no MTD)")
    } else if (identical(stop_type, "efficiency")) {
      "Stopped for efficiency (MTD estimated from the data)"
    } else {
      "Completed"
    }
    if (startsWith(as.character(stop_type), "lowest_dose_too_toxic") %in% TRUE) stop_type <- "lowest_dose_too_toxic"
    posterior_median <- as.numeric(stats::median(mtd_draws, na.rm = TRUE))
    final_mtd_value <- if (identical(stop_type, "lowest_dose_too_toxic")) {
      NA_real_
    } else {
      posterior_median
    }
    final_mtd_actual <- conduct_actual_from_standardized(final_mtd_value, conduct_design)
    # Position of the final MTD among the dose levels (= its dose level).
    final_mtd_position <- if (is_continuous_conduct || !is.finite(final_mtd_value)) NA_integer_ else which.min(abs(doselevels - final_mtd_value))
    final_mtd_level <- final_mtd_position
    final_mtd_label <- if (is.finite(final_mtd_actual)) conduct_dose_label(final_mtd_actual, conduct_design$dose_unit, design = conduct_design) else "None"
    mtd_level_draw <- if (is_continuous_conduct) integer(0) else vapply(mtd_draws, function(z) {
      which.min(abs(doselevels - z))
    }, integer(1))
    posterior_mtd_prob <- if (is_continuous_conduct) numeric(0) else tabulate(mtd_level_draw, nbins = length(doselevels)) / length(mtd_level_draw)
    credible_interval <- as.numeric(stats::quantile(mtd_draws, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE))
    credible_interval_actual <- vapply(credible_interval, conduct_actual_from_standardized, numeric(1), design = conduct_design)
    probability_table <- data.frame(
      `Actual Dose` = vapply(doselevels, conduct_actual_from_standardized, numeric(1), design = conduct_design),
      `Standardized Dose` = round(doselevels, 4),
      `Posterior Pr(MTD closest to dose)` = if (is_continuous_conduct) NA_real_ else round(posterior_mtd_prob, 4),
      Selected = if (is_continuous_conduct) "" else ifelse(seq_along(doselevels) %in% final_mtd_position, "Yes", "No"),
      check.names = FALSE
    )
    if (is_continuous_conduct) {
      summary_table <- data.frame(
        Metric = c(
          "Trial outcome",
          "Final selected MTD",
          "Posterior median MTD (standardized)",
          "Posterior mean MTD",
          "95% credible interval for MTD"
        ),
        Value = c(
          trial_outcome,
          final_mtd_label,
          format_conduct_value(posterior_median),
          conduct_dose_label(conduct_actual_from_standardized(mean(mtd_draws, na.rm = TRUE), conduct_design), conduct_design$dose_unit),
          paste(conduct_dose_label(credible_interval_actual[1], conduct_design$dose_unit, design = conduct_design), "to", conduct_dose_label(credible_interval_actual[2], conduct_design$dose_unit, design = conduct_design))
        ),
        check.names = FALSE
      )
    } else {
      summary_table <- data.frame(
        Metric = c(
          "Trial outcome",
          "Final selected MTD",
          "Final selected MTD level",
          "Posterior median MTD (standardized)",
          "Posterior probability selected dose is MTD",
          "Posterior mean MTD",
          "95% credible interval for MTD"
        ),
        Value = c(
          trial_outcome,
          if (is.na(final_mtd_position)) "None" else conduct_dose_label(conduct_design$actual_dose_levels[final_mtd_position], conduct_design$dose_unit, design = conduct_design),
          if (is.na(final_mtd_level)) "None" else paste("Dose", final_mtd_level),
          format_conduct_value(posterior_median),
          if (is.na(final_mtd_position)) "NA" else format_conduct_value(posterior_mtd_prob[final_mtd_position]),
          conduct_dose_label(conduct_actual_from_standardized(mean(mtd_draws, na.rm = TRUE), conduct_design), conduct_design$dose_unit, design = conduct_design),
          paste(conduct_dose_label(credible_interval_actual[1], conduct_design$dose_unit, design = conduct_design), "to", conduct_dose_label(credible_interval_actual[2], conduct_design$dose_unit, design = conduct_design))
        ),
        check.names = FALSE
      )
    }
    # Pending DLT status: the MTD uses evaluable patients only; report pending patients and whether
    # they would trigger the efficiency rule.
    if (isTRUE(result$n_pending > 0L)) {
      summary_table <- rbind(summary_table, data.frame(
        Metric = "Warning: pending DLT status",
        Value = paste0(result$n_pending, " patient(s) still have a pending DLT result and are not included in this report."),
        check.names = FALSE
      ))
    }
    if (!is.null(result$pending_efficiency_warning)) {
      summary_table <- rbind(summary_table, data.frame(
        Metric = "Warning: efficiency rule (pending)",
        Value = result$pending_efficiency_warning,
        check.names = FALSE
      ))
    }
    if (!is.null(result$incomplete_cohorts) && nrow(result$incomplete_cohorts) > 0L) {
      summary_table <- rbind(summary_table, data.frame(
        Metric = "Warning: incomplete cohort data",
        Value = result$incomplete_cohorts$Message,
        check.names = FALSE
      ))
    }
    if (is.function(progress_callback)) progress_callback(0.20, "Preparing tables and plots")
    list(
      status = "ok",
      summary_table = summary_table,
      probability_table = probability_table,
      mtd_draws = mtd_draws,
      doselevels = doselevels,
      dose_level_labels = if (is_continuous_conduct) integer(0) else seq_along(doselevels),
      final_mtd_position = final_mtd_position,
      final_mtd_level = final_mtd_level,
      final_mtd_value = final_mtd_value,
      final_mtd_actual = final_mtd_actual,
      trial_outcome = trial_outcome,
      posterior_mtd_prob = posterior_mtd_prob,
      conduct_design = conduct_design
    )
  }
  
  conduct_final_mtd_result <- reactive({
    conduct_final_mtd_result_val()
  })
  
  observeEvent(input$runConductFinalMtd, {
    on.exit(session$sendCustomMessage("clearRunButtonClicked", list(id = "runConductFinalMtd")), add = TRUE)
    if (!identical(input$conduct_trial_completed, "Yes")) {
      conduct_final_mtd_status_val(list(state = "error", message = "Select Yes for trial completed before running the end-of-trial report."))
      return(invisible(NULL))
    }
    conduct_final_mtd_result_val(NULL)
    conduct_final_mtd_status_val(list(state = "running", message = "Starting end-of-trial report."))
    final_mtd <- tryCatch(
      withProgress(message = "Running end-of-trial report", value = 0, {
        progress_callback <- function(amount, detail) {
          conduct_final_mtd_status_val(list(state = "running", message = detail))
          incProgress(amount, detail = detail)
        }
        progress_callback(0.10, "Evaluating trial conduct rows")
        result <- calculate_conduct_recommendations(conduct_input_rows())
        if (!identical(result$status, "ok")) {
          return(result)
        }
        final_result <- calculate_conduct_final_mtd(result, progress_callback = progress_callback)
        if (identical(final_result$status, "ok")) {
          conduct_final_mtd_status_val(list(state = "complete", message = "End-of-trial report complete."))
          incProgress(0, detail = "Complete")
        } else {
          conduct_final_mtd_status_val(list(state = "error", message = final_result$message))
        }
        final_result
      }),
      error = function(e) {
        list(status = "error", message = paste("End-of-trial report failed:", conditionMessage(e)))
      }
    )
    if (!identical(final_mtd$status, "ok")) {
      conduct_final_mtd_status_val(list(state = "error", message = final_mtd$message))
    }
    conduct_final_mtd_result_val(final_mtd)
  }, ignoreInit = TRUE)
  
  observeEvent(input$evaluateConduct, {
    on.exit(session$sendCustomMessage("clearRunButtonClicked", list(id = "evaluateConduct")), add = TRUE)
    conduct_eval_requested(TRUE)
    conduct_final_mtd_result_val(NULL)
    conduct_final_mtd_status_val(list(state = "idle", message = ""))
    result <- conduct_ewoc_result()
    if (!identical(result$status, "ok") || is.null(result$rows)) return(invisible(NULL))
    rows <- result$rows
    for (i in seq_len(nrow(rows))) {
      rec <- rows$Recommended_Dose[i]
      alp <- rows$Alpha_Used[i]
      updateNumericInput(session, paste0("conduct_rec_dose_", i),
                         value = if (conduct_is_finite_scalar(rec)) rec else NA_real_)
      updateTextInput(session, paste0("conduct_alpha_", i),
                      value = if (conduct_is_finite_scalar(alp)) as.character(round(alp, 4)) else "NA")
    }
  })
  
  # Alpha shown as text: "NA" where no alpha is used (starting dose, accelerated lead-in).
  conduct_alpha_text <- function(values) {
    values <- suppressWarnings(as.numeric(values))
    ifelse(is.finite(values), format(round(values, 4), trim = TRUE, drop0trailing = TRUE), "NA")
  }

  conduct_display_rows <- function(rows) {
    if (is.null(rows) || nrow(rows) == 0L) {
      return(data.frame())
    }
    design <- conduct_current_design()
    dose_given_label <- if (nzchar(design$dose_unit) && length(unique(design$dose_units)) <= 1L) paste0("Dose Given (", design$dose_unit, ")") else "Dose Given"
    dose_rec_label <- if (nzchar(design$dose_unit) && length(unique(design$dose_units)) <= 1L) paste0("Dose Recommended (", design$dose_unit, ")") else "Dose Recommended"
    display <- data.frame(
      `Cohort #` = rows$Cohort,
      `Patient #` = rows$Patient,
      `Dose Recommended` = if ("Recommended_Dose" %in% names(rows)) rows$Recommended_Dose else rows$Uploaded_Recommended_Dose,
      `Dose Given` = rows$Administered_Dose,
      `Standardized Dose Given` = if ("Standardized_Dose_Given" %in% names(rows)) round(rows$Standardized_Dose_Given, 4) else NA_real_,
      `Standardized Dose Recommended` = if ("Recommended_Standardized_Dose" %in% names(rows)) round(rows$Recommended_Standardized_Dose, 4) else NA_real_,
      `DLT Status` = ifelse(
        "Is_Unevaluable" %in% names(rows) & isTRUE(length(rows$Is_Unevaluable) == nrow(rows)) & rows$Is_Unevaluable,
        "Unevaluable",
        ifelse(rows$DLT == 1L, "DLT", "No DLT")
      ),
      `Alpha` = conduct_alpha_text(if ("Alpha_Used" %in% names(rows)) rows$Alpha_Used else rows$Uploaded_Alpha_Used),
      check.names = FALSE
    ) |>
      stats::setNames(c("Cohort #", "Patient #", dose_rec_label, dose_given_label,
                        "Standardized Dose Given", "Standardized Dose Recommended", "DLT Status", "\u03b1"))
    if ("Dose_Range" %in% names(rows) && any(!is.na(rows$Dose_Range))) {
      display$`Dose Range` <- rows$Dose_Range
    }
    if ("Check_Recommended_Dose" %in% names(rows)) {
      display$`Recalculated Dose Recommended` <- rows$Check_Recommended_Dose
      display[["Recalculated \u03b1"]] <- conduct_alpha_text(rows$Check_Alpha)
      notes <- ifelse(rows$Recommended_Filled, "Dose recommended filled in", "")
      notes <- ifelse(rows$Alpha_Filled, ifelse(nzchar(notes), paste0(notes, "; \u03b1 filled in"), "\u03b1 filled in"), notes)
      notes <- ifelse(rows$Cohort_Filled, ifelse(nzchar(notes), paste0(notes, "; cohort filled in"), "Cohort filled in"), notes)
      display$`Filled In` <- notes
      display$Warning <- rows$Warning
    }
    display
  }
  
  # Downloaded Trial Conduct workbook: the same Data Input columns as the uploaded/template workbook
  # (Cohort, Patient, Dose Given, DLT Status, Dose Recommended, alpha) plus one Warning column giving the
  # reason for each row's warning. Blank rows are dropped; the last row is the next cohort to enrol.
  # attr(, "source_rows") gives each output row's row in result$rows (NA for the next-cohort row).
  build_conduct_download_table <- function(result) {
    rows <- result$rows
    if (is.null(rows) || nrow(rows) == 0L) {
      return(data.frame())
    }
    keep <- if ("Decision" %in% names(rows)) which(!(rows$Decision %in% "Empty row")) else seq_len(nrow(rows))
    rows <- rows[keep, , drop = FALSE]
    flag <- function(name) if (name %in% names(rows)) rows[[name]] %in% TRUE else rep(FALSE, nrow(rows))
    dlt_status <- ifelse(
      flag("Is_Unevaluable"),
      "Unevaluable",
      ifelse(is.finite(rows$DLT) & rows$DLT == 1L, "DLT", ifelse(is.finite(rows$DLT) & rows$DLT == 0L, "No DLT", ""))
    )
    recommended <- if ("Recommended_Dose" %in% names(rows)) rows$Recommended_Dose else rows$Uploaded_Recommended_Dose
    alpha_used <- if ("Alpha_Used" %in% names(rows)) rows$Alpha_Used else rows$Uploaded_Alpha_Used
    incomplete <- result$incomplete_cohorts
    incomplete_note <- function(i) {
      if (is.null(incomplete) || !rows$Cohort[i] %in% incomplete$Cohort) return(NULL)
      not_evaluable <- flag("Is_Unevaluable")[i] || ("Decision" %in% names(rows) && grepl("pending", rows$Decision[i]))
      if (!not_evaluable) return(NULL)
      j <- match(rows$Cohort[i], incomplete$Cohort)
      paste0("Incomplete cohort: ", incomplete$Evaluable[j], " of ", incomplete$Minimum[j],
             " evaluable - the next recommendation may change after pending results are entered")
    }
    warning_text <- vapply(seq_len(nrow(rows)), function(i) {
      reasons <- c(
        incomplete_note(i),
        if (flag("Cohort_Filled")[i]) paste0("Cohort was blank - filled as cohort ", rows$Cohort[i]),
        if (flag("Recommended_Filled")[i]) paste0("Dose Recommended was blank - filled with the recalculated dose (", format_conduct_value(recommended[i]), ")"),
        if (flag("Alpha_Filled")[i]) paste0("\u03b1 was blank - filled with the correct \u03b1 (", format_conduct_value(alpha_used[i]), ")"),
        if ("Warning" %in% names(rows) && nzchar(rows$Warning[i])) rows$Warning[i]
      )
      paste(reasons, collapse = "; ")
    }, character(1))
    out <- data.frame(
      Cohort = rows$Cohort,
      Patient = rows$Patient,
      `Dose Given` = rows$Administered_Dose,
      `DLT Status` = dlt_status,
      `Dose Recommended` = recommended,
      "\u03b1" = round(alpha_used, 4),
      Warning = warning_text,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    attr(out, "source_rows") <- keep
    recommendation <- result$next_recommendation
    if (is.null(recommendation)) {
      # No next cohort: the trial is complete, stopped, or more than N patients were entered.
      return(out)
    }
    is_stop <- identical(recommendation$action, "Stop Trial")
    next_row <- data.frame(
      Cohort = if (!is.null(recommendation$cohort)) sub("^Cohort ", "", recommendation$cohort) else "",
      Patient = "",
      `Dose Given` = NA_real_,
      `DLT Status` = "",
      `Dose Recommended` = if (is_stop) "Stop Trial" else if (is.finite(recommendation$dose)) as.character(recommendation$dose) else "",
      "\u03b1" = if (!is_stop && is.finite(result$alpha_for_next)) round(result$alpha_for_next, 4) else NA_real_,
      Warning = if (is_stop) {
        paste("Trial stopped:", recommendation$stop_reason)
      } else {
        paste(c(
          if (!is.null(result$incomplete_cohorts) && nrow(result$incomplete_cohorts) > 0L) {
            paste0("Based on incomplete cohort data (cohort ", paste(result$incomplete_cohorts$Cohort, collapse = ", "),
                   ") - may change after pending results are entered")
          },
          result$pending_efficiency_warning
        ), collapse = "; ")
      },
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    combined <- rbind(out, next_row)
    attr(combined, "source_rows") <- c(keep, NA_integer_)
    combined
  }

  output$conduct_status_ui <- renderUI({
    if (!conduct_has_result()) {
      if (identical(input$conduct_trial_completed, "Yes")) {
        return(NULL)
      }
      return(div(
        style = "padding: 12px; border: 1px solid #d9d9d9; border-radius: 8px; background-color: #fafafa;",
        "Enter patient rows or upload a CSV/XLS/XLSX file, then click Evaluate Next Dose Recommendation."
      ))
    }
    result <- conduct_ewoc_result()
    if (!identical(result$status, "ok")) {
      return(div(
        style = "padding: 12px; border: 1px solid #de8f8f; border-radius: 8px; background-color: #fdecec; color: #8b1e1e;",
        result$message
      ))
    }
    div(
      style = "padding: 12px; border: 1px solid #84c49a; border-radius: 8px; background-color: #edf8f0; color: #256b38;",
      paste0("Evaluated ", result$n_patients, " patient record(s); observed toxicity rate = ", round(result$observed_dlt_rate, 4), ".")
    )
  })
  
  output$conduct_data_preview <- renderTable({
    req(conduct_has_result())
    result <- conduct_ewoc_result()
    if (!is.null(result$rows)) {
      conduct_display_rows(result$rows)
    } else {
      conduct_display_rows(conduct_input_rows())
    }
  }, rownames = FALSE, na = "\u2014")
  
  output$conduct_posterior_table <- renderTable({
    req(conduct_has_result())
    result <- conduct_ewoc_result()
    req(identical(result$status, "ok"))
    req(!is.null(result$posterior_table))
    result$posterior_table
  }, rownames = FALSE)
  
  output$conduct_recommendation_ui <- renderUI({
    req(conduct_has_result())
    result <- conduct_ewoc_result()
    if (!identical(result$status, "ok")) {
      return(div(
        style = "border: 1px solid #de8f8f; border-radius: 8px; padding: 12px; background-color: #fdecec; color: #8b1e1e;",
        h4("Unable to Compute Recommendation"),
        tags$p(result$message)
      ))
    }
    recommendation <- result$next_recommendation
    design <- if (!is.null(result$conduct_design)) result$conduct_design else conduct_current_design()
    is_stop_recommendation <- !is.null(recommendation) && identical(recommendation$action, "Stop Trial")
    dose_text <- if (is.null(recommendation)) {
      "No next-cohort recommendation (see the messages below)."
    } else if (is_stop_recommendation && identical(recommendation$stop_type, "efficiency")) {
      "Stop Trial (efficiency)"
    } else if (is_stop_recommendation) {
      "Stop Trial (safety: lowest dose too toxic)"
    } else {
      paste0(
        if (!is.null(recommendation$cohort)) paste0(recommendation$cohort, " - ") else "",
        "Recommended dose: ", conduct_dose_label(recommendation$dose, design$dose_unit, design = design),
        if (!identical(recommendation$action, "Recommend Dose")) paste0(" (", recommendation$action, ")") else ""
      )
    }
    warning_list <- result$warnings
    tagList(
      div(
        style = if (is_stop_recommendation) {
          "border: 1px solid #de8f8f; border-radius: 8px; padding: 12px; background-color: #fdecec; color: #8b1e1e;"
        } else {
          "border: 1px solid #84c49a; border-radius: 8px; padding: 12px; background-color: #edf8f0; color: #256b38;"
        },
        h4("EWOC Recommendation"),
        tags$p(tags$strong(dose_text)),
        if (is_stop_recommendation && !is.null(recommendation$stop_reason)) tags$p(recommendation$stop_reason),
        if (is_stop_recommendation) tags$p("The trial does not continue after a stop. Run the end-of-trial report."),
        tags$p(paste0(
          "Evaluable patients (DLT result; used in the model, N and the efficiency rule): ", result$n_treated,
          if (is.null(result$rule_settings$total_sample_size)) "" else paste0(" of N = ", result$rule_settings$total_sample_size),
          "; pending DLT result: ", if (is.null(result$n_pending)) 0L else result$n_pending,
          "; unevaluable (replaced): ", if (is.null(result$n_unevaluable)) 0L else result$n_unevaluable, "."
        )),
        tags$p(paste("Observed toxicity rate:", round(result$observed_dlt_rate, 4))),
        if (!is_stop_recommendation && !is.null(recommendation)) tags$p(paste("\u03b1:", format_conduct_value(result$alpha_for_next)))
      ),
      if (!is.null(result$pending_efficiency_warning)) {
        div(
          style = "margin-top: 8px; border: 1px solid #e3a76f; border-radius: 8px; padding: 10px 12px; background-color: #fff3e6; color: #7a4100;",
          tags$strong("Efficiency rule with pending DLT status"),
          tags$p(style = "margin: 6px 0 0 0;", result$pending_efficiency_warning)
        )
      },
      if (!is.null(result$incomplete_cohorts) && nrow(result$incomplete_cohorts) > 0L) {
        div(
          style = "margin-top: 8px; border: 1px solid #e3a76f; border-radius: 8px; padding: 10px 12px; background-color: #fff3e6; color: #7a4100;",
          tags$strong("Incomplete cohort data"),
          tags$ul(style = "margin: 6px 0 0 0; padding-left: 18px;", lapply(result$incomplete_cohorts$Message, tags$li))
        )
      },
      if (length(result$messages)) {
        div(
          style = "margin-top: 8px; border: 1px solid #9fb8d6; border-radius: 8px; padding: 10px 12px; background-color: #eef4fb; color: #1f3d63;",
          tags$ul(style = "margin: 0; padding-left: 18px;", lapply(result$messages, tags$li))
        )
      },
      if (length(warning_list)) {
        div(
          style = "margin-top: 8px; border: 1px solid #e3a76f; border-radius: 8px; padding: 10px 12px; background-color: #fff3e6; color: #7a4100;",
          tags$strong(paste0("Please re-check the inputs (", length(warning_list), " warning", if (length(warning_list) > 1L) "s" else "", "):")),
          tags$ul(style = "margin: 6px 0 0 0; padding-left: 18px;", lapply(warning_list, tags$li)),
          tags$p(style = "margin: 6px 0 0 0;", "The recommendation above is calculated from the data and the design rules; the recalculated dose and alpha for each cohort are in the table and the downloaded workbook.")
        )
      }
    )
  })
  
  output$conduct_final_mtd_status_ui <- renderUI({
    req(identical(input$conduct_trial_completed, "Yes"))
    status <- conduct_final_mtd_status_val()
    if (is.null(input$runConductFinalMtd) || input$runConductFinalMtd == 0L || identical(status$state, "idle")) {
      return(NULL)
    }
    if (identical(status$state, "running")) {
      return(div(
        style = "margin-top: 10px; padding: 10px 12px; border: 1px solid #9fb7c9; border-radius: 8px; background-color: #f7fbff; color: #2f3a3f;",
        paste("End-of-trial report status:", status$message)
      ))
    }
    if (identical(status$state, "complete")) {
      return(div(
        style = "margin-top: 10px; padding: 10px 12px; border: 1px solid #84c49a; border-radius: 8px; background-color: #edf8f0; color: #256b38;",
        status$message
      ))
    }
    div(
      style = "margin-top: 10px; padding: 10px 12px; border: 1px solid #de8f8f; border-radius: 8px; background-color: #fdecec; color: #8b1e1e;",
      paste("End-of-trial report did not complete:", status$message)
    )
  })
  
  output$conduct_final_mtd_summary_ui <- renderUI({
    req(identical(input$conduct_trial_completed, "Yes"))
    final_mtd <- conduct_final_mtd_result()
    if (is.null(input$runConductFinalMtd) || input$runConductFinalMtd == 0L || is.null(final_mtd)) {
      return(div(
        style = "margin-top: 10px; border: 1px solid #d9d9d9; border-radius: 8px; padding: 12px; background-color: #fafafa; color: #555;",
        "Click Run End-of-Trial Report to calculate the end-of-trial summary from the evaluated trial data."
      ))
    }
    if (!identical(final_mtd$status, "ok")) {
      return(div(
        style = "margin-top: 10px; border: 1px solid #de8f8f; border-radius: 8px; padding: 12px; background-color: #fdecec; color: #8b1e1e;",
        h4("Unable to Compute End-of-Trial Report"),
        tags$p(final_mtd$message)
      ))
    }
    div(
      style = "margin-top: 10px; border: 1px solid #9fb7c9; border-radius: 8px; padding: 12px; background-color: #f7fbff;",
      h4("End-of-Trial Summary"),
      tableOutput("conduct_final_mtd_summary_table"),
      h4("Posterior MTD Probability by Dose"),
      tableOutput("conduct_final_mtd_probability_table"),
      fluidRow(
        column(6, plotOutput("conduct_final_mtd_probability_plot", height = "280px")),
        column(6, plotOutput("conduct_final_mtd_distribution_plot", height = "280px"))
      ),
      uiOutput("conduct_final_mtd_download_ui")
    )
  })
  
  output$conduct_final_mtd_summary_table <- renderTable({
    final_mtd <- conduct_final_mtd_result()
    req(identical(final_mtd$status, "ok"))
    final_mtd$summary_table
  }, rownames = FALSE)
  
  output$conduct_final_mtd_probability_table <- renderTable({
    final_mtd <- conduct_final_mtd_result()
    req(identical(final_mtd$status, "ok"))
    final_mtd$probability_table
  }, rownames = FALSE)
  
  build_conduct_final_mtd_probability_plot <- function(final_mtd) {
    if (length(final_mtd$posterior_mtd_prob) == 0L) {
      return(
        ggplot(final_mtd$probability_table, aes(x = `Standardized Dose`, y = `Actual Dose`)) +
          geom_line(color = "#2f3a3f", linewidth = 0.8) +
          geom_point(color = "#28a745", size = 2.2) +
          labs(x = "Standardized Dose", y = "Actual Dose", title = "Continuous Dose Scale") +
          theme_minimal(base_size = 12)
      )
    }
    level_labels <- if (length(final_mtd$dose_level_labels) == length(final_mtd$posterior_mtd_prob)) {
      final_mtd$dose_level_labels
    } else {
      seq_along(final_mtd$posterior_mtd_prob)
    }
    selected_position <- if (!is.null(final_mtd$final_mtd_position)) final_mtd$final_mtd_position else final_mtd$final_mtd_level
    plot_df <- data.frame(
      Dose_Level = factor(level_labels, levels = level_labels),
      Probability = final_mtd$posterior_mtd_prob,
      Selected = seq_along(final_mtd$posterior_mtd_prob) %in% selected_position
    )
    ggplot(plot_df, aes(x = Dose_Level, y = Probability, fill = Selected)) +
      geom_col(width = 0.68, color = "#2f3a3f") +
      geom_text(aes(label = paste0(round(100 * Probability, 1), "%")), vjust = -0.35, size = 3.4) +
      scale_fill_manual(values = c("FALSE" = "#9fb7c9", "TRUE" = "#28a745"), guide = "none") +
      scale_y_continuous(limits = c(0, max(0.05, min(1, max(plot_df$Probability, na.rm = TRUE) * 1.18)))) +
      labs(x = "Dose Level", y = "Posterior Probability", title = "Posterior Pr(MTD Closest to Dose)") +
      theme_minimal(base_size = 12)
  }
  
  build_conduct_final_mtd_distribution_plot <- function(final_mtd) {
    design <- if (!is.null(final_mtd$conduct_design)) final_mtd$conduct_design else conduct_current_design()
    plot_df <- data.frame(MTD = vapply(final_mtd$mtd_draws, conduct_actual_from_standardized, numeric(1), design = design))
    dose_df <- data.frame(Dose_Value = vapply(final_mtd$doselevels, conduct_actual_from_standardized, numeric(1), design = design))
    final_value <- conduct_actual_from_standardized(final_mtd$final_mtd_value, design)
    plot <- ggplot(plot_df, aes(x = MTD)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#9fb7c9", color = "white", alpha = 0.85) +
      geom_density(color = "#2f3a3f", linewidth = 0.8) +
      geom_vline(data = dose_df, aes(xintercept = Dose_Value), linetype = "dashed", color = "#7a8b94", linewidth = 0.45) +
      labs(x = "Posterior MTD", y = "Density", title = "Posterior MTD Distribution") +
      theme_minimal(base_size = 12)
    # No selected-MTD line when the trial stopped because the lowest dose was too toxic.
    if (is.finite(final_value)) plot <- plot + geom_vline(xintercept = final_value, color = "#28a745", linewidth = 1.1)
    plot
  }
  
  output$conduct_final_mtd_probability_plot <- renderPlot({
    final_mtd <- conduct_final_mtd_result()
    req(identical(final_mtd$status, "ok"))
    build_conduct_final_mtd_probability_plot(final_mtd)
  })
  
  output$conduct_final_mtd_distribution_plot <- renderPlot({
    final_mtd <- conduct_final_mtd_result()
    req(identical(final_mtd$status, "ok"))
    build_conduct_final_mtd_distribution_plot(final_mtd)
  })
  
  output$conduct_final_mtd_download_ui <- renderUI({
    final_mtd <- conduct_final_mtd_result()
    req(identical(final_mtd$status, "ok"))
    div(
      style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 8px;",
      downloadButton(
        "download_conduct_final_mtd_report",
        "Download end-of-trial report (Excel)",
        style = "font-weight: 700;"
      ),
      downloadButton(
        "download_conduct_final_mtd_word",
        "Download end-of-trial report (Word)",
        style = "font-weight: 700;"
      ),
      downloadButton(
        "download_conduct_final_mtd_html",
        "Download end-of-trial report (HTML)",
        style = "font-weight: 700;"
      )
    )
  })
  
  conduct_final_mtd_html_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }
  
  conduct_final_mtd_table_html <- function(df) {
    header <- paste0("<tr>", paste0("<th>", conduct_final_mtd_html_escape(names(df)), "</th>", collapse = ""), "</tr>")
    rows <- apply(df, 1, function(row_vals) {
      paste0("<tr>", paste0("<td>", conduct_final_mtd_html_escape(row_vals), "</td>", collapse = ""), "</tr>")
    })
    paste0("<table>", header, paste(rows, collapse = ""), "</table>")
  }
  
  conduct_final_mtd_base64 <- function(path) {
    if (requireNamespace("base64enc", quietly = TRUE)) {
      return(base64enc::base64encode(path))
    }
    NA_character_
  }
  
  conduct_final_mtd_plot_files <- function(final_mtd) {
    probability_plot <- tempfile(fileext = ".png")
    distribution_plot <- tempfile(fileext = ".png")
    ggplot2::ggsave(
      probability_plot,
      plot = build_conduct_final_mtd_probability_plot(final_mtd),
      width = 6,
      height = 4,
      dpi = 150,
      units = "in"
    )
    ggplot2::ggsave(
      distribution_plot,
      plot = build_conduct_final_mtd_distribution_plot(final_mtd),
      width = 6,
      height = 4,
      dpi = 150,
      units = "in"
    )
    list(probability_plot = probability_plot, distribution_plot = distribution_plot)
  }
  
  build_conduct_final_mtd_html_report <- function(final_mtd) {
    plot_files <- conduct_final_mtd_plot_files(final_mtd)
    probability_img <- conduct_final_mtd_base64(plot_files$probability_plot)
    distribution_img <- conduct_final_mtd_base64(plot_files$distribution_plot)
    plot_html <- if (!is.na(probability_img) && !is.na(distribution_img)) {
      paste0(
        "<div class='plots'>",
        "<figure><img src='data:image/png;base64,", probability_img, "' alt='Posterior MTD probability by dose'><figcaption>Posterior MTD Probability by Dose</figcaption></figure>",
        "<figure><img src='data:image/png;base64,", distribution_img, "' alt='Posterior MTD distribution'><figcaption>Posterior MTD Distribution</figcaption></figure>",
        "</div>"
      )
    } else {
      "<p class='note'>Plot images were not embedded because the base64enc package is not available.</p>"
    }
    paste0(
      "<!doctype html><html><head><meta charset='utf-8'><title>End-of-Trial Report</title>",
      "<style>body{font-family:Segoe UI,Arial,sans-serif;margin:28px;color:#1f2933;}h1{margin-bottom:4px;}h2{margin-top:24px;}table{border-collapse:collapse;margin-top:10px;min-width:520px;}th,td{border:1px solid #d8dee4;padding:8px 10px;text-align:left;}th{background:#edf8f0;} .meta{color:#52606d;margin-bottom:20px;} .plots{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:18px;margin-top:12px;}img{max-width:100%;border:1px solid #d8dee4;}figcaption{font-size:13px;color:#52606d;margin-top:6px;} .note{color:#7a5315;}@media(max-width:800px){.plots{grid-template-columns:1fr;}}</style>",
      "</head><body><h1>End-of-Trial Report</h1>",
      "<p class='meta'>Generated: ", conduct_final_mtd_html_escape(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "</p>",
      "<h2>End-of-Trial Summary</h2>",
      conduct_final_mtd_table_html(final_mtd$summary_table),
      "<h2>Posterior MTD Probability by Dose</h2>",
      conduct_final_mtd_table_html(final_mtd$probability_table),
      "<h2>Plots</h2>",
      plot_html,
      "</body></html>"
    )
  }
  
  output$download_conduct_final_mtd_report <- downloadHandler(
    filename = function() {
      paste0("trial_conduct_end_of_trial_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      final_mtd <- conduct_final_mtd_result()
      if (!identical(final_mtd$status, "ok")) {
        stop("End-of-trial report has not been calculated.", call. = FALSE)
      }
      workbook <- openxlsx::createWorkbook()
      posterior_draws_table <- data.frame(`Posterior MTD Draw` = final_mtd$mtd_draws, check.names = FALSE)
      report_sheets <- list(
        `End-of-Trial Summary` = final_mtd$summary_table,
        `MTD Probability` = final_mtd$probability_table,
        `Posterior MTD Draws` = posterior_draws_table
      )
      for (sheet_name in names(report_sheets)) {
        openxlsx::addWorksheet(workbook, sheet_name)
      }
      openxlsx::writeData(workbook, "End-of-Trial Summary", final_mtd$summary_table)
      openxlsx::writeData(workbook, "MTD Probability", final_mtd$probability_table)
      openxlsx::writeData(
        workbook,
        "Posterior MTD Draws",
        posterior_draws_table
      )
      header_style <- openxlsx::createStyle(
        fgFill = "#D9EAD3", textDecoration = "bold", border = "Bottom",
        halign = "center"
      )
      for (sheet_name in names(report_sheets)) {
        sheet_data <- report_sheets[[sheet_name]]
        openxlsx::addStyle(workbook, sheet_name, header_style, rows = 1, cols = seq_len(ncol(sheet_data)), gridExpand = TRUE)
        openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(ncol(sheet_data)), widths = "auto")
        openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
      }
      openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    }
  )
  
  output$download_conduct_final_mtd_word <- downloadHandler(
    filename = function() {
      paste0("trial_conduct_end_of_trial_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      final_mtd <- conduct_final_mtd_result()
      if (!identical(final_mtd$status, "ok")) {
        stop("End-of-trial report has not been calculated.", call. = FALSE)
      }
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("The officer package is required to create the Word report.", call. = FALSE)
      }
      plot_files <- conduct_final_mtd_plot_files(final_mtd)
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "End-of-Trial Report")
      doc <- officer::body_add_par(doc, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, "End-of-Trial Summary")
      doc <- officer::body_add_table(doc, final_mtd$summary_table)
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, "Posterior MTD Probability by Dose")
      doc <- officer::body_add_table(doc, final_mtd$probability_table)
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, "Posterior MTD Probability by Dose")
      doc <- officer::body_add_img(doc, src = plot_files$probability_plot, width = 6, height = 4)
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, "Posterior MTD Distribution")
      doc <- officer::body_add_img(doc, src = plot_files$distribution_plot, width = 6, height = 4)
      print(doc, target = file)
    }
  )
  
  output$download_conduct_final_mtd_html <- downloadHandler(
    filename = function() {
      paste0("trial_conduct_end_of_trial_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      final_mtd <- conduct_final_mtd_result()
      if (!identical(final_mtd$status, "ok")) {
        stop("End-of-trial report has not been calculated.", call. = FALSE)
      }
      writeLines(build_conduct_final_mtd_html_report(final_mtd), con = file, useBytes = TRUE)
    }
  )
  
  output$conduct_download_table_ui <- renderUI({
    req(conduct_has_result())
    result <- conduct_ewoc_result()
    req(identical(result$status, "ok"))
    downloadButton(
      "download_conduct_evaluated_table",
      "Download trial conduct table (Excel)",
      style = "font-weight: 700;"
    )
  })
  
  output$download_conduct_evaluated_table <- downloadHandler(
    filename = function() {
      paste0("trial_conduct_next_cohort_table_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      result <- conduct_ewoc_result()
      
      conduct_design <- if (!is.null(result$conduct_design)) result$conduct_design else conduct_current_design()
      trial_mode_value <- if (identical(conduct_design$dose_type, "Continuous")) "No" else if (identical(input$conduct_trial_mode, "Yes")) "Yes" else "No"
      max_accel_value <- if (identical(trial_mode_value, "Yes")) input$conduct_max_accel_dose_level else ""
      if (length(max_accel_value) != 1L || is.null(max_accel_value)) max_accel_value <- ""
      dose_metadata_table <- build_conduct_dose_metadata_table(
        dose_type = conduct_design$dose_type,
        dose_unit = conduct_design$dose_unit,
        min_dose = if (identical(conduct_design$dose_type, "Continuous")) conduct_design$min_dose else "",
        max_dose = if (identical(conduct_design$dose_type, "Continuous")) conduct_design$max_dose else "",
        actual_dose_levels = if (identical(conduct_design$dose_type, "Discrete")) conduct_design$actual_dose_levels else numeric(0),
        num_doses = if (identical(conduct_design$dose_type, "Discrete")) conduct_design$num_doses else "",
        startdose = if (identical(conduct_design$dose_type, "Discrete")) conduct_design$startdose else "",
        max_dose_increment = if (identical(conduct_design$dose_type, "Continuous")) conduct_design$max_dose_increment else "",
        actual_dose_units = if (identical(conduct_design$dose_type, "Discrete")) conduct_design$dose_units else character(0)
      )
      design_table <- rbind(
        data.frame(
          `Trial Design Parameter` = c("P(MTD < Min)", "P(MTD > Max)", "a0", "b0", "a1", "b1"),
          Value = c(input$conduct_p0, input$conduct_p1, input$conduct_a0, input$conduct_b0,
                    input$conduct_a1, input$conduct_b1),
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        dose_metadata_table,
        data.frame(
          `Trial Design Parameter` = c(
            "Target DLT Probability (theta)", "Stopping Threshold Delta",
            "\u03b1", "\u03b1 Increment", "Accelerated EWOC",
            "Maximum Accelerated Dose", "Seed", "Burn-in", "MCMC Samples"
          ),
          Value = c(
            input$conduct_theta, input$conduct_deltastop,
            input$conduct_lower_f_bound, input$conduct_increment,
            trial_mode_value, max_accel_value, input$conduct_seed, input$conduct_burn, input$conduct_mm
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        {
          rule_settings <- if (!is.null(result$rule_settings)) result$rule_settings else conduct_trial_rule_settings()
          conduct_trial_rule_rows(
            cohort_size = rule_settings$cohort_size,
            total_sample_size = rule_settings$total_sample_size,
            stopping_rule = rule_settings$efficiency_rule,
            num_patients_restrict = rule_settings$num_patients_restrict,
            efficiency_width = rule_settings$efficiency_width,
            dose_type = conduct_design$dose_type
          )
        }
      )

      data_table <- if (identical(result$status, "ok")) {
        tbl <- build_conduct_download_table(result)
        if (is.null(tbl) || ncol(tbl) == 0L) {
          data.frame(Message = "No patient rows available.", check.names = FALSE)
        } else {
          tbl
        }
      } else {
        data.frame(Message = "Trial Conduct recommendation has not been calculated.", check.names = FALSE)
      }

      workbook <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(workbook, "Trial Design")
      openxlsx::addWorksheet(workbook, "Data Input")
      openxlsx::writeData(workbook, "Trial Design", design_table)
      openxlsx::writeData(workbook, "Data Input", data_table)
      header_style <- openxlsx::createStyle(
        fgFill = "#D9EAD3", textDecoration = "bold", border = "Bottom",
        halign = "center"
      )
      openxlsx::addStyle(workbook, "Trial Design", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
      openxlsx::addStyle(workbook, "Data Input", header_style, rows = 1, cols = seq_len(ncol(data_table)), gridExpand = TRUE)
      # No alpha is used for the starting-dose and lead-in rows: write "NA" there (other alphas stay numeric).
      alpha_col <- match("\u03b1", names(data_table))
      if (is.finite(alpha_col)) {
        for (r in which(is.na(data_table[[alpha_col]]))) {
          openxlsx::writeData(workbook, "Data Input", "NA", startCol = alpha_col, startRow = r + 1L)
        }
      }
      # Highlight values the app filled in (yellow) and rows with a warning (orange).
      evaluated_rows <- if (identical(result$status, "ok")) result$rows else NULL
      if (!is.null(evaluated_rows) && "Recommended_Filled" %in% names(evaluated_rows)) {
        filled_style <- openxlsx::createStyle(fgFill = "#FFF2A8")
        warning_style <- openxlsx::createStyle(fgFill = "#F8CBAD", wrapText = TRUE)
        mark <- function(flags, column_name, style) {
          col_index <- match(column_name, names(data_table))
          row_index <- which(flags) + 1L
          if (is.finite(col_index) && length(row_index)) {
            openxlsx::addStyle(workbook, "Data Input", style, rows = row_index, cols = col_index, gridExpand = TRUE, stack = TRUE)
          }
        }
        # Output rows map to result rows through source_rows (blank rows dropped; NA = next cohort).
        source_rows <- attr(data_table, "source_rows")
        filled_flags <- function(column_name) {
          if (is.null(source_rows) || !column_name %in% names(evaluated_rows)) return(rep(FALSE, nrow(data_table)))
          evaluated_rows[[column_name]][source_rows] %in% TRUE
        }
        mark(filled_flags("Recommended_Filled"), "Dose Recommended", filled_style)
        mark(filled_flags("Alpha_Filled"), "\u03b1", filled_style)
        mark(filled_flags("Cohort_Filled"), "Cohort", filled_style)
        if ("Warning" %in% names(data_table)) {
          mark(nzchar(as.character(data_table$Warning)) %in% TRUE, "Warning", warning_style)
        }
      }
      openxlsx::setColWidths(workbook, "Trial Design", cols = 1, widths = 34)
      openxlsx::setColWidths(workbook, "Trial Design", cols = 2, widths = 18)
      openxlsx::setColWidths(workbook, "Data Input", cols = seq_len(ncol(data_table)), widths = "auto")
      openxlsx::freezePane(workbook, "Trial Design", firstRow = TRUE)
      openxlsx::freezePane(workbook, "Data Input", firstRow = TRUE)
      openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    }
  )
  
  for (conduct_row in seq_len(200L)) local({
    row_id <- conduct_row
    output[[paste0("conduct_recommended_dose_ui_", row_id)]] <- renderUI({
      result <- if (conduct_has_result()) conduct_ewoc_result() else NULL
      value <- if (!is.null(result) && !is.null(result$rows) && row_id <= nrow(result$rows)) {
        result$rows$Recommended_Dose[row_id]
      } else {
        rows <- conduct_input_rows()
        if (row_id <= nrow(rows)) rows$Uploaded_Recommended_Dose[row_id] else NA_real_
      }
      conduct_display_box(" ", if (conduct_is_finite_scalar(value)) format_conduct_value(value, digits = 0) else "\u2014")
    })
    output[[paste0("conduct_alpha_used_ui_", row_id)]] <- renderUI({
      result <- if (conduct_has_result()) conduct_ewoc_result() else NULL
      value <- if (!is.null(result) && !is.null(result$rows) && row_id <= nrow(result$rows)) {
        result$rows$Alpha_Used[row_id]
      } else {
        rows <- conduct_input_rows()
        if (row_id <= nrow(rows)) rows$Uploaded_Alpha_Used[row_id] else NA_real_
      }
      conduct_display_box(" ", if (conduct_is_finite_scalar(value)) format_conduct_value(value) else "NA")
    })
    output[[paste0("conduct_suggested_alpha_ui_", row_id)]] <- renderUI({
      result <- if (conduct_has_result()) conduct_ewoc_result() else NULL
      value <- if (!is.null(result) && !is.null(result$rows) && row_id <= nrow(result$rows)) {
        result$rows$Suggested_Next_Alpha[row_id]
      } else {
        rows <- conduct_input_rows()
        if (row_id <= nrow(rows)) rows$Uploaded_Suggested_Next_Alpha[row_id] else NA_real_
      }
      conduct_display_box(" ", if (conduct_is_finite_scalar(value)) format_conduct_value(value) else "NA")
    })
  })
  
  apply_trial_summary_outputs <- function(trial_summary) {
    trial_summary$combined_table <- round_trial_efficiency_percentages(trial_summary$combined_table)
    combined_table_val(trial_summary$combined_table)
    stopped_details_val(trial_summary$stopped_df)
    trial_report_val(trial_summary)
    trial_scenario_results_val(NULL)
    output$scenario_results_tabs <- renderUI(NULL)
    
    stopping_rule_selected <- isTRUE(trial_summary$inputs$stopping_rule_selected)
    trial_mode <- trial_summary$inputs$trial_mode
    pstop <- trial_summary$pstop
    pstop_delta <- trial_summary$pstop_delta
    avg_n_delta_ci_text <- trial_summary$avg_n_delta_ci_text
    avg_n_stop_rule_ci_text <- trial_summary$avg_n_stop_rule_ci_text
    accel_patients <- trial_summary$accel_patients
    accel_stop_reason <- trial_summary$accel_stop_reason
    ewoc_entry_dose <- trial_summary$ewoc_entry_dose
    overall_avg_n <- trial_summary$overall_avg_n
    overall_avg_n_ci_text <- trial_summary$overall_avg_n_ci_text
    combined_table <- trial_summary$combined_table
    dlt_summary <- trial_summary$dlt_summary
    
    output$eff_table1_block <- renderUI({
      req(combined_table)
      tagList(h4(tags$u("Table 1: Trial Efficiency Summary")),
              tableOutput("table1")
      )
    })
    output$dlt_table2_block <- renderUI({
      req(dlt_summary)
      tagList(h4(tags$u("Table 2: Safety Summary")),
              tableOutput("table2")
      )
    })
    output$stopping_summary_block <- renderUI({
      summary_metric_row <- function(label, value) {
        div(
          style = "display: flex; justify-content: space-between; gap: 12px; padding: 4px 0;",
          span(label),
          tags$strong(as.character(value))
        )
      }
      
      summary_card <- function(title, rows) {
        div(
          style = paste(
            "border: 1px solid #d9d9d9;",
            "border-radius: 8px;",
            "padding: 10px 12px;",
            "margin-bottom: 10px;",
            "background-color: #fafafa;"
          ),
          div(style = "font-weight: 600; margin-bottom: 6px;", title),
          rows
        )
      }
      
      cards <- list()
      if (stopping_rule_selected) {
        cards <- c(cards, list(
          summary_card(
            "Patient Allocation Stopping Rule Summary",
            tagList(
              summary_metric_row("% Trials stopped by maximum patient allocation restriction", round(pstop * 100, 3)),
              summary_metric_row("Average sample size of the stopped Trials (95% CI)", avg_n_stop_rule_ci_text)
            )
          )
        ))
      }
      
      cards <- c(cards, list(
        summary_card(
          "\u0394-Based Summary",
          tagList(
            summary_metric_row("Probability of stopping based on \u0394", round(pstop_delta, 3)),
            summary_metric_row("Average sample size of the stopped Trials (95% CI)", avg_n_delta_ci_text)
          )
        )
      ))
      
      if (identical(trial_mode, "Accelerated EWOC")) {
        accel_reason_values <- accel_stop_reason[!is.na(accel_stop_reason) & nzchar(accel_stop_reason)]
        accel_entry_values <- ewoc_entry_dose[is.finite(ewoc_entry_dose)]
        accel_reason <- if (length(accel_reason_values) > 0) names(sort(table(accel_reason_values), decreasing = TRUE))[1] else "Not available"
        accel_entry <- if (length(accel_entry_values) > 0) {
          entry_idx <- as.integer(names(sort(table(accel_entry_values), decreasing = TRUE))[1])
          dose_labels <- trial_summary$inputs$actual_dose_levels
          if (!is.null(dose_labels)) {
            # SK: prefer the per-dose-level units when available (matches the
            # dose count); falls back to the shared-unit-or-"" value for older
            # trial_summary$inputs that predate per-dose units.
            dose_units_for_labels <- trial_summary$inputs$actual_dose_units
            if (is.null(dose_units_for_labels) || length(dose_units_for_labels) != length(dose_labels)) {
              dose_units_for_labels <- trial_summary$inputs$dose_unit
            }
            dose_labels <- build_discrete_dose_labels(dose_labels, dose_units_for_labels, length(dose_labels))
          }
          if (!is.null(dose_labels) && entry_idx >= 1L && entry_idx <= length(dose_labels)) dose_labels[entry_idx] else paste("Dose", entry_idx)
        } else {
          "Not available"
        }
        cards <- c(cards, list(
          summary_card(
            "Accelerated Startup Summary",
            tagList(
              summary_metric_row("Mean accelerated singleton patients", round(mean(accel_patients, na.rm = TRUE), 3)),
              summary_metric_row("Most common accelerated stop reason", accel_reason),
              summary_metric_row("Most common EWOC entry dose", accel_entry)
            )
          )
        ))
      }
      
      accelerated_card <- if (identical(trial_mode, "Accelerated EWOC") && length(cards) > 0L) cards[[length(cards)]] else NULL
      tagList(
        h4(tags$u("Stopping summary")),
        table_tag_ui(trial_summary$stopping_summary_table),
        accelerated_card
      )
    })
    output$stopped_details_block <- renderUI({
      if (!is.null(stopped_details_val()) && nrow(stopped_details_val()) > 0L) {
        tagList(
          checkboxInput("show_stopped_details", "Show stopped trial details", value = FALSE),
          conditionalPanel(
            condition = "input.show_stopped_details == true",
            h4(tags$u("Stopped trial details")),
            verbatimTextOutput("stopped_trial_details"),
            downloadButton("download_stopped_trials_csv", "Download stopped trial details (CSV)")
          )
        )
      } else {
      }
    })
    
    output$table1 <- renderTable({ data.frame(combined_table, check.names = FALSE) }, rownames = FALSE)
    output$table2 <- renderTable({ data.frame(dlt_summary, check.names = FALSE) }, rownames = FALSE)
    output$stopped_trial_details <- renderText({
      stopped_df <- stopped_details_val()
      if (is.null(stopped_df) || nrow(stopped_df) == 0) {
        return("Stopping rule was not triggered in any trial.")
      }
      n_total <- nrow(stopped_df)
      n_show <- min(3, n_total)
      lines <- c(
        paste("Stopping rule triggered in", n_total, "trial(s)."),
        paste("Showing first", n_show, "trial(s) below. Download CSV for complete details."),
        ""
      )
      for (ii in seq_len(n_show)) {
        lines <- c(
          lines,
          paste0("Trial ", stopped_df$Trial[ii], ": Number of dose values created = ", stopped_df$N_Dose_Values[ii]),
          paste0("Dose values: ", stopped_df$Dose_Values[ii]),
          paste0("DLT values: ", stopped_df$DLT_Values[ii]),
          ""
        )
      }
      paste(lines, collapse = "\n")
    })
    output$download_stopped_trials_csv <- downloadHandler(
      filename = function() {
        paste0("stopped_trial_details_", Sys.Date(), ".csv")
      },
      content = function(file) {
        stopped_df <- stopped_details_val()
        if (is.null(stopped_df) || nrow(stopped_df) == 0) {
          write.csv(data.frame(Message = "No stopped trials"), file, row.names = FALSE)
        } else {
          write.csv(stopped_df, file, row.names = FALSE)
        }
      }
    )
  }
  
  apply_trial_scenario_outputs <- function(scenario_results) {
    if (is.null(scenario_results) || length(scenario_results) == 0) {
      return(invisible(NULL))
    }
    
    scenario_results <- lapply(scenario_results, function(item) {
      item$summary$combined_table <- round_trial_efficiency_percentages(item$summary$combined_table)
      item
    })
    trial_scenario_results_val(scenario_results)
    first_summary <- scenario_results[[1]]$summary
    combined_table_val(first_summary$combined_table)
    stopped_details_val(first_summary$stopped_df)
    trial_report_val(first_summary)
    
    output$scenario_results_tabs <- renderUI({
      req(trial_scenario_results_val())
      do.call(tabsetPanel, c(
        list(type = "tabs"),
        lapply(trial_scenario_results_val(), function(item) {
          tabPanel(item$label, trial_summary_ui(item$summary))
        })
      ))
    })
    output$eff_table1_block <- renderUI(NULL)
    output$dlt_table2_block <- renderUI(NULL)
    output$stopping_summary_block <- renderUI(NULL)
    output$stopped_details_block <- renderUI(NULL)
    output$table1 <- renderTable(NULL)
    output$table2 <- renderTable(NULL)
    output$stopped_trial_details <- renderText(NULL)
    
    invisible(NULL)
  }
  
  output$NN_even_warning_ui <- renderUI({
    if(TRUE)
    {
      return(NULL)
    }
    
    NN <- suppressWarnings(as.integer(input$NN))
    
    if(length(NN) != 1L || !is.finite(NN))
    {
      return(NULL)
    }
    
    if(NN %% 2L != 0L)
    {
      tags$div(
        style = "color: #b26a00; font-size: 12px; margin-top: -8px; margin-bottom: 8px;",
        "Total Number of Patients must be an even number because cohort size is fixed at 2."
      )
    } else {
    }
  })
  
  # SK: "Submit" button on the Trial Setup tab. Lists exactly the fields that
  # are actually visible on that page right now (mirroring the conditionalPanel
  # conditions in the UI one-for-one, by prior_dose_type), and
  # reports back which of those are blank/unfilled -- NOT a full validity
  # check (ranges, cross-field consistency, etc. are still handled by the
  # existing per-tab validators when the user later runs Prior Calculations
  # or Trial Simulation). This only guards the one thing a "Submit" button on
  # a setup page needs to guard: did the user actually fill in every field
  # that's on screen before we send them on to the next page.
  trial_setup_missing_fields <- function(input) {
    missing <- character(0)
    
    is_blank_numeric <- function(value) {
      length(value) != 1L || !is.finite(suppressWarnings(as.numeric(value)))
    }
    is_blank_text <- function(value) {
      length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))
    }
    
    dose_type <- input$prior_dose_type
    is_discrete <- identical(dose_type, "Discrete")
    
    if (is_blank_text(dose_type)) missing <- c(missing, "Dose Type")
    if (is_blank_numeric(input$theta)) missing <- c(missing, "Target Toxicity (theta)")
    if (is_blank_numeric(input$NN)) missing <- c(missing, "Total Number of Patients")
    if (is_blank_numeric(input$cohort)) missing <- c(missing, "Cohort Size")
    
    # The overall "Dose Unit" field is only on screen for Continuous mode -- Discrete mode
    # collects a unit per dose level instead (checked in the per-dose-level loops below).
    if (!is_discrete) {
      if (is_blank_text(input$dose_unit)) missing <- c(missing, "Dose Unit")
    }
    
    if (is_discrete) {
      if (is_blank_numeric(input$num_doses)) missing <- c(missing, "Number of Dose Levels")
      if (is_blank_numeric(input$startdose)) missing <- c(missing, "Starting Dose Level")
      num_doses <- suppressWarnings(as.integer(input$num_doses))
      if (length(num_doses) == 1L && is.finite(num_doses) && num_doses >= 2L) {
        for (i in seq_len(min(num_doses, 12L))) {
          if (is_blank_numeric(input[[paste0("actual_dose_", i)]])) {
            missing <- c(missing, paste0("Dose Level ", i, " Actual Dose Value"))
          }
          if (is_blank_text(input[[paste0("actual_dose_unit_", i)]])) {
            missing <- c(missing, paste0("Dose Level ", i, " Unit"))
          }
        }
      }
    } else {
      if (is_blank_numeric(input$min_dose)) missing <- c(missing, "Minimum Dose Value")
      if (is_blank_numeric(input$max_dose)) missing <- c(missing, "Maximum Dose Value")
    }

    missing
  }
  
  # SK: no "success" status message here -- the user is navigated straight to
  # Prior Information on success, so they'd never see it fire in the first
  # place, and if it persisted (e.g. reactiveVal) it would keep showing "All
  # set" after they came back to Trial Setup and changed something, wrongly
  # suggesting they don't need to click Submit again.
  # SK: "refresh memory" -- Prior Information and Trial Simulation should
  # reflect the setup as of the LAST successful Submit click, never the
  # live, possibly-mid-edit Trial Setup inputs. trial_setup_signature()
  # canonicalizes every Trial Setup field that actually feeds Prior/
  # Simulation (drug mode, dose type, theta/NN/cohort, and whichever
  # dose-level/dose-range/unit fields are relevant for the current drug
  # mode + dose type) into one comparable string. trial_setup_submitted_signature
  # is set to that string only inside the Submit handler below.
  # setup_is_current() is TRUE exactly when the two match -- i.e. nothing on
  # Trial Setup has changed since the last Submit -- and both Prior
  # Information and Trial Simulation are gated on it (see the
  # conditionalPanel wrapping their tab bodies), so editing Trial Setup
  # without resubmitting locks those tabs instead of quietly showing/using
  # stale or half-updated values.
  trial_setup_signature <- reactive({
    dose_type <- input$prior_dose_type
    parts <- list(
      dose_type = dose_type,
      theta = input$theta,
      NN = input$NN,
      cohort = input$cohort
    )
    {
      if (identical(dose_type, "Discrete")) {
        values <- categorical_actual_dose_values()
        units <- categorical_actual_dose_units()
        parts$num_doses <- input$num_doses
        parts$startdose <- input$startdose
        parts$values <- paste(values, collapse = ",")
        parts$units <- paste(units, collapse = ",")
      } else {
        parts$min_dose <- input$min_dose
        parts$max_dose <- input$max_dose
        parts$dose_unit <- input$dose_unit
      }
    }
    paste(vapply(parts, function(x) paste(as.character(x), collapse = "|"), character(1)), collapse = "::")
  })
  trial_setup_submitted_signature <- reactiveVal(NULL)
  setup_is_current <- reactive({
    submitted <- trial_setup_submitted_signature()
    !is.null(submitted) && identical(trial_setup_signature(), submitted)
  })
  # SK: bumped on every successful Submit -- output$dose_inputs (the dynamic
  # scenario/dose tables, which normally re-use whatever the user last typed via
  # isolate(input[[id]])) key off this so a Submit actually blanks those
  # cells out instead of silently re-showing the old numbers underneath the
  # freshly-reset static inputs.
  setup_reset_token <- reactiveVal(0L)
  output$trial_setup_current <- reactive({
    isTRUE(setup_is_current())
  })
  outputOptions(output, "trial_setup_current", suspendWhenHidden = FALSE)
  
  # SK: once Submit is clicked on Trial Setup, Prior Information and Trial
  # Simulation reset to a clean slate -- both their computed results (plots,
  # tables, reports, background run state) AND every input on those two tabs
  # go back to defaults, as if the user had never touched them for this
  # setup. Leaves Trial Conduct tab state untouched -- Conduct is explicitly
  # independent of this page (see the "Setup Notes" panel on Trial Setup).
  reset_prior_and_simulation_results <- function() {
    opt1_val(NULL)
    prior_report_val(NULL)
    combined_table_val(NULL)
    stopped_details_val(data.frame())
    scenario_data_val(NULL)
    trial_report_val(NULL)
    trial_scenario_results_val(NULL)
    
    reset_trial_run_artifacts()
    trial_run_state$running <- FALSE
    trial_run_state$stop_requested <- FALSE
    trial_run_state$completed <- 0L
    trial_run_state$requested <- 0L
    trial_run_state$status <- "Idle. Run the simulations to generate trial results."
    trial_run_state$status_type <- "idle"
    trial_run_state$results <- list()
    trial_run_state$scenario_specs <- list()
    trial_run_state$scenario_results <- list()
    trial_run_state$scenario_index <- 1L
    trial_run_state$trial_inputs <- NULL
    trial_run_state$run_id <- NULL
    
    # Static Prior Information inputs back to their original defaults.
    updateNumericInput(session, "p0", value = 0.1)
    updateNumericInput(session, "p1", value = 0.2)
    updateNumericInput(session, "condition", value = 2)
    updateNumericInput(session, "seed", value = 988)
    
    # Static Trial Simulation inputs back to their original defaults.
    updateNumericInput(session, "M", value = 10)
    updateNumericInput(session, "max_dose_increment", value = 10)
    updateNumericInput(session, "deltastop", value = 0.85)
    updateRadioButtons(session, "trial_mode", selected = "No")
    updateNumericInput(session, "burn", value = 1000)
    updateNumericInput(session, "mm", value = 1500)
    updateNumericInput(session, "lower_f_bound", value = 0.25)
    updateNumericInput(session, "increment", value = 0.05)
    updateRadioButtons(session, "alpha_increment_rule", selected = "Theta")
    updateNumericInput(session, "alpha_increment_cutoff", value = 0.25)
    updateRadioButtons(session, "stopping_rule", selected = "No")
    updateNumericInput(session, "num_patients_restrict", value = NA)
    updateNumericInput(session, "efficiency_stop_tolerance", value = NA)
    
    # Dynamic tables (per-dose true DLT probabilities): back to one scenario,
    # and setup_reset_token tells their renderUI to blank previously-typed
    # cell values instead of re-using them.
    trial_scenario_count(1L)
    setup_reset_token(isolate(setup_reset_token()) + 1L)
  }
  
  observeEvent(input$trial_setup_submit, {
    missing_fields <- trial_setup_missing_fields(input)
    
    if (length(missing_fields) > 0L) {
      show_validation_error(
        "Trial Setup is incomplete",
        tagList(
          p("Please fill in the following before continuing:"),
          tags$ul(lapply(missing_fields, tags$li))
        )
      )
      return(invisible(NULL))
    }
    
    reset_prior_and_simulation_results()
    trial_setup_submitted_signature(trial_setup_signature())
    
    updateTabsetPanel(session, "main_tabs", selected = "Trial Design")
    updateTabsetPanel(session, "trial_design_tabs", selected = "Prior Information")
  })
  
  # SK: any edit back on Trial Setup after a submit invalidates
  # trial_setup_signature() (it depends on every field read above), which
  # flips setup_is_current() to FALSE and re-locks Prior Information/Trial
  # Simulation until Submit is clicked again -- no separate observer needed.
  
  observeEvent(input$theta, {
    theta <- suppressWarnings(as.numeric(input$theta))
    if (length(theta) != 1L || !is.finite(theta) || theta <= 0 || theta >= 1) {
      return(invisible(NULL))
    }
    current_cutoff <- suppressWarnings(as.numeric(input$alpha_increment_cutoff))
    if (length(current_cutoff) != 1L || !is.finite(current_cutoff) || current_cutoff <= 0 || current_cutoff >= theta) {
      current_cutoff <- theta / 2
    }
    updateNumericInput(
      session,
      "alpha_increment_cutoff",
      value = current_cutoff,
      min = 0,
      max = theta
    )
    conduct_cutoff <- suppressWarnings(as.numeric(input$conduct_alpha_increment_cutoff))
    if (length(conduct_cutoff) != 1L || !is.finite(conduct_cutoff) || conduct_cutoff <= 0 || conduct_cutoff >= theta) {
      conduct_cutoff <- theta / 2
    }
    updateNumericInput(
      session,
      "conduct_alpha_increment_cutoff",
      value = conduct_cutoff,
      min = 0,
      max = theta
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$prior_dose_type, {
    if (identical(input$prior_dose_type, "Continuous")) {
      updateRadioButtons(session, "trial_mode", selected = "No")
    }
  }, ignoreInit = TRUE)
  
  output$categorical_actual_dose_values_ui <- renderUI({
    num_doses <- suppressWarnings(as.integer(input$num_doses))
    if (length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 2L) {
      return(NULL)
    }
    num_doses <- min(max(num_doses, 2L), 12L)
    
    # SK: each dose level gets its own unit field now (a dose level's unit
    # can differ from another's, e.g. mg vs. mg/m^2), so there is a
    # "Dose Level i" value input paired with an "Dose Level i Unit" text
    # input for every dose level, instead of one shared "Dose Unit" field.
    tagList(
      tags$hr(style = "border-top: 1px solid #ddd; margin: 8px 0 10px 0;"),
      tags$p(style = "font-size: 12px; font-weight: 600; margin-bottom: 6px;", "Actual Dose Values"),
      tags$p(
        style = "font-size: 11px; color: #666; margin-bottom: 6px;",
        "List dose levels from lowest to highest expected toxicity. Units may differ between levels and a level may be a combination (describe it in the unit field). Each level needs a different value."
      ),
      do.call(tagList, lapply(seq_len(num_doses), function(i) {
        fluidRow(
          column(
            6,
            numericInput(
              paste0("actual_dose_", i),
              paste0("Dose Level ", i),
              value = i,
              min = 0
            )
          ),
          column(
            6,
            textInput(
              paste0("actual_dose_unit_", i),
              paste0("Dose Level ", i, " Unit / Description"),
              value = "mg"
            )
          )
        )
      }))
    )
  })
  
  
  phase1_report_title <- function() {
    report_title <- trimws(as.character(input$report_title))
    if (nzchar(report_title)) report_title else "MEWOC Phase 1 Design Report"
  }
  
  phase1_report_filename_prefix <- function() {
    title <- phase1_report_title()
    prefix <- gsub("[^A-Za-z0-9]+", "_", title)
    prefix <- gsub("^_+|_+$", "", prefix)
    if (nzchar(prefix)) prefix else "MEWOC_Phase_1_Design"
  }
  
  observe({
    num_doses <- suppressWarnings(as.integer(input$num_doses))
    if (length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 2L) {
      return(invisible(NULL))
    }
    
    max_startdose <- num_doses - 1L
    current_startdose <- suppressWarnings(as.integer(input$startdose))
    if (length(current_startdose) != 1L || !is.finite(current_startdose)) {
      current_startdose <- 1L
    }
    updated_startdose <- min(max(current_startdose, 1L), max_startdose)
    
    updateNumericInput(
      session,
      "startdose",
      value = updated_startdose,
      min = 1,
      max = max_startdose,
      step = 1
    )
  })
  
  output$accelerated_cap_ui <- renderUI({
    num_doses <- suppressWarnings(as.integer(input$num_doses))
    startdose <- suppressWarnings(as.integer(input$startdose))
    if (
      length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 2L ||
      length(startdose) != 1L || !is.finite(startdose) || startdose < 1L
    ) {
      return(NULL)
    }
    
    # Provided by the user (blank by default); validation requires a whole number from the
    # starting dose up to one level below the highest dose level.
    numericInput(
      "max_accel_dose_level",
      "Maximum Dose Level for Acceleration",
      value = isolate(input$max_accel_dose_level),
      step = 1
    )
  })
  
  
  observeEvent(input$add_trial_scenario, {
    current_count <- suppressWarnings(as.integer(trial_scenario_count()))
    if (length(current_count) != 1L || !is.finite(current_count) || current_count < 1L) {
      current_count <- 1L
    }
    trial_scenario_count(current_count + 1L)
  })
  
  observeEvent(input$remove_trial_scenario, {
    current_count <- suppressWarnings(as.integer(trial_scenario_count()))
    if (length(current_count) != 1L || !is.finite(current_count) || current_count <= 1L) {
      showNotification("Scenario 1 cannot be deleted.", type = "message")
      trial_scenario_count(1L)
      return(invisible(NULL))
    }
    trial_scenario_count(current_count - 1L)
  })
  
  # Dynamically create numeric input boxes
  output$trial_scenario_label_ui <- renderUI({
    
    
    if (identical(input$prior_dose_type, "Continuous")) {
      tags$b("Define the true continuous toxicity curve for each scenario")
    } else {
      tags$b("Provide the True Probability of DLT for Each Dose Levels")
    }
  })
  
  # SK: output$dose_inputs normally re-uses whatever the user last typed into
  # each true-DLT scenario cell (via isolate(input[[id]])) so adding/removing
  # scenario rows doesn't wipe the others. That's the right behavior day to
  # day, but it means a Trial Setup Submit-triggered reset (setup_reset_token
  # bumped, trial_scenario_count back to 1) would otherwise still show
  # scenario 1's old numbers underneath. dose_inputs_last_reset_token tracks
  # which reset this output has already rendered for, so it can render once
  # with every cell blanked (just_reset == TRUE) and go back to normal
  # isolate()-based persistence afterward.
  dose_inputs_last_reset_token <- reactiveVal(-1L)
  output$dose_inputs <- renderUI({
    current_reset_token <- setup_reset_token()
    just_reset <- !identical(current_reset_token, isolate(dose_inputs_last_reset_token()))
    if (just_reset) {
      isolate(dose_inputs_last_reset_token(current_reset_token))
    }
    
    if (identical(input$prior_dose_type, "Continuous")) {
      scenario_count <- suppressWarnings(as.integer(trial_scenario_count()))
      if (length(scenario_count) != 1L || !is.finite(scenario_count) || scenario_count < 1L) {
        scenario_count <- 1L
      }
      
      header_cells <- list(
        tags$th("", style = "width: 100px; vertical-align: middle;"),
        tags$th("True MTD Dose", style = "text-align: center; min-width: 130px; white-space: nowrap;"),
        tags$th(HTML("True P(DLT) at Minimum Dose"), style = "text-align: center; min-width: 180px; white-space: nowrap;")
      )
      
      body_rows <- lapply(seq_len(scenario_count), function(scenario_index) {
        true_mtd_id <- paste0("cont_true_mtd_", scenario_index)
        true_p_min_id <- paste0("cont_true_p_min_", scenario_index)
        default_mtd <- {
          suppressWarnings(as.numeric(input$max_dose))
        }
        if (TRUE && !is.finite(default_mtd)) default_mtd <- 30
        current_mtd <- if (just_reset) NULL else isolate(input[[true_mtd_id]])
        current_p_min <- if (just_reset) NULL else isolate(input[[true_p_min_id]])
        if (is.null(current_mtd)) current_mtd <- default_mtd
        if (is.null(current_p_min)) current_p_min <- 0.05
        tags$tr(
          tags$td(tags$b(paste("Scenario", scenario_index)), style = "vertical-align: middle; white-space: nowrap; padding: 4px 8px;"),
          tags$td(
            style = "padding: 3px 4px;",
            numericInput(true_mtd_id, NULL, value = current_mtd, min = 0, step = 0.01, width = "125px")
          ),
          tags$td(
            style = "padding: 3px 4px;",
            numericInput(true_p_min_id, NULL, value = current_p_min, min = 0, max = 1, step = 0.01, width = "125px")
          )
        )
      })
      
      return(div(
        style = "overflow-x: auto; margin-bottom: 8px;",
        tags$table(
          class = "table table-bordered table-sm",
          style = "width: auto; margin-bottom: 0;",
          tags$thead(do.call(tags$tr, header_cells)),
          do.call(tags$tbody, body_rows)
        )
      ))
    }
    
    req(input$num_doses)
    
    num_doses <- suppressWarnings(as.integer(input$num_doses))
    scenario_count <- suppressWarnings(as.integer(trial_scenario_count()))
    if (length(num_doses) != 1L || !is.finite(num_doses) || num_doses < 1L) {
      return(NULL)
    }
    if (length(scenario_count) != 1L || !is.finite(scenario_count) || scenario_count < 1L) {
      scenario_count <- 1L
    }
    
    header_cells <- c(
      list(tags$th("", style = "width: 100px; vertical-align: middle;")),
      lapply(seq_len(num_doses), function(i) {
        dose_label <- categorical_actual_dose_labels()
        if (length(dose_label) == num_doses) {
          dose_label <- dose_label[i]
        } else {
          dose_label <- paste("Dose", i)
        }
        tags$th(
          dose_label,
          style = "text-align: center; min-width: 85px; white-space: nowrap;"
        )
      })
    )

    body_rows <- lapply(seq_len(scenario_count), function(scenario_index) {
      dose_cells <- lapply(seq_len(num_doses), function(i) {
        input_id <- scenario_dose_input_id(scenario_index, i)
        current_value <- if (just_reset) NULL else isolate(input[[input_id]])
        if (is.null(current_value)) current_value <- NA
        tags$td(
          style = "padding: 3px 4px;",
          numericInput(
            inputId = input_id,
            label   = NULL,
            value   = current_value,
            min     = 0,
            max     = 1,
            step    = 0.01,
            width   = "85px"
          )
        )
      })
      do.call(tags$tr, c(
        list(tags$td(
          tags$b(paste("Scenario", scenario_index)),
          style = "vertical-align: middle; white-space: nowrap; padding: 4px 8px;"
        )),
        dose_cells
      ))
    })
    
    div(
      style = "overflow-x: auto; margin-bottom: 8px;",
      do.call(
        tags$table,
        c(
          list(
            class = "table table-bordered table-sm",
            style = "width: auto; margin-bottom: 0;",
            tags$thead(do.call(tags$tr, header_cells)),
            do.call(tags$tbody, body_rows)
          )
        )
      )
    )
  })
  
  
  #########################################
  ######### Prior tab Calculations#################
  #########################################  
  prior_results <- eventReactive(input$runPrior, {
    on.exit(session$sendCustomMessage("clearRunButtonClicked", list(id = "runPrior")), add = TRUE)
    
    
    # ------------------------------
    # Create the progress bar
    # ------------------------------
    progress <- shiny::Progress$new()           # creates the progress bar
    on.exit(progress$close())                    # closes it when done
    progress$set(
      message = "Running prior calculations...",
      detail = "Preparing prior hyperparameter search",
      value = 0
    )
    
    
    prior_inputs <- validate_prior_inputs(
      input,
      clear_prior_results_fn = function() {
        opt1_val(NULL)
        prior_report_val(NULL)
      }
    )
    if (is.null(prior_inputs)) {
      return(NULL)
    }
    if (identical(prior_inputs$dose_type, "Discrete")) {
      prior_inputs$actual_dose_levels <- categorical_actual_dose_values()
      prior_inputs$dose_unit <- input$dose_unit
    }
    
    set.seed(prior_inputs$seed)
    
    progress$set(
      message = "Running prior calculations...",
      detail = "Running prior hyperparameter search",
      value = 0.10
    )
    
    # Prior hyperparameters by BOBYQA search (run_mewoc_grid_search() in R/prior_logic.R).
    gridsearch_result <- run_mewoc_grid_search(
      theta = prior_inputs$theta,
      p0 = prior_inputs$p0,
      p1 = prior_inputs$p1,
      condition = prior_inputs$condition,
      progress = progress
    )
    
    opt1<- as.data.frame(gridsearch_result[1,])#fixing the first option
    # Ensure numeric types (prevent factors/characters from previous cbinds)
    opt1 <- as.data.frame(lapply(opt1, function(x) as.numeric(as.character(x))))
    opt1_val(opt1)
    
    progress$set(
      message = "Running prior calculations...",
      detail = "Formatting results",
      value = 0.85
    )
    
    prior_lines <- c(
      paste0("\u03C1\u2081 ~ beta(", round(opt1$a0, 2), ", ", round(opt1$b0, 2), ")"),
      paste0("\u03C1\u2080/\u03C1\u2081 ~ beta(", round(opt1$a1, 2), ", ", round(opt1$b1, 2), ")")
    )
    result <- paste(prior_lines, collapse = "\n")
    prior_report_val(list(
      inputs = prior_inputs,
      opt1 = opt1,
      prior_lines = prior_lines,
      result_text = result,
      generated_at = Sys.time()
    ))
    
    progress$set(
      message = "Running prior calculations...",
      detail = "Complete",
      value = 1
    )
    
    result#keep this after progress bar, otherwise no prior results print
  })
  
  output$PriorResults <- renderText({
    result <- prior_results()
    if (!is.null(result)) {
      session$sendCustomMessage("clearRunButtonClicked", list(id = "runPrior"))
    }
    result
  })
  
  get_prior_plot_config <- function() {
    dose_type <- trimws(as.character(input$prior_dose_type))
    if (length(dose_type) != 1L || !dose_type %in% c("Discrete", "Continuous")) {
      dose_type <- "Discrete"
    }
    
    if (identical(dose_type, "Continuous")) {
      min_dose <- suppressWarnings(as.numeric(input$min_dose))
      max_dose <- suppressWarnings(as.numeric(input$max_dose))
      dose_unit <- trimws(as.character(input$dose_unit))
      if (!is.finite(min_dose) || !is.finite(max_dose) || max_dose <= min_dose || !nzchar(dose_unit)) {
        return(NULL)
      }
      dose_labels <- paste0(c(min_dose, max_dose), " ", dose_unit)
      return(list(
        dose_type = dose_type,
        num_doses = 2L,
        dose_values = c(0, 1),
        dose_labels = dose_labels,
        plot_title = "Distribution of Probability of DLT for Minimum and Maximum Doses"
      ))
    }
    
    num_doses <- suppressWarnings(as.integer(input$num_doses))
    if (!is.finite(num_doses) || num_doses < 2L || num_doses > 12L) {
      return(NULL)
    }
    dose_labels <- categorical_actual_dose_labels()
    if (length(dose_labels) != num_doses) {
      dose_labels <- paste("Dose level", seq_len(num_doses))
    }
    list(
      dose_type = dose_type,
      num_doses = num_doses,
      dose_values = seq(0, 1, length.out = num_doses),
      dose_labels = dose_labels,
      plot_title = "Distribution of Probability of DLT for Each Dose Level"
    )
  }
  
  output$pdlt_plot_ui <- renderUI({
    
    req(!is.null(opt1_val()))
    plot_config <- get_prior_plot_config()
    if (is.null(plot_config)) {
      return(
        tags$div(
          style = "padding: 14px; border: 1px solid #d9d9d9; border-radius: 6px; background-color: #fff8e8; color: #8a5a20;",
          "Please enter valid prior dose settings to produce the probability of DLT plots."
        )
      )
    }
    num_doses <- plot_config$num_doses
    panel_cols <- min(3, max(1, num_doses))
    panel_rows <- ceiling(num_doses / panel_cols)
    plot_height <- max(260, panel_rows * 230)
    
    tags$div(
      style = "height: 340px; overflow-y: auto; overflow-x: hidden; border: 1px solid #d9d9d9; padding: 8px;",
      plotOutput("pdlt_Plot", height = paste0(plot_height, "px"))
    )
  })
  
  
  
  output$pdlt_Plot <- renderPlot({
    opt1 <- opt1_val()
    req(!is.null(opt1))
    req(is.numeric(opt1$a0), is.numeric(opt1$b0), is.numeric(opt1$a1), is.numeric(opt1$b1))
    plot_config <- get_prior_plot_config()
    validate(
      need(
        !is.null(plot_config),
        "Please enter valid prior dose settings to produce the probability of DLT plots."
      )
    )
    build_prior_plot(
      opt1 = opt1,
      num_doses = plot_config$num_doses,
      seed_value = input$seed,
      dose_values = plot_config$dose_values,
      dose_labels = plot_config$dose_labels,
      plot_title = plot_config$plot_title
    )
  })
  
  
  # MT: Download buttons for Prior Results (conditionally shown when prior results are available)
  output$prior_download_buttons_ui <- renderUI({
    req(!is.null(opt1_val()))
    
    fluidRow(
      column(12,
             div(
               style = "display: flex; gap: 10px; margin-bottom: 15px;",
               downloadButton(
                 "download_prior_plots",
                 "Download Combined Plot (JPEG)"
               ),
               downloadButton(
                 "download_prior_summary",
                 "Download Inputs and Results (CSV)"
               )
             )
      )
    )
  })
  
  output$download_prior_word <- downloadHandler(
    filename = function() {
      {
        paste0("MEWOC_Prior_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
      }
    },
    content = function(file) {
      
      library(officer)
      
      
      opt1 <- opt1_val()
      
      # Validate that prior results are available
      if (is.null(opt1)) {
        stop("Prior results not yet computed. Run prior calculation first.")
      }
      
      # Create Word document
      doc <- officer::read_docx()
      
      # Add title and sections
      doc <- officer::body_add_par(doc, "MEWOC Design - Prior Calculation Results")
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, "Input Parameters")
      
      # Add input parameters as text
      doc <- officer::body_add_par(doc, paste("Target Probability of DLT (\u03b8):", input$theta))
      doc <- officer::body_add_par(doc, paste("Dose Level Type:", input$prior_dose_type))
      if (identical(input$prior_dose_type, "Continuous")) {
        doc <- officer::body_add_par(doc, paste("Minimum Dose:", input$min_dose, input$dose_unit))
        doc <- officer::body_add_par(doc, paste("Maximum Dose:", input$max_dose, input$dose_unit))
      } else {
        doc <- officer::body_add_par(doc, paste("Number of Dose Levels:", input$num_doses))
        # SK: each dose level carries its own unit now (see
        # categorical_actual_dose_labels()), so there is no single overall
        # "Dose Unit" to print here -- the per-dose unit is already folded
        # into each Actual Dose Values entry (e.g. "10 mg").
        doc <- officer::body_add_par(doc, paste("Actual Dose Values:", paste(categorical_actual_dose_labels(), collapse = ", ")))
      }
      doc <- officer::body_add_par(doc, paste("Probability (MTD < Min):", input$p0))
      doc <- officer::body_add_par(doc, paste("Probability (MTD > Max):", input$p1))
      doc <- officer::body_add_par(doc, paste("Prior Condition:", input$condition))
      doc <- officer::body_add_par(doc, paste("Random Seed:", input$seed))
      
      # Add hyperparameter section
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, "Prior Hyperparameter Estimation")
      doc <- officer::body_add_par(doc, paste0("\u03c1\u2081 ~ Beta(", round(opt1$a0, 4), ", ", round(opt1$b0, 4), ")"))
      doc <- officer::body_add_par(doc, paste0("\u03c1\u2080/\u03c1\u2081 ~ Beta(", round(opt1$a1, 4), ", ", round(opt1$b1, 4), ")"))
      
      # Add summary statistics
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, "Summary Statistics")
      
      summary_text <- paste(
        "a\u2080 =", round(opt1$a0, 6),
        "\nb\u2080 =", round(opt1$b0, 6),
        "\na\u2081 =", round(opt1$a1, 6),
        "\nb\u2081 =", round(opt1$b1, 6)
      )
      doc <- officer::body_add_par(doc, summary_text)
      
      # Add timestamp
      doc <- officer::body_add_par(doc, "")
      doc <- officer::body_add_par(doc, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      
      # Save document
      print(doc, target = file)
    }
  )
  
  # MT: Download Prior Plots as JPEG
  output$download_prior_plots <- downloadHandler(
    filename = function() {
      {
        paste0("MEWOC_Prior_Combined_Plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpeg")
      }
    },
    content = function(file) {
      
      
      opt1 <- opt1_val()
      plot_config <- get_prior_plot_config()
      
      if (is.null(opt1)) {
        stop("Prior results not yet computed. Run prior calculation first.")
      }
      if (is.null(plot_config)) {
        stop("Please enter valid prior dose settings to produce the probability of DLT plots.")
      }
      
      plot_obj <- build_prior_plot(
        opt1 = opt1,
        num_doses = plot_config$num_doses,
        seed_value = input$seed,
        dose_values = plot_config$dose_values,
        dose_labels = plot_config$dose_labels,
        plot_title = plot_config$plot_title
      )
      num_doses <- plot_config$num_doses
      panel_cols <- min(3, max(1, num_doses))
      panel_rows <- ceiling(num_doses / panel_cols)
      plot_height <- max(7, panel_rows * 3.2)
      
      ggsave(file, plot = plot_obj, width = 14, height = plot_height, dpi = 300, units = "in", device = "jpeg")
    }
  )
  
  # MT: Download Prior Summary as CSV
  output$download_prior_summary <- downloadHandler(
    filename = function() {
      {
        paste0("MEWOC_Prior_Inputs_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      }
    },
    content = function(file) {
      
      opt1 <- opt1_val()
      
      if (is.null(opt1)) {
        stop("Prior results not yet computed. Run prior calculation first.")
      }
      
      dose_rows <- if (identical(input$prior_dose_type, "Continuous")) {
        data.frame(
          Section = "Input Parameters",
          Parameter = c("Treatment Type", "Dose Level Type", "Minimum Dose", "Maximum Dose", "Dose Unit"),
          Value = c("One Drug", input$prior_dose_type, input$min_dose, input$max_dose, input$dose_unit),
          stringsAsFactors = FALSE
        )
      } else {
        dose_labels <- categorical_actual_dose_labels()
        # SK: no single overall "Dose Unit" for Discrete mode any more -- each
        # dose level has its own unit, already folded into dose_labels below.
        data.frame(
          Section = "Input Parameters",
          Parameter = c("Dose Level Type", "Number of Dose Levels", "Actual Dose Values"),
          Value = c(input$prior_dose_type, input$num_doses, paste(dose_labels, collapse = ", ")),
          stringsAsFactors = FALSE
        )
      }
      
      prior_summary <- rbind(
        data.frame(
          Section = "Input Parameters",
          Parameter = "Target Probability of DLT (theta)",
          Value = input$theta,
          stringsAsFactors = FALSE
        ),
        dose_rows,
        data.frame(
          Section = "Input Parameters",
          Parameter = c("P(MTD < Min Dose)", "P(MTD > Max Dose)", "Condition", "Seed"),
          Value = c(input$p0, input$p1, input$condition, input$seed),
          stringsAsFactors = FALSE
        ),
        data.frame(
          Section = "Prior Hyperparameters",
          Parameter = c("rho1 distribution", "rho0/rho1 distribution"),
          Value = c(
            paste0("Beta(", round(opt1$a0, 4), ", ", round(opt1$b0, 4), ")"),
            paste0("Beta(", round(opt1$a1, 4), ", ", round(opt1$b1, 4), ")")
          ),
          stringsAsFactors = FALSE
        )
      )
      
      write.csv(prior_summary, file, row.names = FALSE)
    }
  )
  
  output$trial_download_buttons_ui <- renderUI({
    
    req(!is.null(trial_report_val()) || !is.null(trial_scenario_results_val()))
    
    div(
      style = "display: flex; gap: 10px; margin-bottom: 15px; flex-wrap: wrap;",
      downloadButton(
        "download_trial_summary",
        "Download Trial Summary (CSV)"
      ),
      downloadButton(
        "download_trial_summary_excel",
        "Download Trial Summary (Excel)"
      ),
      downloadButton(
        "download_report_html",
        "Download Report (HTML)"
      ),
      downloadButton(
        "download_report_pdf",
        "Download Report (PDF)"
      )
    )
  })
  
  output$download_trial_summary <- downloadHandler(
    filename = function() {
      paste0("MEWOC_Trial_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      
      scenario_results <- trial_scenario_results_val()
      if (!is.null(scenario_results) && length(scenario_results) > 0) {
        write.csv(build_combined_scenario_csv_df(scenario_results), file, row.names = FALSE)
        return(invisible(NULL))
      }
      
      trial_data <- trial_report_val()
      
      if (is.null(trial_data) || is.null(trial_data$combined_table)) {
        stop("Trial results are not yet available. Run the simulations first.")
      }
      write.csv(build_trial_summary_csv_df(trial_data), file, row.names = FALSE)
    }
  )
  
  output$download_trial_summary_excel <- downloadHandler(
    filename = function() {
      paste0("MEWOC_Trial_Scenario_Summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      write_scenario_results_workbook(trial_scenario_results_val(), file)
    }
  )
  #########################################
  ######### Simulation tab calcualtion#################
  #########################################
  
  
  launch_trial_scenario_run <- function(scenario_index, run_id, base_trial_inputs, opt1) {
    scenario_specs <- isolate(trial_run_state$scenario_specs)
    scenario_count <- length(scenario_specs)
    scenario_spec <- scenario_specs[[scenario_index]]
    scenario_inputs <- base_trial_inputs
    scenario_inputs$doseprobs <- scenario_spec$doseprobs
    if (identical(scenario_inputs$dose_type, "Continuous")) {
      scenario_inputs$true_mtd <- scenario_spec$true_mtd
      scenario_inputs$true_p_min <- scenario_spec$true_p_min
      scenario_inputs$true_p_max <- scenario_spec$true_p_max
      scenario_inputs$true_mtd_std <- scenario_spec$true_mtd_std
      scenario_inputs$true_beta0 <- scenario_spec$true_beta0
      scenario_inputs$true_beta1 <- scenario_spec$true_beta1
    }
    
    ncores <- get_app_trial_worker_count(scenario_inputs$M)
    stop_flag_path <- tempfile(pattern = paste0("trial_stop_", run_id, "_s", scenario_index, "_"), fileext = ".flag")
    progress_path <- tempfile(pattern = paste0("trial_progress_", run_id, "_s", scenario_index, "_"), fileext = ".rds")
    app_dir <- getwd()
    
    trial_run_state$completed <- 0L
    trial_run_state$requested <- as.integer(scenario_inputs$M)
    trial_run_state$ncores <- ncores
    trial_run_state$results <- list()
    trial_run_state$trial_inputs <- scenario_inputs
    trial_run_state$scenario_index <- as.integer(scenario_index)
    trial_run_state$stop_flag_path <- stop_flag_path
    trial_run_state$progress_path <- progress_path
    
    update_trial_run_status(
      paste0(
        "Starting ", scenario_spec$label, " of ", scenario_count,
        ": 0 of ", trial_run_state$requested,
        " simulation(s) completed. The run is now starting in the background."
      ),
      "running"
    )
    
    # Only plain data goes to the background session; it loads the simulation code itself. Letting
    # future collect the app's functions instead made it scan and send every helper (with its source
    # text): about 40 s before each run started, and very slow hand-out of trials to the workers.
    engine_path <- trial_logic_worker_path()
    run_promise <- promises::future_promise(
      {
        setwd(app_dir)
        engine_env <- new.env(parent = globalenv())
        sys.source(engine_path, envir = engine_env, keep.source = FALSE)
        engine_env$run_trial_simulations_background(
          trial_inputs = scenario_inputs,
          opt1 = opt1,
          stop_flag_path = stop_flag_path,
          progress_path = progress_path,
          ncores = ncores
        )
      },
      globals = list(
        app_dir = app_dir, engine_path = engine_path, scenario_inputs = scenario_inputs,
        opt1 = opt1, stop_flag_path = stop_flag_path, progress_path = progress_path, ncores = ncores
      ),
      packages = c("rjags", "coda"),
      # The engine sets its own seeds (set.seed(seed) and one seed per trial), so results do not depend
      # on the background session's random-number stream.
      seed = NULL
    )
    
    trial_run_state$promise <- run_promise
    
    promises::then(
      run_promise,
      onFulfilled = function(run_result) {
        if (!identical(trial_run_state$run_id, run_id)) {
          return(invisible(NULL))
        }
        
        trial_run_state$results <- run_result$results
        trial_run_state$completed <- run_result$completed_M
        
        if (identical(run_result$status, "failed")) {
          trial_run_state$running <- FALSE
          trial_run_state$promise <- NULL
          session$sendCustomMessage("clearRunButtonClicked", list(id = "runSim"))
          reset_trial_run_artifacts()
          failed_trials <- run_result$failed_trials
          failure_lines <- vapply(
            failed_trials[seq_len(min(3, length(failed_trials)))],
            function(res) paste0("Trial ", res$trial_id, ": ", res$error),
            character(1)
          )
          update_trial_run_status(
            paste0(scenario_spec$label, " stopped because at least one trial failed. Review the error dialog and adjust the settings if needed."),
            "error"
          )
          showModal(modalDialog(
            title = "Trial simulation failed",
            paste(c(
              paste(scenario_spec$label, "failed with the current settings."),
              failure_lines
            ), collapse = "\n"),
            easyClose = TRUE
          ))
          return(invisible(NULL))
        }
        
        if (identical(run_result$status, "stopped")) {
          trial_run_state$running <- FALSE
          trial_run_state$promise <- NULL
          session$sendCustomMessage("clearRunButtonClicked", list(id = "runSim"))
          if (length(trial_run_state$results) > 0) {
            trial_summary <- summarize_trial_simulation_results(
              sim_res = trial_run_state$results,
              trial_inputs = scenario_inputs
            )
            scenario_results <- c(
              trial_run_state$scenario_results,
              list(list(label = scenario_spec$label, summary = trial_summary))
            )
            trial_run_state$scenario_results <- scenario_results
            if (length(scenario_results) == 1L) {
              apply_trial_summary_outputs(trial_summary)
            } else {
              apply_trial_scenario_outputs(scenario_results)
            }
          }
          reset_trial_run_artifacts()
          update_trial_run_status(
            paste0(
              "Stopped during ", scenario_spec$label, " after ",
              trial_run_state$completed, " of ", trial_run_state$requested,
              " simulation(s). Later scenarios were not run."
            ),
            "warning"
          )
          return(invisible(NULL))
        }
        
        trial_summary <- summarize_trial_simulation_results(
          sim_res = run_result$results,
          trial_inputs = scenario_inputs
        )
        scenario_results <- c(
          trial_run_state$scenario_results,
          list(list(label = scenario_spec$label, summary = trial_summary))
        )
        trial_run_state$scenario_results <- scenario_results
        
        reset_trial_run_artifacts()
        
        if (scenario_index < scenario_count) {
          update_trial_run_status(
            paste0(
              "Completed ", scenario_spec$label, ". Starting ",
              scenario_specs[[scenario_index + 1L]]$label, " next."
            ),
            "running"
          )
          launch_trial_scenario_run(
            scenario_index = scenario_index + 1L,
            run_id = run_id,
            base_trial_inputs = base_trial_inputs,
            opt1 = opt1
          )
          return(invisible(NULL))
        }
        
        trial_run_state$running <- FALSE
        trial_run_state$promise <- NULL
        session$sendCustomMessage("clearRunButtonClicked", list(id = "runSim"))
        
        if (length(scenario_results) == 1L) {
          apply_trial_summary_outputs(scenario_results[[1]]$summary)
          update_trial_run_status(
            paste0(
              "Completed all ", scenario_results[[1]]$summary$completed_M,
              " simulation(s). Trial summaries and report outputs are ready."
            ),
            "success"
          )
        } else {
          apply_trial_scenario_outputs(scenario_results)
          update_trial_run_status(
            paste0(
              "Completed all ", scenario_count,
              " scenario(s). Trial summaries and combined CSV output are ready."
            ),
            "success"
          )
        }
        invisible(NULL)
      },
      onRejected = function(e) {
        if (!identical(trial_run_state$run_id, run_id)) {
          return(invisible(NULL))
        }
        
        trial_run_state$running <- FALSE
        trial_run_state$promise <- NULL
        session$sendCustomMessage("clearRunButtonClicked", list(id = "runSim"))
        update_trial_run_status(
          paste0(scenario_spec$label, " failed before results were returned."),
          "error"
        )
        reset_trial_run_artifacts()
        showModal(modalDialog(
          title = "Trial simulation failed",
          paste(
            "The background simulation run could not be completed.",
            conditionMessage(e)
          ),
          easyClose = TRUE
        ))
        invisible(NULL)
      }
    )
    
    invisible(NULL)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  start_trial_run <- function() {
    
    if (isTRUE(trial_run_state$running)) {
      showNotification(
        "A simulation run is already in progress. Stop it first or wait for it to finish.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    
    trial_inputs <- validate_trial_inputs(input, has_prior_results = !is.null(opt1_val()))
    if (is.null(trial_inputs)) {
      session$sendCustomMessage("clearRunButtonClicked", list(id = "runSim"))
      return(invisible(NULL))
    }
    scenario_specs <- collect_trial_scenario_specs(trial_inputs)
    if (is.null(scenario_specs)) {
      session$sendCustomMessage("clearRunButtonClicked", list(id = "runSim"))
      return(invisible(NULL))
    }
    
    reset_trial_run_artifacts()
    clear_trial_outputs()
    combined_table_val(NULL)
    stopped_details_val(data.frame())
    trial_report_val(NULL)
    trial_scenario_results_val(NULL)
    opt1 <- opt1_val()
    
    run_id <- paste0(as.integer(Sys.time()), "_", sample.int(1000000, 1))

    trial_run_state$running <- TRUE
    trial_run_state$started_at <- Sys.time()
    trial_run_state$stop_requested <- FALSE
    trial_run_state$completed <- 0L
    trial_run_state$requested <- as.integer(trial_inputs$M)
    trial_run_state$ncores <- get_trial_worker_count(trial_inputs$M)
    trial_run_state$results <- list()
    trial_run_state$scenario_specs <- scenario_specs
    trial_run_state$scenario_results <- list()
    trial_run_state$scenario_index <- 1L
    trial_run_state$trial_inputs <- trial_inputs
    trial_run_state$run_id <- run_id
    trial_run_state$stop_flag_path <- NULL
    trial_run_state$progress_path <- NULL
    
    update_trial_run_status(
      paste0(
        "Starting trial simulations: ", trial_run_state$completed,
        " of ", trial_run_state$requested,
        " completed. The run is now starting in the background."
      ),
      "running"
    )
    
    launch_trial_scenario_run(
      scenario_index = 1L,
      run_id = run_id,
      base_trial_inputs = trial_inputs,
      opt1 = opt1
    )
    
    invisible(NULL)
  }
  
  observeEvent(input$runSim, {
    start_trial_run()
  })
  
  observeEvent(input$stopSim, {
    if (!isTRUE(trial_run_state$running)) {
      showNotification("No simulation is currently running.", type = "message")
      return(invisible(NULL))
    }
    
    trial_run_state$stop_requested <- TRUE
    if (is.character(trial_run_state$stop_flag_path) && nzchar(trial_run_state$stop_flag_path)) {
      file.create(trial_run_state$stop_flag_path)
    }
    update_trial_run_status(
      paste0(
        "Stop requested after ", trial_run_state$completed, " of ",
        trial_run_state$requested,
        " completed simulation(s). The background worker will stop after the current batch finishes."
      ),
      "warning"
    )
    invisible(NULL)
  })
  
  observe({
    if (!isTRUE(trial_run_state$running)) {
      return(invisible(NULL))
    }
    
    # Refresh the progress text every 5 seconds while the simulations run.
    invalidateLater(5000, session)

    progress_path <- trial_run_state$progress_path
    if (!is.character(progress_path) || !nzchar(progress_path)) {
      return(invisible(NULL))
    }

    # Completed trials: the batch progress file, or the per-trial completion markers (which also count
    # trials finished inside a parallel batch that is still running).
    progress_info <- if (file.exists(progress_path)) tryCatch(readRDS(progress_path), error = function(e) NULL) else NULL
    batch_completed <- if (is.null(progress_info)) 0L else as.integer(progress_info$completed)
    requested <- isolate(trial_run_state$requested)
    completed_now <- max(batch_completed, count_trial_progress_done(progress_path))
    if (is.finite(requested) && requested > 0L) completed_now <- min(completed_now, requested)
    trial_run_state$completed <- as.integer(completed_now)
    started_at <- isolate(trial_run_state$started_at)
    elapsed_text <- if (inherits(started_at, "POSIXct")) {
      elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
      minutes <- floor(elapsed / 60)
      paste0(if (minutes > 0) paste0(minutes, " min ") else "", round(elapsed - 60 * minutes), " s")
    } else {
    }

    scenario_specs <- isolate(trial_run_state$scenario_specs)
    scenario_count <- length(scenario_specs)
    scenario_index <- isolate(trial_run_state$scenario_index)
    scenario_label <- if (scenario_count > 1L &&
                          is.finite(scenario_index) &&
                          scenario_index <= scenario_count) {
      paste0(scenario_specs[[scenario_index]]$label, ", ")
    } else {
      ""
    }
    stop_suffix <- if (isTRUE(trial_run_state$stop_requested)) {
      " (stopping after current batch)"
    } else {
      ""
    }
    status_message <- paste0(
      "Simulation running: ",
      scenario_label,
      trial_run_state$completed, " of ", requested, " trial simulation(s) completed",
      if (!is.null(elapsed_text)) paste0(" (elapsed ", elapsed_text, "; updated every 5 seconds)") else "",
      stop_suffix
    )
    update_trial_run_status(
      status_message,
      if (isTRUE(trial_run_state$stop_requested)) "warning" else "running"
    )
    invisible(NULL)
  })
  
  observeEvent(input$prepareStoppedReport, {
    removeModal()
    
    completed_n <- length(trial_run_state$results)
    if (completed_n == 0) {
      showNotification("No completed simulations are available yet for a report.", type = "warning")
      return(invisible(NULL))
    }
    
    trial_summary <- summarize_trial_simulation_results(
      sim_res = trial_run_state$results,
      trial_inputs = trial_run_state$trial_inputs
    )
    apply_trial_summary_outputs(trial_summary)
    update_trial_run_status(
      paste0(
        "Prepared a partial trial summary using ", trial_summary$completed_M,
        " of ", trial_summary$requested_M, " completed simulation(s)."
      ),
      "success"
    )
    invisible(NULL)
  })
  
  observeEvent(input$restartStoppedSim, {
    removeModal()
    start_trial_run()
  })
  
  #########################################
  ######### Escalation Tree Plot ##########
  #########################################
  
  observeEvent(input$runScenario, {
    on.exit(session$sendCustomMessage("clearRunButtonClicked", list(id = "runScenario")), add = TRUE)
    opt1 <- opt1_val()
    tree_inputs <- validate_tree_inputs(input, has_prior_results = !is.null(opt1))
    if (is.null(tree_inputs)) {
      return(invisible(NULL))
    }
    
    # Accelerated EWOC: build_tree_for_cohort_result() starts the tree from the EWOC decision after
    # the lead-in (starting dose to the maximum accelerated dose, no DLT).
    cohort_selected <- tree_inputs$cohort
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building escalation trees...", value = 0)
    
    tree_result <- tryCatch(
      build_tree_for_cohort_result(
        tree_inputs = tree_inputs,
        opt1 = opt1,
        cohort_selected = cohort_selected,
        seed_value = input$seed,
        progress = progress
      ),
      error = function(e) {
        showModal(modalDialog(
          title = "Tree generation failed",
          paste(
            "The escalation tree could not be built with the current inputs.",
            conditionMessage(e)
          ),
          easyClose = TRUE
        ))
      }
    )
    if (is.null(tree_result)) {
      return(invisible(NULL))
    }
    
    if (tree_result$max_depth < tree_inputs$tree_steps - 1L) {
      showModal(modalDialog(
        title = "Tree depth automatically adjusted",
        paste0(
          "Your trial design specifies ", tree_result$max_cohorts, " cohorts (",
          tree_inputs$NN, " patients / ", cohort_selected, " per cohort). ",
          "Tree depth has been capped at ", tree_result$max_depth,
          " decision steps (instead of requested ", tree_inputs$tree_steps - 1L, ") ",
          "to match your actual trial structure."
        ),
        easyClose = TRUE
      ))
    }
    
    scenario_data_val(NULL)
    tree_results <- setNames(list(tree_result$result), paste0("cohort_", cohort_selected))
    scenario_data_val(tree_results)
    tree_report_val(list(
      tree_results = tree_results,
      cohort_selected = cohort_selected,
      tree_steps = tree_inputs$tree_steps,
      trial_mode = tree_inputs$trial_mode,
      generated_at = Sys.time()
    ))
    return(invisible(NULL))
  })
  
  output$tree_download_button_ui <- renderUI({
    sc_all <- scenario_data_val()
    if (is.null(sc_all)) {
      return(NULL)
    }
    
    cohort_selected <- as.integer(input$cohort)
    tree_bundle <- sc_all[[paste0("cohort_", cohort_selected)]]
    if (is.null(tree_bundle)) {
      return(NULL)
    }
    
    downloadButton("download_tree_plot_png", "Download Tree Plot (JPEG)")
  })
  
  build_tree_plot <- function(sc, cohort_size, opt1) {
    build_tree_plot_helper(sc = sc, cohort_size = cohort_size, opt1 = opt1)
  }
  
  report_plot_to_data_uri <- function(plot_obj, width, height) {
    report_plot_to_data_uri_helper(plot_obj = plot_obj, width = width, height = height)
  }
  
  report_named_list_to_df <- function(x, name_col = "Parameter", value_col = "Value") {
    report_named_list_to_df_helper(x = x, name_col = name_col, value_col = value_col)
  }
  
  report_df_to_html_table <- function(df) {
    report_df_to_html_table_helper(df = df)
  }
  
  build_prior_interpretation <- function(prior_data) {
    build_prior_interpretation_helper(prior_data = prior_data)
  }
  
  build_trial_interpretation <- function(trial_data) {
    build_trial_interpretation_helper(trial_data = trial_data)
  }
  
  build_tree_interpretation <- function(tree_data) {
    build_tree_interpretation_helper(tree_data = tree_data)
  }
  
  build_report_payload <- function(selected_sections, include_interpretation,
                                   report_title = "MEWOC Design Summary Report") {
    build_report_payload_from_data(
      report_title = report_title,
      selected_sections = selected_sections,
      include_interpretation = include_interpretation,
      prior_data = prior_report_val(),
      trial_data = trial_report_val(),
      tree_data = tree_report_val(),
      opt1 = opt1_val(),
      trial_scenarios = trial_scenario_results_val()
    )
  }
  
  build_report_html <- function(payload) {
    build_report_html_helper(payload = payload)
  }
  
  
  draw_report_pdf_text_page <- function(title, lines) {
    draw_report_pdf_text_page_helper(title = title, lines = lines)
  }
  
  draw_report_pdf_table_page <- function(title, df, footer = NULL, max_rows = 22) {
    draw_report_pdf_table_page_helper(title = title, df = df, footer = footer, max_rows = max_rows)
  }
  
  build_report_pdf <- function(payload, file) {
    build_report_pdf_helper(payload = payload, file = file)
  }
  
  
  # Readiness: one short line per step, with a tick (ready) or a cross (not yet).
  output$report_status_ui <- renderUI({
    status_line <- function(label, ready, detail) {
      div(
        style = "display: flex; align-items: baseline; gap: 8px; padding: 3px 0;",
        tags$span(
          HTML(if (ready) "&#10004;" else "&#10008;"),
          title = if (ready) "Ready" else "Not ready",
          style = paste0("font-weight: 700; width: 14px;", if (ready) " color: #28a745;" else " color: #c0392b;")
        ),
        tags$span(style = "font-weight: 600;", label),
        tags$span(style = "font-size: 12px; color: #666;", detail)
      )
    }

    prior_data <- prior_report_val()
    trial_data <- trial_report_val()
    tree_data <- tree_report_val()

    tagList(
      status_line("Prior Information", !is.null(prior_data),
                  if (!is.null(prior_data)) format(prior_data$generated_at, "%Y-%m-%d %H:%M") else "run Prior Information"),
      status_line("Trial Simulation", !is.null(trial_data),
                  if (!is.null(trial_data)) format(trial_data$generated_at, "%Y-%m-%d %H:%M") else "run Trial Simulation"),
      status_line("Dose Escalation Tree", !is.null(tree_data),
                  if (!is.null(tree_data)) paste("cohort size", tree_data$cohort_selected) else "build the tree (left)")
    )
  })
  
  output$phase1_design_download_buttons_ui <- renderUI({
    tree_ready <- !is.null(tree_report_val())
    ready_style <- if (tree_ready) {
      paste(
        "color: white;",
        "background-color: #28a745;",
        "border-color: #28a745;",
        "font-weight: bold;"
      )
    } else {
    }
    
    div(
      style = "display: flex; gap: 10px; flex-wrap: wrap;",
      downloadButton(
        "download_protocol_draft",
        "Download Phase 1 Design (Word)",
        style = ready_style
      ),
      downloadButton(
        "download_protocol_draft_html",
        "Download Phase 1 Design (HTML)",
        style = ready_style
      )
    )
  })
  
  output$download_report_html <- downloadHandler(
    filename = function() {
      paste0("MEWOC_Design_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      payload <- build_report_payload(
        selected_sections = c("prior", "trial"),
        include_interpretation = TRUE
      )
      writeLines(build_report_html(payload), con = file, useBytes = TRUE)
    }
  )
  
  
  output$download_protocol_draft <- downloadHandler(
    filename = function() {
      paste0(phase1_report_filename_prefix(), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      prior_data <- prior_report_val()
      opt1 <- opt1_val()
      if (is.null(prior_data) || is.null(opt1)) {
        stop("Run Prior Calculations before downloading the Phase 1 design document.")
      }
      scenarios <- trial_scenario_results_val()
      if (is.null(scenarios) || length(scenarios) == 0L) {
        trial_data <- trial_report_val()
        if (is.null(trial_data)) {
          stop("Run at least one Trial Simulation scenario before downloading the Phase 1 design document.")
        }
        scenarios <- list(list(label = "Scenario 1", summary = trial_data))
      }
      design <- list(
        dose_type = input$prior_dose_type,
        NN = input$NN,
        theta = input$theta,
        cohort = input$cohort,
        lower_f_bound = input$lower_f_bound,
        increment = input$increment,
        deltastop = input$deltastop,
        stopping_rule_selected = identical(input$stopping_rule, "Yes"),
        num_patients_restrict = input$num_patients_restrict,
        num_doses = input$num_doses,
        p0 = input$p0,
        p1 = input$p1,
        M = input$M,
        use_custom_alpha_increment_cutoff = identical(input$alpha_increment_rule, "Custom"),
        alpha_increment_cutoff = if (identical(input$alpha_increment_rule, "Custom")) input$alpha_increment_cutoff else input$theta,
        # Design rules described in the document (accelerated lead-in, continuous limits).
        trial_mode = normalize_trial_mode_input(input$trial_mode),
        max_accel_dose_level = input$max_accel_dose_level,
        startdose = input$startdose,
        max_dose_increment = input$max_dose_increment,
        efficiency_stop_tolerance = input$efficiency_stop_tolerance
      )
      
      tree_data <- NULL
      tree_plot_file <- NULL
      if(identical(input$prior_dose_type, "Discrete"))
      {
        tree_data <- tree_report_val()
        
        if (!is.null(tree_data) && !is.null(tree_data$tree_results)) {
          tree_bundle <- tree_data$tree_results[[paste0("cohort_", tree_data$cohort_selected)]]
          plot_data <- if (!is.null(tree_bundle$primary) && !is.null(tree_bundle$primary$sc)) {
            tree_bundle$primary$sc
          } else {
            tree_bundle
          }
          if (!is.null(plot_data)) {
            tree_plot_file <- tempfile(fileext = ".png")
            tree_plot <- build_tree_plot(plot_data, tree_data$cohort_selected, opt1)
            ggplot2::ggsave(tree_plot_file, plot = tree_plot, width = 10, height = 7, dpi = 220, units = "in")
          }
        }
      }
      
      write_phase1_design(
        output_file = file,
        report_title = phase1_report_title(),
        design = design,
        prior_data = prior_data,
        scenarios = scenarios,
        tree_plot_file = tree_plot_file,
        tree_metadata = tree_data
      )
    }
  )
  
  output$download_protocol_draft_html <- downloadHandler(
    filename = function() {
      paste0(phase1_report_filename_prefix(), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      prior_data <- prior_report_val()
      opt1 <- opt1_val()
      if (is.null(prior_data) || is.null(opt1)) {
        stop("Run Prior Calculations before downloading the Phase 1 design document.")
      }
      scenarios <- trial_scenario_results_val()
      if (is.null(scenarios) || length(scenarios) == 0L) {
        trial_data <- trial_report_val()
        if (is.null(trial_data)) {
          stop("Run at least one Trial Simulation scenario before downloading the Phase 1 design document.")
        }
        scenarios <- list(list(label = "Scenario 1", summary = trial_data))
      }
      
      design <- list(
        dose_type = input$prior_dose_type,
        NN = input$NN,
        theta = input$theta,
        cohort = input$cohort,
        lower_f_bound = input$lower_f_bound,
        increment = input$increment,
        deltastop = input$deltastop,
        stopping_rule_selected = identical(input$stopping_rule, "Yes"),
        num_patients_restrict = input$num_patients_restrict,
        num_doses = input$num_doses,
        p0 = input$p0,
        p1 = input$p1,
        M = input$M,
        use_custom_alpha_increment_cutoff = identical(input$alpha_increment_rule, "Custom"),
        alpha_increment_cutoff = if (identical(input$alpha_increment_rule, "Custom")) input$alpha_increment_cutoff else input$theta,
        # Design rules described in the document (accelerated lead-in, continuous limits).
        trial_mode = normalize_trial_mode_input(input$trial_mode),
        max_accel_dose_level = input$max_accel_dose_level,
        startdose = input$startdose,
        max_dose_increment = input$max_dose_increment,
        efficiency_stop_tolerance = input$efficiency_stop_tolerance
      )
      
      tree_data <- NULL
      tree_plot_file <- NULL
      if(identical(input$prior_dose_type, "Discrete"))
      {
        tree_data <- tree_report_val()
        if (!is.null(tree_data) && !is.null(tree_data$tree_results)) {
          tree_bundle <- tree_data$tree_results[[paste0("cohort_", tree_data$cohort_selected)]]
          plot_data <- if (!is.null(tree_bundle$primary) && !is.null(tree_bundle$primary$sc)) {
            tree_bundle$primary$sc
          } else {
            tree_bundle
          }
          if (!is.null(plot_data)) {
            tree_plot_file <- tempfile(fileext = ".png")
            tree_plot <- build_tree_plot(plot_data, tree_data$cohort_selected, opt1)
            ggplot2::ggsave(tree_plot_file, plot = tree_plot, width = 10, height = 7, dpi = 220, units = "in")
          }
        }
      }
      
      write_phase1_design_html(
        output_file = file,
        report_title = phase1_report_title(),
        design = design,
        prior_data = prior_data,
        scenarios = scenarios,
        tree_plot_file = tree_plot_file,
        tree_metadata = tree_data
      )
    }
    
    
  )
  
  output$trial_conduct_workbook_download_ui <- renderUI({
    prior_ready <- !is.null(opt1_val())
    if (!prior_ready) {
      return(tagList(
        tags$div(
          style = "padding: 8px 10px; margin-bottom: 8px; border: 1px solid #e0b86a; border-radius: 6px; background-color: #fff8e8; color: #7a5315;",
          "Run Prior Calculations to generate the beta hyperparameters before downloading the Trial Conduct workbook."
        ),
        actionButton("trial_conduct_workbook_unavailable", "Download Trial Conduct Workbook", disabled = "disabled")
      ))
    }
    downloadButton("download_trial_conduct_workbook", "Download Trial Conduct Workbook",
                   style = "color: white; background-color: #eb6834; border-color: #eb6834; font-weight: bold;")
  })
  
  output$download_trial_conduct_workbook <- downloadHandler(
    filename = function() {
      paste0("MEWOC_Trial_Conduct_Input_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      opt1 <- opt1_val()
      if (is.null(opt1)) {
        stop("Run Prior Calculations before downloading the Trial Conduct workbook.")
      }
      dose_type_value <- normalize_conduct_dose_type(input$prior_dose_type)
      dose_unit_value <- trimws(as.character(input$dose_unit))
      min_dose_value <- if (identical(dose_type_value, "Continuous")) input$min_dose else ""
      max_dose_value <- if (identical(dose_type_value, "Continuous")) input$max_dose else ""
      max_increment_value <- if (identical(dose_type_value, "Continuous")) {
        max_increment <- suppressWarnings(as.numeric(input$max_dose_increment))
        if (is.finite(max_increment)) max_increment else 100
      } else {
        ""
      }
      trial_mode_value <- if (identical(dose_type_value, "Continuous")) "No" else if (identical(input$trial_mode, "Yes")) "Yes" else "No"
      max_accel_value <- if (identical(trial_mode_value, "Yes")) input$max_accel_dose_level else ""
      if (length(max_accel_value) != 1L || is.null(max_accel_value)) max_accel_value <- ""
      dose_metadata_table <- build_conduct_dose_metadata_table(
        dose_type = dose_type_value,
        dose_unit = dose_unit_value,
        min_dose = min_dose_value,
        max_dose = max_dose_value,
        actual_dose_levels = if (identical(dose_type_value, "Discrete")) categorical_actual_dose_values() else numeric(0),
        num_doses = if (identical(dose_type_value, "Discrete")) input$num_doses else "",
        startdose = if (identical(dose_type_value, "Discrete")) input$startdose else "",
        max_dose_increment = max_increment_value,
        actual_dose_units = if (identical(dose_type_value, "Discrete")) categorical_actual_dose_units() else character(0)
      )
      design_template <- rbind(
        data.frame(
          `Trial Design Parameter` = c("P(MTD < Min)", "P(MTD > Max)", "a0", "b0", "a1", "b1"),
          Value = c(input$p0, input$p1, opt1$a0[1], opt1$b0[1], opt1$a1[1], opt1$b1[1]),
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        dose_metadata_table,
        data.frame(
          `Trial Design Parameter` = c(
            "Target DLT Probability (theta)", "Stopping Threshold Delta",
            "\u03b1", "\u03b1 Increment", "Accelerated EWOC",
            "Maximum Accelerated Dose", "Seed", "Burn-in", "MCMC Samples"
          ),
          Value = c(
            input$theta, input$deltastop,
            input$lower_f_bound, input$increment, trial_mode_value, max_accel_value, input$seed,
            input$burn, input$mm
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        conduct_trial_rule_rows(
          cohort_size = input$cohort,
          total_sample_size = input$NN,
          stopping_rule = input$stopping_rule,
          num_patients_restrict = input$num_patients_restrict,
          efficiency_width = input$efficiency_stop_tolerance,
          dose_type = dose_type_value
        )
      )
      patient_template <- data.frame(
        Cohort = c(1L, 1L, 2L),
        Patient = c(1L, 2L, 3L),
        `Dose Given` = c(NA_real_, NA_real_, NA_real_),
        `DLT Status` = c("No DLT", "DLT", "Unevaluable"),
        `Dose Recommended` = c(NA_real_, NA_real_, NA_real_),
        "\u03b1" = c(NA_real_, NA_real_, NA_real_),
        check.names = FALSE
      )
      workbook <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(workbook, "Trial Design")
      openxlsx::addWorksheet(workbook, "Data Input")
      openxlsx::writeData(workbook, "Trial Design", design_template)
      openxlsx::writeData(workbook, "Data Input", patient_template)
      header_style <- openxlsx::createStyle(
        fgFill = "#D9EAD3", textDecoration = "bold", border = "Bottom",
        halign = "center"
      )
      openxlsx::addStyle(workbook, "Trial Design", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
      openxlsx::addStyle(workbook, "Data Input", header_style, rows = 1, cols = 1:6, gridExpand = TRUE)
      openxlsx::setColWidths(workbook, "Trial Design", cols = 1, widths = 34)
      openxlsx::setColWidths(workbook, "Trial Design", cols = 2, widths = 18)
      openxlsx::setColWidths(workbook, "Data Input", cols = 1:6, widths = "auto")
      openxlsx::freezePane(workbook, "Trial Design", firstRow = TRUE)
      openxlsx::freezePane(workbook, "Data Input", firstRow = TRUE)
      openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    }
  )
  
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      paste0("MEWOC_Design_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      payload <- build_report_payload(
        selected_sections = c("prior", "trial"),
        include_interpretation = TRUE
      )
      build_report_pdf(payload, file)
    }
  )
  
  output$download_tree_plot_png <- downloadHandler(
    filename = function() {
      paste0(
        "MEWOC_Dose_Escalation_Tree_Cohort_",
        input$cohort,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".jpeg"
      )
    },
    content = function(file) {
      sc_all <- scenario_data_val()
      if (is.null(sc_all)) {
        stop("Tree not yet built. Build the escalation tree first.")
      }
      
      cohort_selected <- as.integer(input$cohort)
      tree_bundle <- sc_all[[paste0("cohort_", cohort_selected)]]
      opt1 <- opt1_val()
      
      if (is.null(tree_bundle) || is.null(opt1)) {
        stop("Tree not yet available for the selected cohort. Build the escalation tree first.")
      }
      
      sc <- if (!is.null(tree_bundle$primary) && !is.null(tree_bundle$primary$sc)) {
        tree_bundle$primary$sc
      } else {
        tree_bundle
      }
      
      plot_obj <- build_tree_plot(sc = sc, cohort_size = cohort_selected, opt1 = opt1)
      plot_height_in <- max(8, sc$plot_height / 120)
      plot_width_in <- max(14, 2.4 * (sc$max_depth + 1L))
      
      ggsave(file, plot = plot_obj, width = plot_width_in, height = plot_height_in, dpi = 300, units = "in", device = "jpeg")
    }
  )
  
  output$dose_esc_plot_ui <- renderUI({
    sc_all <- scenario_data_val()
    if (is.null(sc_all)) {
      return(tags$div(
        style = "padding: 60px 20px; text-align: center; color: #777; border: 1px dashed #ccc; border-radius: 6px; background-color: #fafafa;",
        "Set the decision steps and click Build Escalation Tree to preview the tree here."
      ))
    }
    
    cohort_selected <- as.integer(input$cohort)
    tree_bundle <- sc_all[[paste0("cohort_", cohort_selected)]]
    if (is.null(tree_bundle)) {
      return(tags$div(
        style = "padding: 20px; color: #666; font-style: italic;",
        "Please rebuild the tree after changing the cohort size."
      ))
    }
    
    tree_plot_items <- if (!is.null(tree_bundle$primary) && !is.null(tree_bundle$primary$sc)) {
      append(list(tree_bundle$primary), tree_bundle$additional)
    } else {
      list(list(panel_title = "Dose escalation tree", sc = tree_bundle))
    }
    
    plot_blocks <- lapply(seq_along(tree_plot_items), function(idx) {
      plot_item <- tree_plot_items[[idx]]
      plot_id <- paste0("dose_esc_plot_", cohort_selected, "_", idx)
      local({
        item_local <- plot_item
        id_local <- plot_id
        cohort_local <- cohort_selected
        output[[id_local]] <- renderPlot({
          req(input$cohort == cohort_local)
          sc_all_local <- scenario_data_val()
          req(!is.null(sc_all_local))
          opt1_local <- opt1_val()
          req(!is.null(opt1_local))
          tree_bundle_local <- sc_all_local[[paste0("cohort_", cohort_local)]]
          req(!is.null(tree_bundle_local))
          build_tree_plot(sc = item_local$sc, cohort_size = cohort_local, opt1 = opt1_local)
        })
      })
      
      tags$div(
        style = "margin-bottom: 28px;",
        h4(plot_item$panel_title),
        plotOutput(
          outputId = plot_id,
          height = paste0(plot_item$sc$plot_height, "px")
        )
      )
    })
    
    tags$div(
      style = "max-height: 1200px; overflow-y: auto; margin-bottom: 20px;",
      do.call(tagList, plot_blocks)
    )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server, onStart = function() start_mewoc_app(app_dir))
