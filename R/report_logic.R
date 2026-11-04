report_plot_to_data_uri <- function(plot_obj, width, height) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    return(NULL)
  }
  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)
  ggsave(tmp_png, plot = plot_obj, width = width, height = height, dpi = 200, units = "in")
  base64enc::dataURI(file = tmp_png, mime = "image/png")
}

report_named_list_to_df <- function(x, name_col = "Parameter", value_col = "Value") {
  if (length(x) == 0) {
    return(data.frame())
  }
  data.frame(
    stats::setNames(list(names(x), vapply(x, function(val) paste(val, collapse = ", "), character(1))), c(name_col, value_col)),
    stringsAsFactors = FALSE
  )
}

report_df_to_html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return("<p class='muted'>No data available.</p>")
  }
  header_cells <- paste0(
    "<tr>",
    paste0("<th>", vapply(names(df), htmltools::htmlEscape, character(1)), "</th>", collapse = ""),
    "</tr>"
  )
  body_rows <- apply(df, 1, function(row_vals) {
    paste0(
      "<tr>",
      paste0("<td>", vapply(as.character(row_vals), htmltools::htmlEscape, character(1)), "</td>", collapse = ""),
      "</tr>"
    )
  })
  paste0(
    "<table><thead>", header_cells, "</thead><tbody>",
    paste(body_rows, collapse = ""),
    "</tbody></table>"
  )
}

build_prior_interpretation <- function(prior_data) {
  if (is.null(prior_data) || is.null(prior_data$opt1)) {
    return("Prior results have not been generated yet.")
  }
  paste0(
    "The prior currently anchors the minimum-dose and maximum-dose toxicity relationship through ",
    "rho1 ~ Beta(", round(prior_data$opt1$a0, 3), ", ", round(prior_data$opt1$b0, 3), ") truncated to [0.001, 0.999], and ",
    "rho0/rho1 ~ Beta(", round(prior_data$opt1$a1, 3), ", ", round(prior_data$opt1$b1, 3), "). ",
    "Those hyperparameters define the toxicity shape the trial simulation and escalation tree use before any observed DLT data are incorporated."
  )
}

build_trial_interpretation <- function(trial_data) {
  if (is.null(trial_data) || is.null(trial_data$combined_table)) {
    return("Trial simulation results have not been generated yet.")
  }
  alloc_col <- "% Allocation"
  mtd_col <- "% MTD recommendation"
  if (!all(c(alloc_col, mtd_col) %in% names(trial_data$combined_table))) {
    summary_row <- trial_data$combined_table[1, , drop = FALSE]
    dose_unit <- if (!is.null(trial_data$inputs$dose_unit)) paste0(" ", trial_data$inputs$dose_unit) else ""
    mean_eff_n <- trial_data$overall_avg_n_ci_text
    completed_M <- if (!is.null(trial_data$completed_M)) trial_data$completed_M else trial_data$inputs$M
    requested_M <- if (!is.null(trial_data$requested_M)) trial_data$requested_M else trial_data$inputs$M
    trial_count_text <- if (completed_M < requested_M) {
      paste0(completed_M, " completed out of ", requested_M, " requested")
    } else {
      as.character(completed_M)
    }
    return(paste0(
      "Across ", trial_count_text, " simulated ", trial_data$inputs$trial_mode,
      " trial(s), the mean actual accrued sample size (95% CI) was ", mean_eff_n, ". ",
      "The average estimated MTD was ", summary_row[["Average Estimated MTD"]], dose_unit,
      ", with average bias ", summary_row[["Average Bias"]],
      " and RMSE ", summary_row[["RMSE"]], ". ",
      "The percent selection within 10% of theta was ",
      summary_row[["% Selection within 10% of theta"]], "%."
    ))
  }
  alloc_values <- suppressWarnings(as.numeric(trial_data$combined_table[[alloc_col]]))
  mtd_values <- suppressWarnings(as.numeric(trial_data$combined_table[[mtd_col]]))
  alloc_idx <- if (any(is.finite(alloc_values))) which.max(replace(alloc_values, !is.finite(alloc_values), -Inf)) else 1L
  mtd_idx <- if (any(is.finite(mtd_values))) which.max(replace(mtd_values, !is.finite(mtd_values), -Inf)) else 1L
  dose_label_col <- if ("Dose Level" %in% names(trial_data$combined_table)) {
    "Dose Level"
  } else {
    names(trial_data$combined_table)[1]
  }
  mean_eff_n <- trial_data$overall_avg_n_ci_text
  completed_M <- if (!is.null(trial_data$completed_M)) trial_data$completed_M else trial_data$inputs$M
  requested_M <- if (!is.null(trial_data$requested_M)) trial_data$requested_M else trial_data$inputs$M
  trial_count_text <- if (completed_M < requested_M) {
    paste0(completed_M, " completed out of ", requested_M, " requested")
  } else {
    as.character(completed_M)
  }
  accel_sentence <- if (identical(trial_data$inputs$trial_mode, "Accelerated EWOC")) {
    paste0(
      " The accelerated lead-in treated a mean of ",
      round(mean(trial_data$accel_patients[is.finite(trial_data$accel_patients)]), 2),
      " singleton patient(s) before the cohort-based EWOC phase began."
    )
  } else {
    ""
  }
  paste0(
    "Across ", trial_count_text, " simulated ", trial_data$inputs$trial_mode, " trial(s), the mean actual accrued sample size (95% CI) was ", mean_eff_n, ". ",
    "Most patients were allocated to ", trial_data$combined_table[[dose_label_col]][alloc_idx],
    ", and the most frequently recommended MTD was ", trial_data$combined_table[[dose_label_col]][mtd_idx], ". ",
    "The Delta safety stop (no MTD) was triggered in ", round(100 * trial_data$pstop_delta, 1), "% of trials",
    if (isTRUE(trial_data$inputs$stopping_rule_selected)) {
      paste0(", and the efficiency stop in ", round(100 * trial_data$pstop_rule, 1), "% of trials.")
    } else {
      "."
    },
    accel_sentence
  )
}

report_trial_inputs_for_download <- function(inputs) {
  # Keep dose probabilities separate so downloaded input tables stay readable.
  hidden <- c("doseprobs")
  inputs[setdiff(names(inputs), hidden)]
}

build_tree_interpretation <- function(tree_data) {
  if (is.null(tree_data) || is.null(tree_data$tree_results)) {
    return("Dose escalation tree results have not been generated yet.")
  }
  mode_sentence <- if (identical(tree_data$trial_mode, "Accelerated EWOC")) {
    "For Accelerated EWOC, it starts from the EWOC decision after the lead-in has reached the maximum dose level for acceleration with no DLT; the lead-in patients are included in the model but not drawn. "
  } else {
    ""
  }
  paste0(
    "The decision tree shown here was built for cohort size ", tree_data$cohort_selected,
    " using ", tree_data$trial_mode, " and expands ", tree_data$tree_steps, " decision steps. ",
    mode_sentence,
    "It uses the same prior settings and stopping rules as the simulations: the Delta safety stop (Pr[P(DLT at the minimum dose) > theta] > Delta) ",
    "and the efficiency stop when selected. ",
    "A skipped escalation to a dose not yet given is limited to one dose level, while later revisits and multi-step de-escalation remain allowed."
  )
}

resolve_tree_plot_entries <- function(tree_entry, cohort_selected, opt1) {
  if (is.null(tree_entry) || is.null(opt1)) {
    return(list())
  }

  tree_items <- if (!is.null(tree_entry$primary) && !is.null(tree_entry$primary$sc)) {
    c(list(tree_entry$primary), tree_entry$additional)
  } else {
    list(list(panel_title = "Dose escalation tree", sc = tree_entry))
  }

  lapply(tree_items, function(item) {
    sc <- item$sc
    tree_plot <- build_tree_plot(sc = sc, cohort_size = cohort_selected, opt1 = opt1)
    plot_height_in <- max(8, sc$plot_height / 120)
    plot_width_in <- max(14, 2.4 * (sc$max_depth + 1L))
    list(
      panel_title = item$panel_title,
      sc = sc,
      plot_obj = tree_plot,
      plot_width = plot_width_in,
      plot_height = plot_height_in,
      plot_uri = report_plot_to_data_uri(tree_plot, plot_width_in, plot_height_in)
    )
  })
}

build_report_payload_from_data <- function(report_title, selected_sections, include_interpretation,
                                           prior_data, trial_data, tree_data, opt1,
                                           trial_scenarios = NULL) {
  if (length(selected_sections) == 0) {
    stop("Choose at least one section to include in the report.")
  }

  report_title <- trimws(report_title)
  if (!nzchar(report_title)) {
    report_title <- "MEWOC Design Summary Report"
  }

  payload <- list(
    title = report_title,
    generated_at = Sys.time(),
    selected_sections = selected_sections,
    include_interpretation = include_interpretation,
    prior = list(available = FALSE),
    trial = list(available = FALSE),
    tree = list(available = FALSE)
  )

  if ("prior" %in% selected_sections) {
    if (!is.null(prior_data) && !is.null(prior_data$opt1)) {
      prior_dose_type <- if (!is.null(prior_data$inputs$dose_type)) {
        as.character(prior_data$inputs$dose_type)
      } else {
        "Discrete"
      }
      if (identical(prior_dose_type, "Continuous")) {
        num_doses_prior <- 2L
        dose_values_prior <- c(0, 1)
        dose_labels_prior <- paste0(
          c(prior_data$inputs$min_dose, prior_data$inputs$max_dose),
          " ",
          prior_data$inputs$dose_unit
        )
        prior_plot_title <- "Distribution of Probability of DLT for Minimum and Maximum Doses"
      } else {
        num_doses_prior <- as.integer(prior_data$inputs$num_doses)
        dose_values_prior <- seq(0, 1, length.out = num_doses_prior)
        dose_labels_prior <- if (!is.null(prior_data$inputs$actual_dose_levels)) {
          build_discrete_dose_labels(
            prior_data$inputs$actual_dose_levels,
            prior_data$inputs$dose_unit,
            num_doses_prior
          )
        } else {
          paste("Dose level", seq_len(num_doses_prior))
        }
        prior_plot_title <- "Distribution of Probability of DLT for Each Dose Level"
      }
      prior_plot <- build_prior_plot(
        opt1 = prior_data$opt1,
        num_doses = num_doses_prior,
        seed_value = prior_data$inputs$seed,
        dose_values = dose_values_prior,
        dose_labels = dose_labels_prior,
        plot_title = prior_plot_title
      )
      panel_cols <- min(3, max(1, num_doses_prior))
      panel_rows <- ceiling(num_doses_prior / panel_cols)
      plot_height <- max(7, panel_rows * 3.2)
      payload$prior <- c(prior_data, list(
        available = TRUE,
        plot_obj = prior_plot,
        plot_width = 14,
        plot_height = plot_height,
        plot_uri = report_plot_to_data_uri(prior_plot, 14, plot_height),
        interpretation = if (isTRUE(include_interpretation)) build_prior_interpretation(prior_data) else NULL
      ))
    }
  }

  if ("trial" %in% selected_sections) {
    if (!is.null(trial_data) && !is.null(trial_data$combined_table)) {
      scenario_payloads <- NULL
      if (!is.null(trial_scenarios) && length(trial_scenarios) > 0) {
        scenario_payloads <- lapply(trial_scenarios, function(item) {
          c(item$summary, list(label = item$label))
        })
      }
      payload$trial <- c(trial_data, list(
        available = TRUE,
        scenarios = scenario_payloads,
        interpretation = if (isTRUE(include_interpretation)) build_trial_interpretation(trial_data) else NULL
      ))
    }
  }

  if ("tree" %in% selected_sections) {
    if (!is.null(tree_data) && !is.null(tree_data$tree_results) && !is.null(opt1)) {
      tree_entry <- tree_data$tree_results[[paste0("cohort_", tree_data$cohort_selected)]]
      plot_entries <- resolve_tree_plot_entries(tree_entry, tree_data$cohort_selected, opt1)
      if (length(plot_entries) > 0) {
        payload$tree <- c(tree_data, list(
          available = TRUE,
          sc = plot_entries[[1]]$sc,
          plot_obj = plot_entries[[1]]$plot_obj,
          plot_width = plot_entries[[1]]$plot_width,
          plot_height = plot_entries[[1]]$plot_height,
          plot_uri = plot_entries[[1]]$plot_uri,
          plot_entries = plot_entries,
          interpretation = if (isTRUE(include_interpretation)) build_tree_interpretation(tree_data) else NULL
        ))
      }
    }
  }

  payload
}

build_report_html <- function(payload) {
  report_section <- function(title, body_html) {
    paste0("<section><h2>", htmltools::htmlEscape(title), "</h2>", body_html, "</section>")
  }
  missing_html <- "<div class='note'>This section has not been generated yet in the app.</div>"

  body_parts <- c(
    paste0(
      "<section class='hero'><h1>", htmltools::htmlEscape(payload$title), "</h1>",
      "<p class='lede'>This report combines the currently available Prior, Trial Simulation, and Dose Escalation Tree outputs from the MEWOC Shiny app.</p>",
      "<p class='tiny'><strong>Generated:</strong> ", htmltools::htmlEscape(format(payload$generated_at, "%Y-%m-%d %H:%M:%S")), "</p></section>"
    )
  )

  if ("prior" %in% payload$selected_sections) {
    if (isTRUE(payload$prior$available)) {
      prior_inputs_df <- report_named_list_to_df(payload$prior$inputs)
      prior_body <- paste0(
        report_df_to_html_table(prior_inputs_df),
        "<div class='callout'><p><strong>", htmltools::htmlEscape(payload$prior$prior_lines[1]), "</strong></p>",
        "<p><strong>", htmltools::htmlEscape(payload$prior$prior_lines[2]), "</strong></p>",
        "</div>",
        if (!is.null(payload$prior$interpretation)) paste0("<p>", htmltools::htmlEscape(payload$prior$interpretation), "</p>") else "",
        if (!is.null(payload$prior$plot_uri)) {
          paste0("<img src='", payload$prior$plot_uri, "' alt='Prior plot' style='width: 100%; border: 1px solid #d7d0c4; border-radius: 12px; margin-top: 12px;'>")
        } else {
          "<p class='muted'>Plot image could not be embedded in this HTML export.</p>"
        }
      )
      body_parts <- c(body_parts, report_section("Prior Information", prior_body))
    } else {
      body_parts <- c(body_parts, report_section("Prior Information", missing_html))
    }
  }

  if ("trial" %in% payload$selected_sections) {
    if (isTRUE(payload$trial$available)) {
      scenario_list <- payload$trial$scenarios
      if (is.null(scenario_list) || length(scenario_list) == 0L) {
        scenario_list <- list(c(payload$trial, list(label = "Scenario 1")))
      }
      render_trial_scenario <- function(trial_item, include_interpretation = FALSE) {
        trial_inputs_df <- report_named_list_to_df(c(
          list(
            `Requested number of trials` = trial_item$requested_M,
            `Completed simulations used in summary` = trial_item$completed_M,
            `Run status` = if (identical(trial_item$run_status, "stopped")) "Stopped early" else "Completed"
          ),
          report_trial_inputs_for_download(trial_item$inputs),
          list(`Dose probabilities` = paste(round(trial_item$inputs$doseprobs, 4), collapse = ", "))
        ))
        stopped_df_html <- report_df_to_html_table(utils::head(trial_item$stopped_df, 10))
        accel_reason_values <- trial_item$accel_stop_reason[!is.na(trial_item$accel_stop_reason) & nzchar(trial_item$accel_stop_reason)]
        accel_entry_values <- trial_item$ewoc_entry_dose[is.finite(trial_item$ewoc_entry_dose)]
        accel_reason <- if (length(accel_reason_values) > 0) names(sort(table(accel_reason_values), decreasing = TRUE))[1] else "Not available"
        accel_entry <- if (length(accel_entry_values) > 0) {
          entry_idx <- as.integer(names(sort(table(accel_entry_values), decreasing = TRUE))[1])
          dose_labels <- if (!is.null(trial_item$inputs$actual_dose_levels)) {
            build_discrete_dose_labels(
              trial_item$inputs$actual_dose_levels,
              summary_dose_units(trial_item$inputs),
              length(trial_item$inputs$actual_dose_levels)
            )
          } else {
            NULL
          }
          if (!is.null(dose_labels) && entry_idx >= 1L && entry_idx <= length(dose_labels)) dose_labels[entry_idx] else paste("Dose", entry_idx)
        } else {
          "Not available"
        }
        paste0(
          "<h3>", htmltools::htmlEscape(trial_item$label), "</h3>",
          report_df_to_html_table(trial_inputs_df),
          "<h3>Efficiency Summary</h3>",
          report_df_to_html_table(trial_item$combined_table),
          "<h3>Safety Summary</h3>",
          report_df_to_html_table(trial_item$dlt_summary),
          "<div class='grid'>",
          "<div class='card'><strong>Mode</strong><p>", htmltools::htmlEscape(trial_item$inputs$trial_mode), "</p></div>",
          "<div class='card'><strong>Delta-Based Summary</strong><p>Probability of stopping based on Delta: ", htmltools::htmlEscape(as.character(round(trial_item$pstop_delta, 3))), "</p><p>Average sample size of stopped trials (95% CI): ", htmltools::htmlEscape(trial_item$avg_n_delta_ci_text), "</p></div>",
          "<div class='card'><strong>Patient Allocation Summary</strong><p>Probability of patient-allocation stop: ", htmltools::htmlEscape(as.character(round(trial_item$pstop_rule, 3))), "</p><p>Average sample size of stopped trials (95% CI): ", htmltools::htmlEscape(trial_item$avg_n_stop_rule_ci_text), "</p></div>",
          "<div class='card'><strong>Performance</strong><p>Mean actual accrued sample size (95% CI): ", htmltools::htmlEscape(trial_item$overall_avg_n_ci_text), "</p></div>",
          "</div>",
          if (identical(trial_item$inputs$trial_mode, "Accelerated EWOC")) {
            paste0(
              "<h3>Accelerated Startup Summary</h3>",
              "<div class='grid'>",
              "<div class='card'><strong>Mean accelerated singleton patients</strong><p>",
              htmltools::htmlEscape(as.character(round(mean(trial_item$accel_patients[is.finite(trial_item$accel_patients)]), 3))),
              "</p></div>",
              "<div class='card'><strong>Most common stop reason</strong><p>",
              htmltools::htmlEscape(accel_reason),
              "</p></div>",
              "<div class='card'><strong>Most common EWOC entry dose</strong><p>",
              htmltools::htmlEscape(accel_entry),
              "</p></div>",
              "</div>"
            )
          } else {
            ""
          },
          if (isTRUE(include_interpretation) && !is.null(payload$trial$interpretation)) paste0("<p>", htmltools::htmlEscape(payload$trial$interpretation), "</p>") else "",
          "<h3>Stopped Trial Details</h3>",
          if (nrow(trial_item$stopped_df) > 10) "<p class='tiny'>Showing the first 10 stopped trials in the report.</p>" else "",
          stopped_df_html
        )
      }
      trial_body <- paste(vapply(seq_along(scenario_list), function(i) {
        render_trial_scenario(scenario_list[[i]], include_interpretation = i == 1L)
      }, character(1)), collapse = "")
      body_parts <- c(body_parts, report_section("Trial Simulation", trial_body))
    } else {
      body_parts <- c(body_parts, report_section("Trial Simulation", missing_html))
    }
  }

  if ("tree" %in% payload$selected_sections) {
    if (isTRUE(payload$tree$available)) {
      tree_meta_df <- data.frame(
        Parameter = c("Cohort size used", "Decision steps", "Tree generated"),
        Value = c(
          payload$tree$cohort_selected,
          payload$tree$tree_steps,
          format(payload$tree$generated_at, "%Y-%m-%d %H:%M:%S")
        ),
        stringsAsFactors = FALSE
      )
      tree_body <- paste0(
        report_df_to_html_table(tree_meta_df),
        if (!is.null(payload$tree$interpretation)) paste0("<p>", htmltools::htmlEscape(payload$tree$interpretation), "</p>") else "",
        paste0(vapply(payload$tree$plot_entries, function(entry) {
          paste0(
            "<h3>", htmltools::htmlEscape(entry$panel_title), "</h3>",
            if (!is.null(entry$plot_uri)) {
              paste0(
                "<img src='", entry$plot_uri,
                "' alt='Dose escalation tree' style='width: 100%; border: 1px solid #d7d0c4; border-radius: 12px; margin-top: 12px;'>"
              )
            } else {
              "<p class='muted'>Tree plot image could not be embedded in this HTML export.</p>"
            }
          )
        }, character(1)), collapse = "")
      )
      body_parts <- c(body_parts, report_section("Dose Escalation Tree", tree_body))
    } else {
      body_parts <- c(body_parts, report_section("Dose Escalation Tree", missing_html))
    }
  }

  paste0(
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    "<title>", htmltools::htmlEscape(payload$title), "</title>",
    "<style>",
    ":root{--bg:#f6f4ee;--panel:#fffdf8;--ink:#1f2a2e;--muted:#5a6a70;--accent:#1f6f78;--accent-soft:#dceff1;--warm:#d78258;--line:#d7d0c4;}",
    "*{box-sizing:border-box;}body{margin:0;font-family:Georgia,'Times New Roman',serif;color:var(--ink);background:radial-gradient(circle at top right,#efe7d6 0,transparent 32%),linear-gradient(180deg,#f6f4ee 0%,#f1ede3 100%);line-height:1.55;}",
    ".wrap{max-width:980px;margin:0 auto;padding:36px 20px 56px;}.hero{background:linear-gradient(135deg,rgba(31,111,120,0.10),rgba(215,130,88,0.12));border:1px solid var(--line);border-radius:18px;padding:28px 24px;margin-bottom:22px;}",
    "h1,h2,h3{margin:0 0 10px;line-height:1.2;}h1{font-size:2rem;}h2{font-size:1.3rem;margin-top:26px;}h3{font-size:1.05rem;color:var(--accent);margin-top:18px;}",
    "p{margin:10px 0;}.lede{font-size:1.03rem;color:var(--muted);max-width:760px;}.tiny,.muted{color:var(--muted);font-size:0.95rem;}",
    ".grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:14px;margin:18px 0 8px;}.card{background:var(--panel);border:1px solid var(--line);border-radius:14px;padding:16px 16px 14px;box-shadow:0 8px 24px rgba(31,42,46,0.04);}",
    ".callout{background:var(--accent-soft);border-left:5px solid var(--accent);padding:14px 16px;border-radius:10px;margin:16px 0;}.note{background:#fff7ef;border-left:5px solid var(--warm);padding:14px 16px;border-radius:10px;margin:16px 0;}",
    "table{width:100%;border-collapse:collapse;margin:12px 0 20px;background:var(--panel);border:1px solid var(--line);border-radius:12px;overflow:hidden;}th,td{padding:11px 12px;border-bottom:1px solid #e7e0d5;text-align:left;vertical-align:top;}th{background:#efe8db;font-size:0.96rem;}tr:last-child td{border-bottom:none;}",
    "</style></head><body><div class='wrap'>",
    paste(body_parts, collapse = ""),
    "</div></body></html>"
  )
}

draw_report_pdf_text_page <- function(title, lines) {
  grid::grid.newpage()
  grid::grid.text(title, x = 0.05, y = 0.96, just = c("left", "top"), gp = grid::gpar(fontsize = 18, fontface = "bold"))
  grid::grid.text(
    paste(lines, collapse = "\n"),
    x = 0.05, y = 0.91,
    just = c("left", "top"),
    gp = grid::gpar(fontsize = 10)
  )
}

draw_report_pdf_table_page <- function(title, df, footer = NULL, max_rows = 22) {
  df_to_show <- df
  footer_lines <- character(0)
  if (!is.null(df_to_show) && nrow(df_to_show) > max_rows) {
    df_to_show <- utils::head(df_to_show, max_rows)
    footer_lines <- c(footer_lines, paste("Showing first", max_rows, "rows in the PDF report."))
  }
  if (!is.null(footer) && nzchar(footer)) {
    footer_lines <- c(footer_lines, footer)
  }
  lines <- if (is.null(df_to_show) || nrow(df_to_show) == 0) {
    "No data available."
  } else {
    capture.output(print(df_to_show, row.names = FALSE))
  }
  draw_report_pdf_text_page(title, c(lines, if (length(footer_lines) > 0) c("", footer_lines)))
}

build_report_pdf <- function(payload, file) {
  # The Cairo PDF device renders Unicode labels (theta, alpha); the base pdf() device cannot.
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(filename = file, width = 11, height = 8.5, onefile = TRUE)
  } else {
    grDevices::pdf(file = file, width = 11, height = 8.5, onefile = TRUE)
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  cover_lines <- c(
    paste("Generated:", format(payload$generated_at, "%Y-%m-%d %H:%M:%S")),
    paste("Included sections:", paste(payload$selected_sections, collapse = ", ")),
    "",
    "This PDF summarizes the currently available outputs from the MEWOC app."
  )
  draw_report_pdf_text_page(payload$title, cover_lines)

  if ("prior" %in% payload$selected_sections) {
    if (isTRUE(payload$prior$available)) {
      prior_lines <- c(
        paste("theta:", payload$prior$inputs$theta),
        paste("Dose type:", if (!is.null(payload$prior$inputs$dose_type)) payload$prior$inputs$dose_type else "Discrete"),
        if (identical(payload$prior$inputs$dose_type, "Continuous")) {
          c(
            paste("Minimum dose:", payload$prior$inputs$min_dose, payload$prior$inputs$dose_unit),
            paste("Maximum dose:", payload$prior$inputs$max_dose, payload$prior$inputs$dose_unit)
          )
        } else {
          paste("Number of doses:", payload$prior$inputs$num_doses)
        },
        paste("p0:", payload$prior$inputs$p0),
        paste("p1:", payload$prior$inputs$p1),
        paste("Condition:", payload$prior$inputs$condition),
        paste("Seed:", payload$prior$inputs$seed),
        "",
        payload$prior$prior_lines,
        if (!is.null(payload$prior$interpretation)) c("", payload$prior$interpretation) else NULL
      )
      draw_report_pdf_text_page("Prior Information", prior_lines)
      print(payload$prior$plot_obj)
    } else {
      draw_report_pdf_text_page("Prior Information", "This section has not been generated yet in the app.")
    }
  }

  if ("trial" %in% payload$selected_sections) {
    if (isTRUE(payload$trial$available)) {
      scenario_list <- payload$trial$scenarios
      if (is.null(scenario_list) || length(scenario_list) == 0L) {
        scenario_list <- list(c(payload$trial, list(label = "Scenario 1")))
      }
      for (i in seq_along(scenario_list)) {
        trial_item <- scenario_list[[i]]
        trial_lines <- c(
          paste("Requested simulations:", trial_item$requested_M),
          paste("Completed simulations used in summary:", trial_item$completed_M),
          paste("Run status:", if (identical(trial_item$run_status, "stopped")) "Stopped early" else "Completed"),
          paste("NN:", trial_item$inputs$NN),
          paste("Cohort size:", trial_item$inputs$cohort),
          paste("Start dose:", trial_item$inputs$startdose),
          paste("Trial mode:", trial_item$inputs$trial_mode),
          if (identical(trial_item$inputs$trial_mode, "Accelerated EWOC")) paste("Maximum dose level for acceleration:", trial_item$inputs$max_accel_dose_level) else NULL,
          paste("Delta threshold:", trial_item$inputs$deltastop),
          paste("Stopping rule enabled:", trial_item$inputs$stopping_rule_selected),
          if (isTRUE(trial_item$inputs$stopping_rule_selected)) paste("Max patients per dose:", trial_item$inputs$num_patients_restrict) else NULL,
          paste("Dose probabilities:", paste(round(trial_item$inputs$doseprobs, 4), collapse = ", ")),
          "",
          paste("Probability of stopping based on Delta:", round(trial_item$pstop_delta, 3)),
          paste("Average sample size for Delta-stopped trials:", trial_item$avg_n_delta_ci_text),
          paste("Probability of patient-allocation stop:", round(trial_item$pstop_rule, 3)),
          paste("Average sample size for patient-allocation stopped trials:", trial_item$avg_n_stop_rule_ci_text),
          paste("Mean actual accrued sample size (95% CI):", trial_item$overall_avg_n_ci_text),
          if (identical(trial_item$inputs$trial_mode, "Accelerated EWOC")) paste("Mean accelerated singleton patients:", round(mean(trial_item$accel_patients[is.finite(trial_item$accel_patients)]), 3)) else NULL,
          if (i == 1L && !is.null(payload$trial$interpretation)) c("", payload$trial$interpretation) else NULL
        )
        draw_report_pdf_text_page(paste("Trial Simulation -", trial_item$label), trial_lines)
        draw_report_pdf_table_page(paste(trial_item$label, "Efficiency Summary"), trial_item$combined_table)
        draw_report_pdf_table_page(paste(trial_item$label, "Safety Summary"), trial_item$dlt_summary)
        draw_report_pdf_table_page(paste(trial_item$label, "Stopped Trial Details"), trial_item$stopped_df, max_rows = 12)
      }
    } else {
      draw_report_pdf_text_page("Trial Simulation", "This section has not been generated yet in the app.")
    }
  }

  if ("tree" %in% payload$selected_sections) {
    if (isTRUE(payload$tree$available)) {
      tree_lines <- c(
        paste("Cohort size used:", payload$tree$cohort_selected),
        paste("Trial mode:", payload$tree$trial_mode),
        paste("Decision steps:", payload$tree$tree_steps),
        paste("Generated:", format(payload$tree$generated_at, "%Y-%m-%d %H:%M:%S")),
        if (!is.null(payload$tree$interpretation)) c("", payload$tree$interpretation) else NULL
      )
      draw_report_pdf_text_page("Dose Escalation Tree", tree_lines)
      for (entry in payload$tree$plot_entries) {
        draw_report_pdf_text_page("Dose Escalation Tree Plot", entry$panel_title)
        print(entry$plot_obj)
      }
    } else {
      draw_report_pdf_text_page("Dose Escalation Tree", "This section has not been generated yet in the app.")
    }
  }
}
