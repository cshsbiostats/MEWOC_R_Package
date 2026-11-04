#' Launch the MEWOC single-agent Shiny app
#'
#' Starts the packaged 'shiny' application for single-agent MEWOC designs with
#' discrete or continuous doses: trial setup, prior calibration, simulation,
#' dose escalation tree, Phase 1 design document and Trial Conduct workbook. The
#' app needs the suggested packages 'future', 'promises' and 'readxl'.
#' Helpers are loaded in a private application environment. The caller's future
#' plan and any previous application image resource path are restored on exit.
#'
#' @param ... Arguments passed to [shiny::runApp()], for example `port` or
#'   `launch.browser`.
#'
#' @return Called for its side effect of running the app; returns the value of
#'   [shiny::runApp()] when the app is stopped.
#' @examples
#' if (interactive()) {
#'   run_mewoc_app()
#' }
#' @export
run_mewoc_app <- function(...) {
  app_dir <- system.file("shiny", "mewoc", package = "MEWOC")
  if (!nzchar(app_dir)) {
    stop("The packaged MEWOC Shiny app could not be found.", call. = FALSE)
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Package 'future' is required to run the MEWOC app.", call. = FALSE)
  }
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  shiny::runApp(app_dir, ...)
}
