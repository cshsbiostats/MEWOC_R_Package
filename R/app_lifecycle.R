# Lifecycle is deferred until Shiny starts the app, so constructing an app object
# neither launches workers nor changes the caller's resource paths or future plan.
start_mewoc_app <- function(app_dir) {
  old_plan <- future::plan()
  paths <- shiny::resourcePaths()
  old_resource <- unname(paths["mewoc-images"])
  had_resource <- length(old_resource) == 1L && !is.na(old_resource)
  cleaned <- FALSE
  cleanup <- function() {
    if (cleaned) return(invisible(NULL))
    cleaned <<- TRUE
    on.exit({
      if (had_resource) shiny::addResourcePath("mewoc-images", old_resource)
      else shiny::removeResourcePath("mewoc-images")
    }, add = TRUE)
    future::plan(old_plan)
    invisible(NULL)
  }
  started <- FALSE
  on.exit(if (!started) cleanup(), add = TRUE)
  shiny::onStop(cleanup, session = NULL)
  shiny::addResourcePath("mewoc-images", file.path(app_dir, "www"))
  # Two sessions retain asynchronous evaluation (one worker would be sequential).
  future::plan(future::multisession, workers = 2L)
  started <- TRUE
  invisible(cleanup)
}
