#' Launch the Pmetrics Reports app
#'
#' @param res A PM_result object.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param background Launch the report in a background R process so multiple
#'   reports can stay open at once. Defaults to `TRUE` when
#'   `launch.browser = TRUE`.
#' @param ... Additional options passed into golem options.
#'
#' @export
run_app <- function(res = NULL, launch.browser = TRUE, background = launch.browser, ...) {
  report_generated_at <- Sys.time()

  if (isTRUE(launch.browser) && isTRUE(background)) {
    return(invisible(run_app_background(res = res, report_generated_at = report_generated_at, ...)))
  }

  app <- golem::with_golem_options(
    app = shiny::shinyApp(ui = app_ui, server = app_server),
    golem_opts = c(list(res = res, report_generated_at = report_generated_at), list(...))
  )

  shiny::runApp(app, launch.browser = launch.browser)
}

#' Alias for [run_app()]
#'
#' @inheritParams run_app
#' @export
report_app <- function(res = NULL, launch.browser = TRUE, ...) {
  run_app(res = res, launch.browser = launch.browser, ...)
}

run_app_background <- function(res = NULL, report_generated_at = Sys.time(), ...) {
  launch_dir <- tempfile("PmetricsReports-launch-")
  dir.create(launch_dir, recursive = TRUE)

  res_path <- file.path(launch_dir, "res.rds")
  saveRDS(res, res_path)

  port_file <- file.path(launch_dir, "port.txt")
  pkg_path <- tryCatch(golem::pkg_path(), error = function(e) "")
  golem_opts <- list(res_path = res_path, report_generated_at = report_generated_at)
  extra_opts <- list(...)

  process <- callr::r_bg(
    function(pkg_path, port_file, golem_opts, extra_opts) {
      if (nzchar(pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(pkg_path, quiet = TRUE)
      } else {
        library(PmetricsReports)
      }

      app <- golem::with_golem_options(
        app = shiny::shinyApp(ui = app_ui, server = app_server),
        golem_opts = c(golem_opts, extra_opts)
      )

      port <- httpuv::randomPort()
      writeLines(as.character(port), port_file)

      shiny::runApp(
        app,
        launch.browser = FALSE,
        port = port,
        host = "127.0.0.1"
      )
    },
    args = list(
      pkg_path = pkg_path,
      port_file = port_file,
      golem_opts = golem_opts,
      extra_opts = extra_opts
    ),
    supervise = TRUE,
    stdout = "|",
    stderr = "|"
  )

  port <- NA_integer_
  deadline <- Sys.time() + 10
  while (!file.exists(port_file) && Sys.time() < deadline) {
    Sys.sleep(0.05)
  }
  if (file.exists(port_file)) {
    port <- suppressWarnings(as.integer(readLines(port_file, warn = FALSE)[1]))
  }

  if (is.finite(port) && !is.na(port)) {
    utils::browseURL(sprintf("http://127.0.0.1:%s", port))
  }

  invisible(process)
}
