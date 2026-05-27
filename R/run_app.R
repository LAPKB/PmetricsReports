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

  if (isTRUE(background)) {
    return(invisible(run_app_background(
      res = res,
      report_generated_at = report_generated_at,
      launch.browser = launch.browser,
      ...
    )))
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

report_process_runtime <- local({
  runtime <- new.env(parent = emptyenv())
  runtime$processes <- list()
  runtime
})

prune_retained_report_processes <- function() {
  retained <- report_process_runtime$processes
  if (!length(retained)) {
    return(retained)
  }

  kept <- retained[vapply(retained, function(process) {
    isTRUE(tryCatch(process$is_alive(), error = function(e) FALSE))
  }, logical(1))]

  report_process_runtime$processes <- kept
  kept
}

retain_report_process <- function(process) {
  retained <- prune_retained_report_processes()
  key <- tryCatch(as.character(process$get_pid()), error = function(e) NULL)
  if (is.null(key) || !nzchar(key)) {
    key <- paste0("retained-", length(retained) + 1L)
  }

  retained[[key]] <- process
  report_process_runtime$processes <- retained
  invisible(process)
}

retained_report_process_count <- function() {
  length(prune_retained_report_processes())
}

clear_retained_report_processes <- function() {
  report_process_runtime$processes <- list()
  invisible(TRUE)
}

run_app_background <- function(
    res = NULL,
    report_generated_at = Sys.time(),
    launch.browser = TRUE,
    ...) {
  launch_dir <- tempfile("PmetricsReports-launch-")
  dir.create(launch_dir, recursive = TRUE)

  res_path <- file.path(launch_dir, "res.rds")
  saveRDS(res, res_path)

  port_file <- file.path(launch_dir, "port.txt")
  pkg_path <- tryCatch(
    getNamespaceInfo(asNamespace("PmetricsReports"), "path"),
    error = function(e) ""
  )
  golem_opts <- list(res_path = res_path, report_generated_at = report_generated_at)
  extra_opts <- list(...)

  process <- callr::r_bg(
    function(pkg_path, port_file, golem_opts, extra_opts) {
      if (nzchar(pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(pkg_path, quiet = TRUE)
      } else {
        library(PmetricsReports)
      }

      live_session <- if (is.list(extra_opts$live_session)) {
        unclass(extra_opts$live_session)
      } else {
        NULL
      }

      if (is.list(live_session)) {
        get("prime_live_session_connection", envir = asNamespace("PmetricsReports"))(live_session)
        golem_opts$live_session <- live_session
        extra_opts$live_session <- NULL
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
  deadline <- Sys.time() + 30
  while (!file.exists(port_file) && process$is_alive() && Sys.time() < deadline) {
    Sys.sleep(0.05)
  }

  if (!process$is_alive()) {
    startup_output <- c(
      tryCatch(process$read_all_error_lines(), error = function(e) character()),
      tryCatch(process$read_all_output_lines(), error = function(e) character())
    )
    startup_output <- startup_output[nzchar(startup_output)]
    cli::cli_abort(c(
      "x" = "PmetricsReports app process exited before startup completed.",
      if (length(startup_output)) paste(startup_output, collapse = "\n")
    ))
  }

  if (file.exists(port_file)) {
    port <- suppressWarnings(as.integer(readLines(port_file, warn = FALSE)[1]))
  }

  app_url <- if (is.finite(port) && !is.na(port)) {
    sprintf("http://127.0.0.1:%s", port)
  } else {
    NULL
  }

  if (is.null(app_url) || !nzchar(app_url)) {
    cli::cli_abort(c(
      "x" = "PmetricsReports app did not report a local URL before the startup timeout expired."
    ))
  }

  if (isTRUE(launch.browser) && !is.null(app_url)) {
    utils::browseURL(app_url)
  }

  attr(process, "app_url") <- app_url
  attr(process, "launch_dir") <- launch_dir
  attr(process, "res_path") <- res_path

  retain_report_process(process)

  invisible(process)
}
