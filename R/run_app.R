#' Launch the Pmetrics Reports app
#'
#' @param res A PM_result object.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Additional options passed into golem options.
#'
#' @export
run_app <- function(res = NULL, launch.browser = TRUE, ...) {
  app <- golem::with_golem_options(
    app = shiny::shinyApp(ui = app_ui, server = app_server),
    golem_opts = c(list(res = res), list(...))
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
