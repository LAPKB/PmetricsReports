sample_live_messages <- function() {
  list(
    list(kind = "session_started", session_id = "pm-live-test"),
    list(kind = "progress", event = list(kind = "fit_started")),
    list(
      kind = "progress",
      event = list(
        kind = "nonparametric_cycle",
        cycle = 1,
        neg2ll = 120.5,
        objective_delta = 4.2,
        cycle_elapsed_ms = 250,
        total_elapsed_ms = 250,
        nspp = 4,
        status = "continue",
        error_models = list(
          list(outeq = 1, kind = "gamma", value = 0.30),
          list(outeq = 2, kind = "gamma", value = 0.45)
        ),
        parameters = list(
          list(name = "CL", mean = 1.1, median = 1.0, sd = 0.2),
          list(name = "V", mean = 2.2, median = 2.0, sd = 0.4)
        )
      )
    ),
    list(kind = "progress", event = list(kind = "paused", cycle = 1))
  )
}

sample_live_state <- function() {
  config <- list(session_id = "pm-live-test", host = "127.0.0.1", port = 4000L)
  state <- PmetricsReports:::new_live_session_state(config)
  PmetricsReports:::reduce_live_session_state(state, sample_live_messages())
}

sample_final_report_payload <- function() {
  result <- structure(list(marker = "ready"), class = "PM_result")
  jsonlite::base64_enc(memCompress(serialize(result, NULL, xdr = FALSE), type = "gzip"))
}

testthat::test_that("live state reduction builds monitor history", {
  state <- sample_live_state()

  testthat::expect_equal(state$view, "live_monitor")
  testthat::expect_equal(state$run_state, "paused")
  testthat::expect_equal(state$cycle, 1L)
  testthat::expect_equal(state$nspp, 4L)
  testthat::expect_equal(nrow(state$objective_history), 1L)
  testthat::expect_equal(nrow(state$error_model_history), 2L)
  testthat::expect_equal(nrow(state$parameter_history), 2L)
  testthat::expect_match(state$command_status, "Pause acknowledged")
  testthat::expect_equal(state$objective_history$neg2ll[[1]], 120.5)
})

testthat::test_that("live controls enable only when the run state allows them", {
  config <- list(session_id = "pm-live-test", host = "127.0.0.1", port = 4000L)
  state <- PmetricsReports:::new_live_session_state(config)

  controls <- PmetricsReports:::live_control_state(state)
  testthat::expect_false(controls$pause_enabled)
  testthat::expect_false(controls$resume_enabled)
  testthat::expect_false(controls$stop_enabled)

  state$view <- "live_monitor"
  state$run_state <- "running"
  controls <- PmetricsReports:::live_control_state(state)
  testthat::expect_true(controls$pause_enabled)
  testthat::expect_false(controls$resume_enabled)
  testthat::expect_true(controls$stop_enabled)

  state$pending_command <- "pause_after_cycle"
  controls <- PmetricsReports:::live_control_state(state)
  testthat::expect_false(controls$pause_enabled)

  state$pending_command <- NULL
  state$run_state <- "paused"
  controls <- PmetricsReports:::live_control_state(state)
  testthat::expect_false(controls$pause_enabled)
  testthat::expect_true(controls$resume_enabled)
  testthat::expect_true(controls$stop_enabled)

  state$run_state <- "completed"
  controls <- PmetricsReports:::live_control_state(state)
  testthat::expect_false(controls$pause_enabled)
  testthat::expect_false(controls$resume_enabled)
  testthat::expect_false(controls$stop_enabled)
})

testthat::test_that("live command writer and plot helpers follow live cycle data", {
  state <- sample_live_state()

  buffer <- character()
  connection <- textConnection("buffer", open = "w", local = TRUE)
  on.exit(
    {
      if (!is.null(connection)) {
        try(close(connection), silent = TRUE)
      }
    },
    add = TRUE
  )

  result <- PmetricsReports:::send_live_session_command(connection, "stop_after_cycle")

  testthat::expect_true(result$ok)
  close(connection)
  connection <- NULL
  testthat::expect_match(buffer[[1]], '"kind":"stop_after_cycle"')
  testthat::expect_s3_class(PmetricsReports:::build_live_objective_plot(state), "ggplot")
  testthat::expect_s3_class(PmetricsReports:::build_live_error_model_plot(state), "ggplot")
  testthat::expect_s3_class(PmetricsReports:::build_live_parameter_plot(state, "mean"), "ggplot")

  parameter_table <- PmetricsReports:::live_parameter_metric_table(state, "mean")
  testthat::expect_equal(parameter_table$cycle[[1]], 1)
  testthat::expect_true(all(c("CL", "V") %in% names(parameter_table)))
})

testthat::test_that("final report ready message caches the finished report for same-tab reload", {
  config <- list(session_id = "pm-live-test", host = "127.0.0.1", port = 4000L)
  PmetricsReports:::reset_live_report_result()

  state <- sample_live_state()
  reduced <- PmetricsReports:::reduce_live_session_state(
    state,
    list(list(
      kind = "final_report_ready",
      result_payload = sample_final_report_payload(),
      report_generated_at = "2026-05-25T12:34:56.000Z"
    ))
  )
  processed <- PmetricsReports:::consume_live_session_message_effects(
    config,
    reduced,
    list(list(
      kind = "final_report_ready",
      result_payload = sample_final_report_payload(),
      report_generated_at = "2026-05-25T12:34:56.000Z"
    ))
  )

  testthat::expect_true(processed$reload_required)
  testthat::expect_equal(processed$state$view, "final_report")
  testthat::expect_s3_class(PmetricsReports:::get_live_report_result(config), "PM_result")
  testthat::expect_equal(PmetricsReports:::get_live_report_result(config)$marker, "ready")
  testthat::expect_true(inherits(PmetricsReports:::get_live_report_generated_at(config), "POSIXt"))

  PmetricsReports:::reset_live_report_result()
})

testthat::test_that("report failed message keeps the live monitor visible with a failure banner", {
  config <- list(session_id = "pm-live-test", host = "127.0.0.1", port = 4000L)
  PmetricsReports:::reset_live_report_result()

  state <- sample_live_state()
  reduced <- PmetricsReports:::reduce_live_session_state(
    state,
    list(list(kind = "report_failed", message = "Finished report handoff failed"))
  )
  processed <- PmetricsReports:::consume_live_session_message_effects(
    config,
    reduced,
    list(list(kind = "report_failed", message = "Finished report handoff failed"))
  )

  testthat::expect_false(processed$reload_required)
  testthat::expect_equal(processed$state$view, "live_monitor")
  testthat::expect_equal(processed$state$run_state, "failed")
  testthat::expect_match(processed$state$status, "handoff failed")
  testthat::expect_null(PmetricsReports:::get_live_report_result(config))
})

testthat::test_that("live app ui exposes summary metrics and status strip", {
  rendered <- htmltools::renderTags(PmetricsReports:::live_app_ui())$html

  testthat::expect_match(rendered, "Pmetrics Live Report")
  testthat::expect_match(rendered, "live-report-page")
  testthat::expect_match(rendered, "live-summary-grid")
  testthat::expect_match(rendered, "live-command-strip")
})
