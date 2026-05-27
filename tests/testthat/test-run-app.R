testthat::test_that("background report processes are retained while alive", {
  PmetricsReports:::clear_retained_report_processes()
  withr::defer(PmetricsReports:::clear_retained_report_processes())

  fake_alive <- structure(
    list(
      is_alive = function() TRUE,
      get_pid = function() 12345L
    ),
    class = "fake_process"
  )
  fake_dead <- structure(
    list(
      is_alive = function() FALSE,
      get_pid = function() 54321L
    ),
    class = "fake_process"
  )

  PmetricsReports:::retain_report_process(fake_alive)
  PmetricsReports:::retain_report_process(fake_dead)

  testthat::expect_equal(PmetricsReports:::retained_report_process_count(), 1L)
})