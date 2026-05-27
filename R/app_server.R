app_server <- function(input, output, session) {
  if (is_live_session_mode() && !inherits(get_report_result(), "PM_result") && !inherits(get_live_report_result(), "PM_result")) {
    return(live_app_server(input, output, session))
  }

  res <- validate_report_result(get_report_result())
  opts <- report_options()
  ic_method <- resolve_cycle_ic_method(res, preferred = opts$ic_method)
  ic_label <- toupper(ic_method)
  gamlam_type <- if (!is.null(res$cycle$gamlam$type) && length(res$cycle$gamlam$type)) {
    as.character(res$cycle$gamlam$type[[1]])
  } else {
    "Proportional"
  }
  gamlam_label <- if (identical(gamlam_type, "Additive")) "Lambda" else "Gamma"

  outeq_choices <- available_outeq(res)
  parameter_choices <- available_parameters(res)

  summary_info <- summary_table(res)
  export_status <- shiny::reactiveVal("")
  selected_outeq <- shiny::reactiveVal(
    if (length(outeq_choices)) as.integer(outeq_choices[[1]]) else NA_integer_
  )
  selected_parameter <- shiny::reactiveVal(
    if (length(parameter_choices)) parameter_choices[[1]] else NULL
  )

  current_outeq_index <- shiny::reactive({
    shiny::req(length(outeq_choices) > 0)

    current <- selected_outeq()
    index <- match(current, as.integer(outeq_choices))

    if (is.na(index)) 1L else index
  })

  current_outeq <- shiny::reactive({
    shiny::req(length(outeq_choices) > 0)

    current <- selected_outeq()
    if (is.na(current) || !current %in% as.integer(outeq_choices)) {
      as.integer(outeq_choices[[1]])
    } else {
      as.integer(current)
    }
  })

  current_parameter_index <- shiny::reactive({
    shiny::req(length(parameter_choices) > 0)

    current <- selected_parameter()
    index <- match(current, parameter_choices)

    if (is.na(index)) {
      1L
    } else {
      index
    }
  })

  current_parameter <- shiny::reactive({
    shiny::req(length(parameter_choices) > 0)

    current <- selected_parameter()
    if (is.null(current) || !nzchar(current) || !current %in% parameter_choices) {
      parameter_choices[[1]]
    } else {
      current
    }
  })

  observeEvent(input$prev_parameter, {
    index <- current_parameter_index()
    if (index > 1L) {
      selected_parameter(parameter_choices[[index - 1L]])
    }
  })

  observeEvent(input$next_parameter, {
    index <- current_parameter_index()
    if (index < length(parameter_choices)) {
      selected_parameter(parameter_choices[[index + 1L]])
    }
  })

  observeEvent(input$parameter_select,
    {
      shiny::req(input$parameter_select)
      selected_parameter(input$parameter_select)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$prev_outeq, {
    index <- current_outeq_index()
    if (index > 1L) {
      selected_outeq(as.integer(outeq_choices[[index - 1L]]))
    }
  })

  observeEvent(input$next_outeq, {
    index <- current_outeq_index()
    if (index < length(outeq_choices)) {
      selected_outeq(as.integer(outeq_choices[[index + 1L]]))
    }
  })

  observeEvent(input$outeq_select,
    {
      shiny::req(input$outeq_select)
      selected_outeq(as.integer(input$outeq_select))
    },
    ignoreInit = TRUE
  )

  observeEvent(input$metrics_info, {
    opts <- report_options()
    bias_label <- metric_type_label(normalize_metric_method(opts$bias_method, "mwe"))
    imp_label <- metric_type_label(normalize_metric_method(opts$imp_method, "mwse"))

    shiny::showModal(
      shiny::modalDialog(
        title = "Bias and Imprecision Metrics",
        htmltools::tags$p(
          "Bias summarizes systematic over- or under-prediction. ",
          "Imprecision summarizes prediction scatter around observations"
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Current bias metric: "),
          bias_label
        ),
        htmltools::tags$p(
          htmltools::tags$strong("Current imprecision metric: "),
          imp_label
        ),
        htmltools::tags$p(
          "You can change these with  ",
          htmltools::tags$code("setPMoptions() "),
          "in the Pmetrics package."
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
  })

  observeEvent(input$close_app,
    {
      shiny::stopApp()
    },
    ignoreInit = TRUE
  )

  observeEvent(input$export_pdf_modal, {
    # Temporary testing switches (set in the R console):
    # options(PmetricsReports.pdf_test_platform = "windows")
    # options(PmetricsReports.pdf_test_platform = "linux")
    # options(PmetricsReports.pdf_test_platform = "macos")
    # options(PmetricsReports.pdf_test_no_latex = TRUE)
    # Reset with:
    # options(PmetricsReports.pdf_test_platform = NULL, PmetricsReports.pdf_test_no_latex = FALSE)
    default_name <- paste0("Pmetrics-report-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    export_status("")

    if (!has_latex_engine()) {
      inst <- pdf_engine_install_instructions()
      shiny::showModal(
        shiny::modalDialog(
          title = "LaTeX Engine Required for PDF Export",
          htmltools::tags$p(
            "A LaTeX engine is needed to create PDF exports."
          ),
          htmltools::tags$p(
            "Detected platform: ", htmltools::tags$strong(inst$platform), "."
          ),
          htmltools::tags$p("Run the following commands in R:"),
          htmltools::tags$pre(
            paste(inst$commands, collapse = "\n")
          ),
          if (nzchar(inst$note)) htmltools::tags$p(inst$note),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
      return(invisible(NULL))
    }

    shiny::showModal(
      shiny::modalDialog(
        title = "Export PDF Options",
        shiny::textInput(
          inputId = "pdf_filename",
          label = "File name",
          value = default_name
        ),
        shiny::textInput(
          inputId = "pdf_output_dir",
          label = "Output folder (local path)",
          value = path.expand("~/Downloads")
        ),
        shiny::checkboxGroupInput(
          inputId = "pdf_sections",
          label = "Include sections",
          choices = c(
            "Summary" = "summary",
            "Observed vs Predicted" = "obs_pred",
            "Prediction Metrics" = "metrics",
            "Residual Plots" = "residuals",
            "Parameters" = "parameters",
            "Cycle Info" = "cycle"
          ),
          selected = c("summary", "obs_pred", "metrics", "residuals", "parameters", "cycle")
        ),
        htmltools::tags$div(
          id = "pdf-render-status-live",
          class = "pdf-render-status pdf-render-status--active",
          style = "display:none;",
          "Rendering PDF..."
        ),
        shiny::uiOutput("pdf_render_status"),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "export_pdf_run",
            "Download PDF",
            class = "btn btn-success",
            onclick = "(function(){var el=document.getElementById('pdf-render-status-live'); if(el){el.style.display='block';}})();"
          )
        ),
        easyClose = TRUE
      )
    )
  })

  output$pdf_render_status <- shiny::renderUI({
    status <- export_status()
    if (!nzchar(status)) {
      return(NULL)
    }

    htmltools::tags$div(
      class = "pdf-render-status pdf-render-status--active",
      status
    )
  })

  observeEvent(input$export_pdf_run,
    {
      export_status("Rendering PDF...")

      tryCatch(
        {
          if (!requireNamespace("quarto", quietly = TRUE)) {
            cli::cli_abort(c(
              "x" = "The {.pkg quarto} package is required to export PDF reports.",
              "i" = "Install it, then try Export PDF again."
            ))
          }

          output_dir <- input$pdf_output_dir
          if (is.null(output_dir) || !nzchar(trimws(output_dir))) {
            output_dir <- path.expand("~/Downloads")
          }
          output_dir <- path.expand(trimws(output_dir))
          dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

          export_name <- input$pdf_filename
          if (is.null(export_name) || !nzchar(trimws(export_name))) {
            export_name <- paste0("pmetrics-report-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
          }
          if (!grepl("\\\\.pdf$", export_name, ignore.case = TRUE)) {
            export_name <- paste0(export_name, ".pdf")
          }

          work_dir <- tempfile("pmetrics_export_")
          dir.create(work_dir, recursive = TRUE)
          work_dir <- normalizePath(work_dir, mustWork = TRUE)
          on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

          qmd_path <- file.path(work_dir, "report.qmd")
          res_rds <- file.path(work_dir, "res.rds")

          template <- app_sys("app/templates/report_export.qmd")
          file.copy(template, qmd_path, overwrite = TRUE)
          saveRDS(res, res_rds)

          pkg_path <- tryCatch(golem::pkg_path(), error = function(e) "")
          selected_sections <- input$pdf_sections
          if (is.null(selected_sections) || !length(selected_sections)) {
            selected_sections <- c("summary", "obs_pred", "metrics", "residuals", "parameters", "cycle")
          }

          quarto::quarto_render(
            input = qmd_path,
            output_format = "pdf",
            execute_params = list(
              res_path = res_rds,
              pkg_path = pkg_path,
              report_generated_at = report_generated_at(),
              report_filename = export_name,
              outeq = current_outeq(),
              block = integer(0),
              pred_type = input$pred_type,
              icen = input$icen,
              cycle_mode = "all",
              cycle_gamlam_label = gamlam_label,
              cycle_ic_label = ic_label,
              parameter = current_parameter(),
              include_summary = "summary" %in% selected_sections,
              include_obs_pred = "obs_pred" %in% selected_sections,
              include_metrics = "metrics" %in% selected_sections,
              include_residuals = "residuals" %in% selected_sections,
              include_parameters = "parameters" %in% selected_sections,
              include_cycle = "cycle" %in% selected_sections
            ),
            quiet = FALSE
          )

          rendered <- file.path(work_dir, "report.pdf")
          if (!file.exists(rendered)) {
            stop("PDF export failed: 'report.pdf' was not produced in the work directory.")
          }

          final_path <- file.path(output_dir, export_name)
          file.copy(rendered, final_path, overwrite = TRUE)

          shiny::removeModal(session = session)
          export_status("")
          shiny::showNotification(
            paste("PDF saved to:", final_path),
            type = "message",
            duration = 6
          )
        },
        error = function(e) {
          export_status("")
          msg <- conditionMessage(e)
          shiny::showNotification(
            msg,
            type = "error",
            duration = NULL
          )
        }
      )
    },
    ignoreInit = TRUE
  )

  output$parameter_nav <- shiny::renderUI({
    if (!length(parameter_choices)) {
      return(htmltools::tags$em("No parameter data available."))
    }

    index <- current_parameter_index()

    htmltools::tags$div(
      class = "parameter-nav",
      shiny::actionButton(
        "prev_parameter",
        label = htmltools::HTML("&#9664;"),
        class = "btn btn-outline-secondary parameter-nav__button",
        disabled = if (index <= 1L) "disabled" else NULL
      ),
      htmltools::tags$div(
        class = "parameter-nav__label",
        shiny::selectInput(
          inputId = "parameter_select",
          label = NULL,
          choices = parameter_choices,
          selected = current_parameter(),
          width = "100%"
        ),
        htmltools::tags$span(
          class = "parameter-nav__position",
          paste(index, "of", length(parameter_choices))
        )
      ),
      shiny::actionButton(
        "next_parameter",
        label = htmltools::HTML("&#9654;"),
        class = "btn btn-outline-secondary parameter-nav__button",
        disabled = if (index >= length(parameter_choices)) "disabled" else NULL
      )
    )
  })

  output$summary_card <- shiny::renderUI({
    shiny::tagList(
      htmltools::tags$p(
        htmltools::tags$strong("Subjects: "), summary_info$subjects
      ),
      htmltools::tags$p(
        htmltools::tags$strong("Cycles: "), summary_info$cycles
      ),
      htmltools::tags$p(
        htmltools::tags$strong("Status: "), if (grepl("converg", summary_info$model_status, ignore.case = TRUE)) "Converged" else "Reached maximum cycles before convergence"
      )
    )
  })

  output$op_outeq_nav <- shiny::renderUI({
    if (length(outeq_choices) <= 1L) {
      return(NULL)
    }

    index <- current_outeq_index()

    htmltools::tags$div(
      class = "parameter-nav",
      shiny::actionButton(
        "prev_outeq",
        label = htmltools::HTML("&#9664;"),
        class = "btn btn-outline-secondary parameter-nav__button",
        disabled = if (index <= 1L) "disabled" else NULL
      ),
      htmltools::tags$div(
        class = "parameter-nav__label",
        shiny::selectInput(
          inputId = "outeq_select",
          label = "Output equation",
          choices = as.character(outeq_choices),
          selected = as.character(current_outeq()),
          width = "100%"
        ),
        htmltools::tags$span(
          class = "parameter-nav__position",
          paste(index, "of", length(outeq_choices))
        )
      ),
      shiny::actionButton(
        "next_outeq",
        label = htmltools::HTML("&#9654;"),
        class = "btn btn-outline-secondary parameter-nav__button",
        disabled = if (index >= length(outeq_choices)) "disabled" else NULL
      )
    )
  })

  output$op_plot_header <- shiny::renderUI({
    shiny::req(current_outeq())
    htmltools::tags$span(paste0("Outeq ", current_outeq()))
  })

  output$cycle_controls <- shiny::renderUI({
    htmltools::tags$div(
      class = "op-controls-row",
      htmltools::tags$div(
        class = "op-controls-group",
        shiny::radioButtons(
          inputId = "cycle_mode",
          label = NULL,
          choices = stats::setNames(
            c("neg2ll", ic_method, "gamlam", "norm_mean", "norm_median", "norm_sd"),
            c("-2*LL", ic_label, gamlam_label, "Normalized: Mean", "Median", "SD")
          ),
          selected = if (is.null(input$cycle_mode) || !input$cycle_mode %in% c("neg2ll", ic_method, "gamlam", "norm_mean", "norm_median", "norm_sd")) "neg2ll" else input$cycle_mode,
          inline = TRUE
        )
      )
    )
  })

  op_plot_obj <- shiny::reactive({
    shiny::req(current_outeq())

    build_op_plot(
      res = res,
      icen = input$icen,
      pred_type = input$pred_type,
      outeq = current_outeq(),
      block = integer(0),
      resid = isTRUE(input$show_residual)
    )
  })

  output$op_plot <- plotly::renderPlotly({
    plotly::ggplotly(op_plot_obj(), tooltip = c("x", "y"))
  })

  output$op_metrics <- shiny::renderUI({
    shiny::req(current_outeq())
    build_metrics_ui(
      res = res,
      icen = input$icen,
      pred_type = input$pred_type,
      outeq = current_outeq(),
      block = integer(0)
    )
  })

  output$final_table <- shiny::renderUI({
    shiny::req(current_parameter())
    if (is.null(res$final$ab)) {
      return(htmltools::tags$em("No PM_final object available."))
    }

    limits <- data.frame(res$final$ab)
    if (!"par" %in% names(limits)) {
      limits$par <- rownames(limits)
    }
    limits <- limits |>
      dplyr::filter(par == current_parameter()) |>
      dplyr::select(par, dplyr::everything())

    if (!nrow(limits)) {
      return(htmltools::tags$em("Selected parameter not found."))
    }

    html_table(limits, digits = 3)
  })

  output$final_plot <- plotly::renderPlotly({
    shiny::req(current_parameter())
    plotly::ggplotly(
      build_final_plot(res = res, parameter = current_parameter()),
      tooltip = c("x", "y")
    )
  })

  output$param_summary_table <- shiny::renderUI({
    tbl <- parameter_summary_table(res)

    if (!nrow(tbl)) {
      return(htmltools::tags$em("No parameter summary data available."))
    }

    html_table(tbl, digits = 3)
  })

  output$support_points_table <- shiny::renderUI({
    tbl <- support_points_table(res)

    if (!nrow(tbl)) {
      return(htmltools::tags$em("No support points available."))
    }

    html_table(tbl, digits = 3)
  })

  output$covariance_matrix_table <- shiny::renderUI({
    tbl <- covariance_matrix_table(res)

    if (!nrow(tbl)) {
      return(htmltools::tags$em("No covariance matrix available."))
    }

    html_table(tbl, digits = 4)
  })

  output$correlation_matrix_table <- shiny::renderUI({
    tbl <- correlation_matrix_table(res)

    if (!nrow(tbl)) {
      return(htmltools::tags$em("No correlation matrix available."))
    }

    html_table(tbl, digits = 4)
  })

  output$cycle_objective_plot <- plotly::renderPlotly({
    plotly::ggplotly(
      build_cycle_objective_plot(
        res,
        metric = if (is.null(input$cycle_mode)) "neg2ll" else input$cycle_mode,
        gamlam_label = gamlam_label
      ),
      tooltip = c("x", "y")
    )
  })

  output$cycle_objective_table <- shiny::renderUI({
    tbl <- cycle_objective_table(res)

    if (!nrow(tbl)) {
      return(htmltools::tags$em("No cycle values data available."))
    }

    html_table(tbl, digits = 4)
  })
}
