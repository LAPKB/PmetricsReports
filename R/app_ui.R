app_ui <- function(request) {
  res <- validate_report_result(get_report_result())

  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_fillable(
      title = report_browser_title(),
      theme = bslib::bs_theme(
        bootswatch = "flatly",
        primary = "#2c3e50",
        "card-border-radius" = "0.5rem"
      ),
      shiny::div(
        class = "app-banner",
        shiny::tags$span(class = "app-banner__title", "Pmetrics Report"),
        shiny::tags$div(
          class = "app-banner__actions",
          shiny::actionButton("export_pdf_modal", "Export PDF", class = "btn-success app-banner__export"),
          shiny::actionButton(
            "close_app",
            "Close",
            class = "btn-danger app-banner__close",
            onclick = "setTimeout(function(){window.close();}, 100);"
          )
        )
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          "Summary",
          bslib::card(
            bslib::card_header("Run summary"),
            bslib::card_body(shiny::uiOutput("summary_card"))
          )
        ),
        bslib::nav_panel(
          "Observed vs Predicted",
          shiny::fluidRow(
            shiny::column(
              width = 8,
              bslib::card(
                bslib::card_header(shiny::uiOutput("op_plot_header")),
                bslib::card_body(
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shiny::radioButtons(
                        "pred_type",
                        label = "Prediction type:",
                        choices = c("Posterior" = "post", "Population" = "pop"),
                        selected = "post",
                        inline = TRUE
                      )
                    ),
                    shiny::column(
                      width = 3,
                      shiny::radioButtons(
                        "icen",
                        label = "Prediction summary:",
                        choices = c("Median" = "median", "Mean" = "mean"),
                        selected = "median",
                        inline = TRUE
                      )
                    ),
                    shiny::column(
                      width = 2,
                      shiny::checkboxInput(
                        "show_residual",
                        label = "Residuals",
                        value = FALSE
                      )
                    )
                  ),
                  shiny::uiOutput("op_outeq_nav"),
                  plotly::plotlyOutput("op_plot", height = "460px")
                )
              )
            ),
            shiny::column(
              width = 4,
              bslib::card(
                bslib::card_header(
                  htmltools::tags$span(
                    "Prediction metrics ",
                    shiny::actionLink(
                      inputId = "metrics_info",
                      label = htmltools::HTML("&#9432;"),
                      class = "btn btn-sm btn-outline-secondary metrics-info-btn",
                      title = "Information about bias and imprecision metrics"
                    )
                  )
                ),
                bslib::card_body(shiny::uiOutput("op_metrics"))
              )
            )
          )
        ),
        bslib::nav_panel(
          "Parameters",
          bslib::navset_card_tab(
            bslib::nav_panel(
              "Marginal Plots",
              bslib::card(
                bslib::card_header("Parameter marginal distributions"),
                bslib::card_body(
                  shiny::uiOutput("parameter_nav"),
                  plotly::plotlyOutput("final_plot", height = "420px")
                )
              )
            ),
            bslib::nav_panel(
              "Summaries",
              bslib::card(
                bslib::card_header("Population parameter summaries"),
                bslib::card_body(shiny::uiOutput("param_summary_table"))
              )
            ),
            bslib::nav_panel(
              "Support Points",
              bslib::card(
                bslib::card_header("Support points for parameter distributions"),
                bslib::card_body(shiny::uiOutput("support_points_table"))
              )
            ),
            bslib::nav_panel(
              "Covariances",
              bslib::card(
                bslib::card_header("Population parameter covariance matrix"),
                bslib::card_body(shiny::uiOutput("covariance_matrix_table"))
              )
            ),
            bslib::nav_panel(
              "Correlations",
              bslib::card(
                bslib::card_header("Population parameter correlation matrix"),
                bslib::card_body(shiny::uiOutput("correlation_matrix_table"))
              )
            )
          )
        ),
        bslib::nav_panel(
          "Cycle Info",
          bslib::navset_card_tab(
            bslib::nav_panel(
              "Plot",
              bslib::card(
                bslib::card_header("Value by cycle"),
                bslib::card_body(
                  shiny::uiOutput("cycle_controls"),
                  plotly::plotlyOutput("cycle_objective_plot", height = "520px")
                )
              )
            ),
            bslib::nav_panel(
              "Values",
              bslib::card(
                bslib::card_header("Cycle Values"),
                bslib::card_body(shiny::uiOutput("cycle_objective_table"))
              )
            )
          )
        )
      )
    )
  )
}

#' Add external resources.
#' @noRd
#' @importFrom golem add_resource_path bundle_resources favicon
#' @import shiny
#'
# Internal helper for wiring static resources into the app UI.
golem_add_external_resources <- function() {
  www_path <- app_sys("app/www")
  golem::add_resource_path("www", www_path)
  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = www_path,
      app_title = report_browser_title()
    )
  )
}
