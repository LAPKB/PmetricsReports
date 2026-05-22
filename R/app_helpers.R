app_sys <- function(...) {
  path <- system.file(..., package = "PmetricsReports")
  if (identical(path, "")) {
    path <- file.path(golem::pkg_path(), ...)
  }
  path
}

pmoptions_user_file <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])

  opt_dir <- if (sysname %in% c("darwin", "linux")) {
    path.expand("~/.PMopts")
  } else if (sysname == "windows") {
    file.path(Sys.getenv("APPDATA"), "PMopts")
  } else {
    path.expand("~/.PMopts")
  }

  file.path(opt_dir, "PMoptions.json")
}

pmoptions_default_file <- function() {
  installed <- system.file("options", "PMoptions.json", package = "PmetricsReports")
  if (nzchar(installed)) {
    return(installed)
  }

  source_path <- file.path(golem::pkg_path(), "inst", "options", "PMoptions.json")
  if (file.exists(source_path)) {
    return(source_path)
  }

  ""
}

read_pmoptions_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(list())
  }

  tryCatch(
    jsonlite::read_json(path = path, simplifyVector = TRUE),
    error = function(e) list()
  )
}

report_options <- function() {
  defaults <- read_pmoptions_file(pmoptions_default_file())

  if (!length(defaults)) {
    defaults <- list(
      sep = ",",
      dec = ".",
      digits = 2,
      show_metrics = TRUE,
      bias_method = "percent_mwe",
      imp_method = "percent_rmbawse",
      ic_method = "aic",
      report_template = "plotly",
      date_format = "%m/%d/%y",
      update_check = "weekly",
      update_timeout = 1,
      backend = "rust",
      model_template_path = ""
    )
  }

  user_opts <- read_pmoptions_file(pmoptions_user_file())
  if (!length(user_opts)) {
    return(defaults)
  }

  synced <- defaults
  shared_names <- intersect(names(defaults), names(user_opts))
  for (nm in shared_names) {
    synced[[nm]] <- user_opts[[nm]]
  }

  synced
}

normalize_metric_method <- function(method, fallback) {
  if (is.null(method) || !length(method)) {
    return(fallback)
  }

  method <- tolower(trimws(as.character(method)[[1]]))
  if (!nzchar(method)) {
    return(fallback)
  }

  method <- sub("^percent_", "", method)
  if (!nzchar(method)) {
    fallback
  } else {
    method
  }
}

resolve_metric_method <- function(method, fallback, available) {
  method <- normalize_metric_method(method, fallback)
  available <- tolower(available)

  if (method %in% available) {
    return(method)
  }

  if (fallback %in% available) {
    return(fallback)
  }

  if (length(available)) {
    return(available[[1]])
  }

  fallback
}

resolve_cycle_ic_method <- function(res, preferred = report_options()$ic_method) {
  if (is.null(preferred) || !length(preferred)) {
    preferred <- "aic"
  }

  preferred <- tolower(trimws(as.character(preferred)[[1]]))
  if (!preferred %in% c("aic", "bic")) {
    preferred <- "aic"
  }

  available <- character(0)
  if (!is.null(res$cycle$objective)) {
    available <- tolower(names(as.data.frame(res$cycle$objective)))
  }

  if (preferred %in% available) {
    return(preferred)
  }

  if ("aic" %in% available) {
    return("aic")
  }

  if ("bic" %in% available) {
    return("bic")
  }

  preferred
}

get_report_result <- function() {
  golem::get_golem_options("res")
}

validate_report_result <- function(res) {
  if (!inherits(res, "PM_result")) {
    cli::cli_abort(c(
      "x" = "PmetricsReports expects a {.cls PM_result} object.",
      "i" = "Launch it from {.fn PM_report} after a run completes."
    ))
  }

  res
}

pdf_test_sysname <- function() {
  simulated <- getOption("PmetricsReports.pdf_test_platform", NULL)
  if (is.null(simulated) || !nzchar(simulated)) {
    return(tolower(Sys.info()[["sysname"]]))
  }

  simulated <- tolower(simulated)
  if (simulated %in% c("mac", "macos", "darwin")) {
    return("darwin")
  }
  if (simulated %in% c("linux", "windows")) {
    return(simulated)
  }

  tolower(Sys.info()[["sysname"]])
}

pdf_engine_suggestion <- function() {
  sysname <- pdf_test_sysname()

  switch(
    sysname,
    darwin = c(
      "x" = "A LaTeX engine is required to render PDF reports.",
      "i" = "On macOS, install TinyTeX with {.code tinytex::install_tinytex()} or install MacTeX."
    ),
    linux = c(
      "x" = "A LaTeX engine is required to render PDF reports.",
      "i" = "On Linux, install TinyTeX with {.code tinytex::install_tinytex()} or install TeX Live from your distribution's packages."
    ),
    windows = c(
      "x" = "A LaTeX engine is required to render PDF reports.",
      "i" = "On Windows, install TinyTeX with {.code tinytex::install_tinytex()} or install MiKTeX."
    ),
    c(
      "x" = "A LaTeX engine is required to render PDF reports.",
      "i" = "Install TinyTeX with {.code tinytex::install_tinytex()} or another LaTeX distribution for your platform."
    )
  )
}

pdf_engine_install_instructions <- function() {
  sysname <- pdf_test_sysname()

  platform <- switch(
    sysname,
    darwin = "macOS",
    linux = "Linux",
    windows = "Windows",
    "your platform"
  )

  note <- switch(
    sysname,
    darwin = "If prompted on macOS, allow required command line tools to install.",
    linux = "If required on Linux, install system build tools first using your distribution packages.",
    windows = "If prompted on Windows, allow TinyTeX to update PATH during installation.",
    ""
  )

  list(
    platform = platform,
    commands = c(
      "install.packages(\"tinytex\")",
      "tinytex::install_tinytex()"
    ),
    note = note
  )
}

has_latex_engine <- function() {
  if (isTRUE(getOption("PmetricsReports.pdf_test_no_latex", FALSE))) {
    return(FALSE)
  }

  any(nzchar(Sys.which(c("lualatex", "xelatex", "pdflatex", "latexmk"))))
}

available_outeq <- function(res) {
  if (is.null(res$op$data) || !nrow(res$op$data)) {
    return(integer(0))
  }

  sort(unique(stats::na.omit(res$op$data$outeq)))
}

available_blocks <- function(res) {
  if (is.null(res$op$data) || !nrow(res$op$data)) {
    return(integer(0))
  }

  if (!"block" %in% names(res$op$data)) {
    return(integer(0))
  }

  sort(unique(stats::na.omit(res$op$data$block)))
}

available_parameters <- function(res) {
  if (is.null(res$final$popPoints)) {
    return(character(0))
  }

  setdiff(names(res$final$popPoints), "prob")
}

summary_table <- function(res) {
  data.frame(
    subjects = if (!is.null(res$final$nsub)) res$final$nsub else NA_integer_,
    cycles = if (!is.null(res$cycle$objective$cycle)) max(res$cycle$objective$cycle, na.rm = TRUE) else NA_integer_,
    model_status = if (!is.null(res$cycle$data$status)) res$cycle$data$status else NA_character_,
    stringsAsFactors = FALSE
  )
}

html_table <- function(data, digits = 3) {
  format_cell <- function(x) {
    if (is.numeric(x)) {
      formatC(x, digits = digits, format = "fg", flag = "#")
    } else if (is.logical(x)) {
      ifelse(x, "TRUE", "FALSE")
    } else {
      as.character(x)
    }
  }

  rows <- apply(data, 1, function(row) {
    htmltools::tags$tr(
      lapply(row, function(cell) htmltools::tags$td(format_cell(cell)))
    )
  })

  htmltools::tags$table(
    class = "table table-sm table-striped table-hover",
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(data), htmltools::tags$th))),
    htmltools::tags$tbody(rows)
  )
}

op_metrics_table <- function(res, icen, pred_type, outeq, block) {
  data <- res$op$data |>
    dplyr::filter(icen == !!icen, pred.type == !!pred_type, outeq %in% !!outeq)

  if (length(block) > 0) {
    data <- data |>
      dplyr::filter(block %in% !!block)
  }

  if (!nrow(data)) {
    return(data.frame())
  }

  compute_op_metrics(data)
}

compute_op_metrics <- function(data) {
  if (!nrow(data)) {
    return(data.frame())
  }

  N <- sum(!is.na(data$obs))
  mean_obs <- mean(data$obs, na.rm = TRUE)
  wmean_obs <- sum(data$obs / data$obsSD, na.rm = TRUE) / N

  mae <- sum(data$d, na.rm = TRUE) / N
  percent_mae <- mean(data$d / data$obs, na.rm = TRUE) * 100

  mwe <- sum(data$wd, na.rm = TRUE) / N
  percent_mwe <- sum(data$wd, na.rm = TRUE) / sum(data$obs / data$obsSD, na.rm = TRUE) * 100

  mse <- sum(data$ds, na.rm = TRUE) / N
  percent_mse <- mean(data$ds, na.rm = TRUE) / (mean_obs^2) * 100

  mwse <- sum(data$wds, na.rm = TRUE) / N
  percent_mwse <- mwse / (wmean_obs^2) * 100

  rmse <- sqrt(mse)
  percent_rmse <- rmse / mean_obs * 100

  mbase <- mse - mae^2
  percent_mbase <- mbase / (mean_obs^2) * 100

  mbawse <- mwse - mwe^2
  percent_mbawse <- mbawse / (wmean_obs^2) * 100

  rmbawse <- sqrt(mbawse)
  percent_rmbawse <- sqrt(mbawse) * 100 / (sum(data$obs / data$obsSD, na.rm = TRUE) / N)

  data.frame(
    Type = c("mae", "mwe", "mse", "mwse", "rmse", "mbase", "mbawse", "rmbawse"),
    Absolute = c(mae, mwe, mse, mwse, rmse, mbase, mbawse, rmbawse),
    Percent = c(percent_mae, percent_mwe, percent_mse, percent_mwse, percent_rmse, percent_mbase, percent_mbawse, percent_rmbawse),
    stringsAsFactors = FALSE
  )
}

build_op_plot <- function(res, icen, pred_type, outeq, block, resid = FALSE) {
  data <- res$op$data |>
    dplyr::filter(icen == !!icen, pred.type == !!pred_type, outeq %in% !!outeq)

  if (length(block) > 0) {
    data <- data |>
      dplyr::filter(block %in% !!block)
  }

  if (!nrow(data)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No PM_op data available for the current selection."))
  }

  if (!resid) {
    subtitle <- if (identical(pred_type, "pop")) "Population" else "Posterior"
    ggplot2::ggplot(data, ggplot2::aes(x = pred, y = obs)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
      ggplot2::geom_point(shape = 21, colour = "black", fill = "#e74c3c", alpha = 0.85, size = 2.2) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        x = "Predicted",
        y = "Observed",
        subtitle = subtitle
      )
  } else {
    ggplot2::ggplot(data, ggplot2::aes(x = time, y = wd)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "black") +
      ggplot2::geom_point(shape = 21, colour = "black", fill = "#e74c3c", alpha = 0.85, size = 2.2) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        x = "Time",
        y = "Weighted prediction error",
        subtitle = if (identical(pred_type, "pop")) "Population" else "Posterior"
      )
  }
}

build_residual_conc_plot <- function(res, icen, pred_type, outeq, block) {
  data <- res$op$data |>
    dplyr::filter(icen == !!icen, pred.type == !!pred_type, outeq %in% !!outeq)

  if (length(block) > 0) {
    data <- data |>
      dplyr::filter(block %in% !!block)
  }

  if (!nrow(data)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No PM_op data available for the current selection."))
  }

  ggplot2::ggplot(data, ggplot2::aes(x = obs, y = wd)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "black") +
    ggplot2::geom_point(shape = 21, colour = "black", fill = "#e74c3c", alpha = 0.85, size = 2.2) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Observed concentration",
      y = "Weighted prediction error",
      subtitle = if (identical(pred_type, "pop")) "Population" else "Posterior"
    )
}

build_final_plot <- function(res, parameter) {
  if (is.null(parameter) || !nzchar(parameter)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Choose a parameter to display."))
  }

  ab <- data.frame(res$final$ab)

  if (!is.null(res$final$popPoints)) {
    data <- res$final$popPoints
    if (is.null(data) || !nrow(data) || !parameter %in% names(data)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Selected parameter not found."))
    }

    limits <- ab[ab$par == parameter, , drop = FALSE]
    selected <- data.frame(value = data[[parameter]], prob = data$prob)

    ggplot2::ggplot(selected, ggplot2::aes(x = value, y = prob)) +
      ggplot2::geom_segment(ggplot2::aes(xend = value, yend = 0), colour = "#2c3e50") +
      ggplot2::geom_vline(data = limits, ggplot2::aes(xintercept = min), linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_vline(data = limits, ggplot2::aes(xintercept = max), linetype = "dashed", alpha = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        x = "Parameter value",
        y = "Probability"
      )
  } else {
    mean <- res$final$popMean[[parameter]]
    sd <- res$final$popSD[[parameter]]
    limits <- ab[ab$par == parameter, , drop = FALSE]
    if (length(mean) == 0 || length(sd) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Selected parameter not found."))
    }

    x <- seq(limits$min[1], limits$max[1], length.out = 200)
    data <- data.frame(x = x, y = stats::dnorm(x, mean = mean, sd = sd))

    ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(colour = "#2c3e50") +
      ggplot2::geom_vline(data = limits, ggplot2::aes(xintercept = min), linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_vline(data = limits, ggplot2::aes(xintercept = max), linetype = "dashed", alpha = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        x = "Parameter value",
        y = "Density"
      )
  }
}

parameter_summary_table <- function(res) {
  if (is.null(res$final$popMean) || is.null(res$final$popSD)) {
    return(data.frame())
  }

  params <- names(res$final$popMean)
  if ("prob" %in% params) params <- setdiff(params, "prob")

  data <- data.frame(
    Parameter = params,
    Mean = unlist(res$final$popMean[params]),
    Median = if (!is.null(res$final$popMed)) unlist(res$final$popMed[params]) else NA_real_,
    SD = unlist(res$final$popSD[params]),
    `CV%` = if (!is.null(res$final$popCV)) unlist(res$final$popCV[params]) else NA_real_,
    `% Shrinkage` = if (!is.null(res$final$shrinkage)) unlist(res$final$shrinkage[params]) else NA_real_,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = NULL
  )

  data
}

support_points_table <- function(res) {
  if (is.null(res$final$popPoints)) {
    return(data.frame())
  }

  data.frame(res$final$popPoints, stringsAsFactors = FALSE, row.names = NULL)
}

covariance_matrix_table <- function(res) {
  if (is.null(res$final$popCov)) {
    return(data.frame())
  }

  data <- as.data.frame(res$final$popCov)
  data
}

correlation_matrix_table <- function(res) {
  if (is.null(res$final$popCor)) {
    return(data.frame())
  }

  data <- as.data.frame(res$final$popCor)
  data
}

cycle_objective_table <- function(res) {
  if (is.null(res$cycle$objective)) {
    return(data.frame())
  }

  data <- as.data.frame(res$cycle$objective)
  if (!"cycle" %in% names(data) || !"neg2ll" %in% names(data)) {
    return(data.frame())
  }

  ic_method <- resolve_cycle_ic_method(res)

  ic_name <- names(data)[tolower(names(data)) == ic_method][1]
  if (is.na(ic_name) || !nzchar(ic_name)) {
    ic_name <- if (ic_method == "aic") "aic" else "bic"
    if (!ic_name %in% names(data)) {
      return(data.frame())
    }
  }

  gamlam_type <- tryCatch({
    as.character(res$cycle$data$gamlam$type[[1]])
  }, error = function(e) {
    tryCatch(as.character(res$cycle$gamlam$type[[1]]), error = function(e2) NA_character_)
  })

  gamlam_label <- if (!is.na(gamlam_type) && identical(gamlam_type, "Additive")) {
    "Lambda"
  } else {
    "Gamma"
  }

  gamlam_values <- rep(NA_real_, nrow(data))
  gamlam_df <- tryCatch(as.data.frame(res$cycle$gamlam), error = function(e) NULL)
  if (!is.null(gamlam_df) && all(c("cycle", "value") %in% names(gamlam_df))) {
    idx <- match(data$cycle, gamlam_df$cycle)
    gamlam_values <- gamlam_df$value[idx]
  }

  data.frame(
    Cycle = as.character(as.integer(data$cycle)),
    `-2*LL` = data$neg2ll,
    stats::setNames(list(data[[ic_name]]), toupper(ic_method)),
    stats::setNames(list(gamlam_values), gamlam_label),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = NULL
  )
}

build_cycle_objective_plot <- function(res, metric = "neg2ll", gamlam_label = "Gamma") {
  if (is.null(res$cycle$objective)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No cycle objective data available."))
  }

  data <- as.data.frame(res$cycle$objective)
  if (!"cycle" %in% names(data)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Cycle objective data incomplete."))
  }

  metric <- tolower(metric)

  if (metric == "gamlam") {
    if (is.null(res$cycle$gamlam)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No gamma/lambda cycle data available."))
    }

    gamlam_df <- as.data.frame(res$cycle$gamlam)
    if (!all(c("cycle", "value") %in% names(gamlam_df))) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Gamma/lambda cycle data incomplete."))
    }

    ggplot2::ggplot(gamlam_df, ggplot2::aes(x = cycle, y = value)) +
      ggplot2::geom_line(colour = "#2c3e50") +
      ggplot2::geom_point(colour = "#2c3e50", fill = "#e74c3c", shape = 21, size = 2) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        x = "Cycle",
        y = gamlam_label
      )
  } else if (metric %in% c("norm_mean", "norm_median", "norm_sd")) {
    norm_source <- sub("^norm_", "", metric)
    if (!norm_source %in% c("mean", "median", "sd")) {
      norm_source <- "mean"
    }

    norm_tbl <- res$cycle[[norm_source]]
    if (is.null(norm_tbl)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No normalized cycle data available."))
    }

    norm_df <- as.data.frame(norm_tbl)
    if (!"cycle" %in% names(norm_df)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "Normalized cycle data incomplete."))
    }

    param_cols <- setdiff(names(norm_df), "cycle")
    if (!length(param_cols)) {
      return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No parameter columns available for normalization plot."))
    }

    long_df <- do.call(
      rbind,
      lapply(param_cols, function(par) {
        vals <- norm_df[[par]]
        base_val <- vals[[1]]
        if (is.na(base_val) || identical(base_val, 0)) {
          base_val <- 1
        }

        data.frame(
          cycle = norm_df$cycle,
          parameter = par,
          value = vals / base_val,
          stringsAsFactors = FALSE
        )
      })
    )

    ggplot2::ggplot(long_df, ggplot2::aes(x = cycle, y = value, colour = parameter)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        x = "Cycle",
        y = "Normalized value",
        colour = "Parameter",
        subtitle = paste0("Normalized by ", tools::toTitleCase(norm_source))
      )
  } else {
    if (!metric %in% tolower(names(data))) {
      metric <- "neg2ll"
    }

    metric_name <- names(data)[tolower(names(data)) == metric][1]
    if (is.na(metric_name) || !nzchar(metric_name)) {
      metric_name <- "neg2ll"
    }

    data$criterion_value <- data[[metric_name]]

    y_label <- switch(
      tolower(metric_name),
      neg2ll = "-2 Log Likelihood",
      aic = "AIC",
      bic = "BIC",
      metric_name
    )

    ggplot2::ggplot(data, ggplot2::aes(x = cycle, y = criterion_value)) +
      ggplot2::geom_line(colour = "#2c3e50") +
      ggplot2::geom_point(colour = "#2c3e50", fill = "#e74c3c", shape = 21, size = 2) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        x = "Cycle",
        y = y_label
      )
  }
}

split_metrics_by_type <- function(res, icen, pred_type, outeq, block) {
  data <- res$op$data |>
    dplyr::filter(icen == !!icen, pred.type == !!pred_type, outeq %in% !!outeq)

  if (length(block) > 0) {
    data <- data |>
      dplyr::filter(block %in% !!block)
  }

  if (!nrow(data)) {
    return(list(
      bias = data.frame(),
      imprecision = data.frame(),
      bias_method = NA_character_,
      imp_method = NA_character_
    ))
  }

  metrics <- compute_op_metrics(data)
  opts <- report_options()
  bias_method <- resolve_metric_method(opts$bias_method, "mwe", metrics$Type)
  imp_method <- resolve_metric_method(opts$imp_method, "mwse", metrics$Type)

  bias_type <- normalize_metric_method(bias_method, "mwe")
  imp_type <- normalize_metric_method(imp_method, "mwse")

  bias <- metrics[metrics$Type == bias_type, , drop = FALSE]
  imprecision <- metrics[metrics$Type == imp_type, , drop = FALSE]

  if (nrow(bias)) {
    bias$Type <- bias_method
  }
  if (nrow(imprecision)) {
    imprecision$Type <- imp_method
  }

  list(
    bias = bias,
    imprecision = imprecision,
    bias_method = bias_method,
    imp_method = imp_method
  )
}

metric_type_label <- function(type_code) {
  labels <- c(
    mae = "Mean absolute error",
    mwe = "Mean weighted error",
    mse = "Mean squared error",
    mwse = "Mean weighted squared error",
    rmse = "Root mean squared error",
    mbase = "Mean bias-adjusted squared error",
    mbawse = "Mean bias-adjusted weighted squared error",
    rmbawse = "Root mean bias-adjusted weighted squared error"
  )

  if (type_code %in% names(labels)) labels[[type_code]] else type_code
}

prediction_metrics_table <- function(res, icen, pred_type, outeq, block) {
  split <- split_metrics_by_type(res, icen, pred_type, outeq, block)

  if (!nrow(split$bias) && !nrow(split$imprecision)) {
    return(data.frame())
  }

  build_row <- function(df, row_name) {
    if (!nrow(df)) {
      return(data.frame(
        Type = NA_character_,
        Absolute = NA_real_,
        Percent = NA_real_,
        row.names = row_name,
        check.names = FALSE
      ))
    }

    type_code <- sub("^percent_", "", df$Type[[1]])
    out <- data.frame(
      Type = metric_type_label(type_code),
      Absolute = df$Absolute[[1]],
      Percent = df$Percent[[1]],
      row.names = row_name,
      check.names = FALSE
    )
    out
  }

  rbind(
    build_row(split$bias, "Bias"),
    build_row(split$imprecision, "Imprecision")
  )
}

prediction_metrics_display_table <- function(res, icen, pred_type, outeq, block) {
  split <- split_metrics_by_type(res, icen, pred_type, outeq, block)

  if (!nrow(split$bias) && !nrow(split$imprecision)) {
    return(data.frame())
  }

  build_metric_row <- function(df, row_name) {
    if (!nrow(df)) {
      return(list(
        metric = row_name,
        absolute = NA_real_,
        percent = NA_real_
      ))
    }

    type_code <- sub("^percent_", "", df$Type[[1]])
    list(
      metric = row_name,
      absolute = df$Absolute[[1]],
      percent = df$Percent[[1]]
    )
  }

  bias_row <- build_metric_row(split$bias, "Bias")
  imp_row <- build_metric_row(split$imprecision, "Imprecision")

  data.frame(
    Metric = c(bias_row$metric, imp_row$metric),
    Absolute = c(bias_row$absolute, imp_row$absolute),
    Percent = c(bias_row$percent, imp_row$percent),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = NULL
  )
}

build_metrics_ui <- function(res, icen, pred_type, outeq, block) {
  metrics_tbl <- prediction_metrics_display_table(res, icen, pred_type, outeq, block)
  if (!nrow(metrics_tbl)) {
    return(htmltools::tags$em("No metrics available for the current selection."))
  }

  htmltools::tags$div(
    class = "prediction-metrics-table-wrapper",
    htmltools::tags$table(
      class = "table table-sm table-striped table-hover",
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th("Metric"),
          htmltools::tags$th("Absolute"),
          htmltools::tags$th("Percent")
        )
      ),
      htmltools::tags$tbody(
        lapply(seq_len(nrow(metrics_tbl)), function(i) {
          htmltools::tags$tr(
            htmltools::tags$td(metrics_tbl$Metric[i]),
            htmltools::tags$td(formatC(metrics_tbl$Absolute[i], digits = 3, format = "fg", flag = "#")),
            htmltools::tags$td(formatC(metrics_tbl$Percent[i], digits = 3, format = "fg", flag = "#"))
          )
        })
      )
    )
  )
}
