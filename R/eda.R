#' Model results caption spec
#'
#' Controls what `eda_qt_rr_plot()` and `eda_qtc_comparison_plot()` report in
#' the plot caption about the fitted QT vs RR slope.
#'
#' @param slope Logical, show the slope estimate with its confidence interval
#' @param ci Numeric confidence interval level for the slope (default: 0.90)
#' @param pvalue Logical, show the slope p-value on a second caption line
#' @param eps Numeric, p-values below this are shown as "< eps"
#' @param digits Integer, decimal places for the slope, CI, and p-value
#'   (zero-padded, so 0.1 prints as 0.100 with the default of 3)
#'
#' @return A list of class `cqtkit_model_results_spec`
#' @export
#'
#' @examples
#' model_results_spec(pvalue = TRUE)
#' model_results_spec(ci = 0.95, digits = 4)
model_results_spec <- function(
  slope = TRUE,
  ci = 0.90,
  pvalue = FALSE,
  eps = 0.001,
  digits = 3
) {
  checkmate::assertFlag(slope)
  checkmate::assertNumber(ci, lower = 0, upper = 1)
  checkmate::assertFlag(pvalue)
  checkmate::assertNumber(eps, lower = 0, upper = 1)
  checkmate::assertInt(digits, lower = 0)
  structure(
    list(slope = slope, ci = ci, pvalue = pvalue, eps = eps, digits = digits),
    class = "cqtkit_model_results_spec"
  )
}

#' EDA QT RR Plot
#'
#' Plots QT against RR.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param rr_col An unquoted column name for RR measurements
#' @param qt_col An unquoted column name for QT measurements
#' @param id_col An unquoted column name for subject ID
#' @param trt_col An unquoted column name for treatment group
#' @param model_type Lm or lme, which model to fit for showing on plot
#' @param show_model_results A `model_results_spec()` controlling the slope,
#'   CI level, p-value, and digits shown in the caption. `TRUE` uses the
#'   default spec, `FALSE` shows no model results
#' @param method Method for nlme::lme fitting (ML or REML)
#' @param remove_rr_iiv Logical, whether to remove IIV on RR slope
#' @param style A ggstylekit::style_spec() object
#'
#' @return A scatter plot of QT vs RR with optional regression line and slope estimate caption
#' @export
#'
#' @examples
#'
#' eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, ID, model_type = "lme")
eda_qt_rr_plot <- function(
  data,
  rr_col,
  qt_col,
  id_col = NULL,
  trt_col = NULL,
  model_type = c("lm", "lme"),
  show_model_results = model_results_spec(),
  method = "REML",
  remove_rr_iiv = FALSE,
  style = ggstylekit::style_spec()
) {
  checkmate::assertDataFrame(data)
  spec <- as_model_results_spec(show_model_results)

  qt <- rlang::enquo(qt_col)
  rr <- rlang::enquo(rr_col)
  id <- rlang::enquo(id_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(qt, rr, id, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  model_type <- match.arg(model_type)
  if (rlang::quo_is_null(id) && (model_type == "lme") && !is.null(spec)) {
    stop(
      "Must supply id_col if fitting LME model. Otherwise use model_type = 'lm'"
    )
  }

  plot_data <- data

  plot_data$.trt_group <- factor(
    if (!rlang::quo_is_null(trt)) {
      rlang::eval_tidy(trt, data)
    } else {
      "All"
    }
  )

  qt_rr_plot <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = !!rr, y = !!qt)) +
    ggplot2::geom_point(trt_color_mapping(trt))

  if (model_type == "lm" && !is.null(spec)) {
    lm_results <- compute_lm_fit_df(
      data,
      xdata_col = !!rr,
      ydata_col = !!qt,
      conf_int = spec$ci
    )

    label <- format_model_results(
      "Linear Regression",
      lm_results$slope,
      lm_results$slope_ci_lower,
      lm_results$slope_ci_upper,
      lm_results$p_value_slope,
      spec
    )

    plot_data$predictions <- lm_results$intercept +
      lm_results$slope * rlang::eval_tidy(rr, plot_data)

    qt_rr_plot <- qt_rr_plot +
      ggplot2::geom_line(
        data = plot_data,
        ggplot2::aes(y = .data$predictions),
        color = "black"
      )
  } else if ((model_type == "lme") && !is.null(spec)) {
    lme_mod <- fit_qtc_linear_model(
      data,
      qt_col = !!qt,
      rr_col = !!rr,
      id_col = !!id,
      method = method,
      remove_rr_iiv
    )

    estimates <- compute_model_fit_parameters(
      lme_mod,
      conf_int = spec$ci,
      trt_col_name = name_quo_if_not_null(trt),
      id_col_name = name_quo_if_not_null(id)
    )

    slope <- estimates |>
      dplyr::filter(.data$Parameters == rlang::quo_name(rr)) |>
      dplyr::pull(.data$Value)

    slope_ci_lower <- estimates |>
      dplyr::filter(.data$Parameters == rlang::quo_name(rr)) |>
      dplyr::pull(.data$CIl)

    slope_ci_upper <- estimates |>
      dplyr::filter(.data$Parameters == rlang::quo_name(rr)) |>
      dplyr::pull(.data$CIu)

    slope_p_value <- estimates |>
      dplyr::filter(.data$Parameters == rlang::quo_name(rr)) |>
      dplyr::pull(.data$`p-value`)

    label <- format_model_results(
      "Linear Mixed Effects",
      slope,
      slope_ci_lower,
      slope_ci_upper,
      slope_p_value,
      spec
    )

    plot_data$predictions <- stats::predict(lme_mod, level = 0)

    qt_rr_plot <- qt_rr_plot +
      ggplot2::geom_line(
        data = plot_data,
        ggplot2::aes(y = .data$predictions),
        color = "black"
      )
  } else {
    label <- NULL
  }

  if (!is.null(label)) {
    qt_rr_plot <- qt_rr_plot +
      ggplot2::labs(caption = label)
  }

  qt_rr_plot <- cqtkit_style_plot(
    qt_rr_plot,
    style,
    xlabel = "RR (ms)",
    ylabel = "QT (ms)"
  )
  return(qt_rr_plot)
}

#' EDA QTc Comparison Plot
#'
#' Plots different corrections of QT against RR to compare which to use.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param rr_col An unquoted column name for RR measurements
#' @param qt_col An unquoted column name for QT measurements
#' @param qtcb_col An unquoted column name for Bazett corrected QT data
#' @param qtcf_col An unquoted column name for Fridericia corrected QT data
#' @param qtcp_col An unquoted column name for QTc measurements
#' @param id_col An unquoted column name for subject ID
#' @param trt_col An unquoted column name for treatment group data
#' @param model_type Lm or lme, which model to fit for showing on plot
#' @param show_model_results A `model_results_spec()` controlling the slope,
#'   CI level, p-value, and digits shown in the caption. `TRUE` uses the
#'   default spec, `FALSE` shows no model results
#' @param method Method for nlme::lme fitting (ML or REML)
#' @param remove_rr_iiv Logical, whether to remove IIV on RR slope
#' @param style A ggstylekit::style_spec() object
#'
#' @return A multi-panel plot comparing QT, QTcB, QTcF, and QTcP corrections against RR
#' @export
#'
#' @examples
#'
#' eda_qtc_comparison_plot(
#'   cqtkit_data_verapamil,
#'   RR,
#'   QT,
#'   QTCB,
#'   QTCF,
#'   id_col = ID,
#'   trt_col = TRTG,
#'   model_type = "lme",
#'   show_model_results = TRUE,
#'   remove_rr_iiv = TRUE)
eda_qtc_comparison_plot <- function(
  data,
  rr_col,
  qt_col,
  qtcb_col,
  qtcf_col,
  qtcp_col = NULL,
  id_col = NULL,
  trt_col = NULL,
  model_type = c("lm", "lme"),
  show_model_results = model_results_spec(),
  method = "REML",
  remove_rr_iiv = FALSE,
  style = ggstylekit::style_spec()
) {
  checkmate::assertDataFrame(data)
  spec <- as_model_results_spec(show_model_results)

  rr <- rlang::enquo(rr_col)
  qt <- rlang::enquo(qt_col)
  id <- rlang::enquo(id_col)
  qtcb <- rlang::enquo(qtcb_col)
  qtcf <- rlang::enquo(qtcf_col)
  qtcp <- rlang::enquo(qtcp_col)
  trt <- rlang::enquo(trt_col)

  model_type <- match.arg(model_type)
  if (rlang::quo_is_null(id) && (model_type == "lme") && !is.null(spec)) {
    stop(
      "Must supply id_col if fitting LME model. Otherwise use show_lm_results = TRUE"
    )
  }

  vars <- c(rr, qt, id, qtcb, qtcf, qtcp, trt)
  required_cols <- unlist(lapply(vars, name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  qtcs_quos <- c(qt, qtcb, qtcf, qtcp)
  qtcs <- unlist(sapply(qtcs_quos, name_quo_if_not_null))

  if (is.null(style)) style <- ggstylekit::style_spec()
  if (is.null(style$xlabel)) style$xlabel <- "RR (ms)"

  plots <- lapply(qtcs, function(qtc) {
    style$ylabel <- paste(qtc, "(ms)")
    style$title <- qtc

    p <- eda_qt_rr_plot(
      data,
      rr_col = !!rr,
      qt_col = !!dplyr::sym(qtc),
      id_col = !!id,
      trt_col = !!trt,
      model_type = model_type,
      show_model_results = spec,
      method = method,
      remove_rr_iiv = remove_rr_iiv,
      style = style
    )
    return(p)
  })

  legend_pos <- if (rlang::quo_is_null(trt)) {
    "none"
  } else {
    style$legend.position %||% "top"
  }
  combine_panels(plots, ncol = 1, legend_position = legend_pos)
}

#' EDA Quantiles Plot
#'
#' Plots the observed decile-decile scatter plot of x-data vs y-data with linear regression.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param xdata_col An unquoted column name for x data
#' @param ydata_col An unquoted column name for y data
#' @param trt_col An unquoted column name for treatment column to stratify the data by
#' @param plot_observations Logical, whether to include raw individual data points as background (default: FALSE)
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param error_bars A string for setting which errorbars are shown, CI, SE, SD
#' @param style A ggstylekit::style_spec() object
#'
#' @return A scatter plot of decile medians with linear regression and optional error bars
#'
#' @export
#'
#' @examples
#'
#' eda_quantiles_plot(
#'   cqtkit_data_verapamil,
#'   RR,
#'   QTCF,
#'   trt_col = TRTG,
#'   style = ggstylekit::style_spec(
#'     ylims = c(300, 500),
#'     xlabel = "RR (ms)",
#'     ylabel = "QTcF (ms)",
#'     legends = ggstylekit::legend_spec(channel = "color", title = "Treatment Group"),
#'     legend.position = "top"
#'   )
#' )
eda_quantiles_plot <- function(
  data,
  xdata_col,
  ydata_col,
  trt_col = NULL,
  plot_observations = FALSE,
  conf_int = 0.90,
  error_bars = "CI",
  style = ggstylekit::style_spec()
) {
  checkmate::assertDataFrame(data)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)
  checkmate::assert_choice(error_bars, c("CI", "SE", "SD"), null.ok = TRUE)

  xdata <- rlang::enquo(xdata_col)
  ydata <- rlang::enquo(ydata_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(xdata, ydata, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  # Add trt group
  data$.trt_group <- if (!rlang::quo_is_null(trt)) {
    rlang::eval_tidy(trt, data) # Preserve actual grouping
  } else {
    "All" # Fallback for global plot
  }

  obs <- data |>
    dplyr::group_by(.data$.trt_group) |>
    dplyr::group_modify(~ compute_quantiles_obs_df(.x, !!xdata, !!ydata)) |>
    dplyr::ungroup()

  quantile_aes <- if (rlang::quo_is_null(trt)) {
    ggplot2::aes(
      x = .data$xdata,
      y = .data$mean_dv,
      group = .data$.trt_group
    )
  } else {
    ggplot2::aes(
      x = .data$xdata,
      y = .data$mean_dv,
      group = .data$.trt_group,
      color = .data$.trt_group
    )
  }

  p <- obs |>
    ggplot2::ggplot(quantile_aes)

  if (plot_observations) {
    obs_aes <- if (rlang::quo_is_null(trt)) {
      ggplot2::aes(x = !!xdata, y = !!ydata)
    } else {
      ggplot2::aes(x = !!xdata, y = !!ydata, color = !!trt)
    }
    p <- p +
      ggplot2::geom_point(data = data, obs_aes, alpha = 0.25)
  }

  p <- p +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, level = conf_int)



  p <- add_error_bars_to_plot(obs, p, NULL, error_bars, conf_int)
  caption <- p$labels$caption

  caption <- paste0(
    "Shaded region represents ",
    conf_int * 100,
    "% CI\n",
    caption
  )
  p <- p +
    ggplot2::labs(
      caption = caption
    )

  p <- cqtkit_style_plot(p, style)
  return(p)
}

#' EDA Scatter With Regressions
#'
#' Plots scatter plot with linear and loess regressions. Can be used to check for linearity.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param ydata_col An unquoted column name for dependent variable measurements
#' @param xdata_col An unquoted column name for independent variable measurements
#' @param trt_col An unquoted column name for treatment group
#' @param reference_threshold Optional vector of numbers to add as horizontal dashed lines
#' @param loess_line Logical, whether to add LOESS regression line
#' @param linear_line Logical, whether to add linear regression line
#' @param span A fractional value for LOESS span parameter in geom_smooth if LOESS is used, default 0.99
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param style A ggstylekit::style_spec() object.
#'
#' @return A scatter plot with linear and/or LOESS regression lines for assessing linearity
#' @export
#'
#' @examples
#'
#' eda_scatter_with_regressions(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   CONC,
#'   TRTG,
#'   reference_threshold = 10)
eda_scatter_with_regressions <- function(
  data,
  ydata_col,
  xdata_col,
  trt_col = NULL,
  reference_threshold = NULL,
  loess_line = TRUE,
  linear_line = TRUE,
  span = 0.99,
  conf_int = 0.90,
  style = ggstylekit::style_spec()
) {
  checkmate::assertDataFrame(data)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)

  ydata <- rlang::enquo(ydata_col)
  xdata <- rlang::enquo(xdata_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(ydata, xdata, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  plot_data <- data
  plot_data$.trt_group <- if (!rlang::quo_is_null(trt)) {
    as.factor(rlang::eval_tidy(trt, data))
  } else {
    as.factor("Treatment")
  }

  p <- plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(x = !!xdata, y = !!ydata)
    ) +
    ggplot2::geom_point(trt_color_mapping(trt))

  if (loess_line) {
    p <- p +
      ggplot2::geom_smooth(
        method = "loess",
        span = span,
        level = conf_int,
        formula = y ~ x
      ) |>
      ggstylekit::series_layer(
        name = "LOESS Regression",
        legend_channel = "linetype"
      )
  }

  if (linear_line) {
    p <- p +
      ggplot2::geom_smooth(
        method = "lm",
        formula = y ~ x,
        level = conf_int
      ) |>
      ggstylekit::series_layer(
        name = "Linear Regression",
        legend_channel = "linetype"
      )
  }

  # Add horizontal references
  p <- p |> add_horizontal_references(reference_threshold)

  caption <- paste0("Shaded region represents ", round(conf_int * 100), "% CI")
  if (loess_line) {
    caption <- paste0(caption, "\n", "LOESS span = ", span)
  }

  p <- p +
    ggplot2::labs(
      caption = caption
    )

  fit_names <- c(
    if (loess_line) "LOESS Regression",
    if (linear_line) "Linear Regression"
  )
  legends <- list(
    ggstylekit::legend_spec(
      channel = "color",
      title = "Treatment Group",
      order = 1
    )
  )
  linetypes <- NULL
  if (length(fit_names) > 0) {
    linetypes <- stats::setNames(rep("dashed", length(fit_names)), fit_names)
    legends <- c(
      legends,
      list(ggstylekit::legend_spec(channel = "linetype", title = "", order = 2))
    )
  }

  p <- cqtkit_style_plot(
    p,
    style,
    xlabel = "Concentration (ng/mL)",
    ylabel = bquote(Delta ~ "QTc (ms)"),
    colors = c(
      if (loess_line) c("LOESS Regression" = "blue"),
      if (linear_line) c("Linear Regression" = "black")
    ),
    fill = if (loess_line) c("LOESS Regression" = "lightblue"),
    linewidths = if (loess_line) c("LOESS Regression" = 0.5),
    linetypes = linetypes,
    legends = legends
  )

  return(p)
}

#' EDA Hysteresis Loop Plot
#'
#' Hysteresis loop plot to visually inspect hysteresis. Counter-clockwise loops
#' (effect lags concentration) suggest hysteresis may be present.
#'
#' When `show_hysteresis_warning = TRUE`, facet labels are annotated based on
#' the detection algorithm in [compute_potential_hysteresis()], which flags
#' hysteresis when the time of max effect (Umax) lags max concentration (Tmax)
#' by >= 1 hour and more than 3 timepoints show mean deltaQTc > 5 ms.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param ntime_col An unquoted column name for nominal time points
#' @param deltaqtc_col An unquoted column name for dQTC measurements at the time points in NTLD
#' @param conc_col An unquoted column name for drug concentrations at each NTLD
#' @param dosef_col An unquoted column name for DOSE factors.
#' @param group_col An unquoted column name for additional grouping column
#' @param reference_dose Reference dose value for comparison calculations
#' @param show_hysteresis_warning Logical, whether to add "Hysteresis Detected" to facet labels for affected groups
#' @param style A ggstylekit::style_spec() object
#'
#' @return A faceted plot showing concentration vs deltaQTc trajectories over time with directional arrows
#'
#' @seealso [compute_potential_hysteresis()] for the detection algorithm used when
#'   `show_hysteresis_warning = TRUE`
#' @export
#'
#' @examples
#'
#' eda_hysteresis_loop_plot(
#'   cqtkit_data_verapamil,
#'   NTLD,
#'   deltaQTCF,
#'   CONC,
#'   DOSEF,
#'   reference_dose = "0 mg",
#'   style = ggstylekit::style_spec(
#'     ylabel = bquote(Delta~Delta~"QTcF (ms)")
#'   )
#' )
eda_hysteresis_loop_plot <- function(
  data,
  ntime_col,
  deltaqtc_col,
  conc_col,
  dosef_col,
  group_col = NULL,
  reference_dose = NULL,
  show_hysteresis_warning = TRUE,
  style = ggstylekit::style_spec()
) {
  checkmate::assertDataFrame(data)
  time <- rlang::enquo(ntime_col)
  deltaqtc <- rlang::enquo(deltaqtc_col)
  conc <- rlang::enquo(conc_col)
  dosef <- rlang::enquo(dosef_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(
    lapply(
      c(time, deltaqtc, conc, dosef, group),
      name_quo_if_not_null
    )
  )

  checkmate::assertNames(names(data), must.include = required_cols)

  if (!is.null(reference_dose)) {
    checkmate::assert_choice(
      as.character(reference_dose),
      as.character(data |> dplyr::pull(!!dosef))
    )
  }

  checkmate::assert_factor(data |> dplyr::pull(!!dosef))

  ### This should be it's own compute_ function
  mean_qtc_df <- compute_grouped_mean_sd(
    data = data,
    dv_col = !!deltaqtc,
    ntime_col = !!time,
    dose_col = !!dosef,
    group_col = !!group,
    reference_dose = reference_dose
  )

  mean_conc_df <- compute_grouped_mean_sd(
    data = data,
    dv_col = !!conc,
    ntime_col = !!time,
    dose_col = !!dosef,
    group_col = !!group,
    reference_dose = reference_dose
  )

  mean_qtc_conc_df <- tibble::tibble(
    time = mean_conc_df$time,
    dose = factor(
      mean_conc_df$dose,
      levels = levels(data |> dplyr::pull(!!dosef))
    ),
    meanCONC = mean_conc_df$mean_dv,
    meandQTC = mean_qtc_df$mean_dv,
    group = mean_conc_df$group
  )

  if (!is.null(reference_dose)) {
    mean_qtc_conc_df$meandQTC <- mean_qtc_df$mean_delta_dv
    mean_qtc_conc_df$meanCONC <- mean_conc_df$mean_delta_dv
    mean_qtc_conc_df <- mean_qtc_conc_df |>
      dplyr::filter(.data$dose != reference_dose)
  }

  dose_labeller <- compute_hysteresis_labeller(
    mean_qtc_conc_df,
    .data$time,
    .data$meandQTC,
    .data$meanCONC,
    .data$dose,
    .data$group
  )

  group_values <- mean_qtc_conc_df$group
  group_levels <- if (is.factor(group_values)) {
    levels(group_values)
  } else {
    gtools::mixedsort(unique(group_values))
  }
  hysteresis_labels <- sapply(group_levels, function(g) dose_labeller()[[g]])

  mean_qtc_conc_df <- mean_qtc_conc_df |>
    dplyr::mutate(
      dosef_hys = factor(
        as.character(dose_labeller()[as.character(.data$group)]),
        levels = hysteresis_labels
      )
    )

  # Add mid point calculation for adding arrows
  mean_qtc_conc_df <- mean_qtc_conc_df |>
    dplyr::group_by(.data$group) |>
    dplyr::arrange(.data$time) |>
    dplyr::mutate(
      xmid = (.data$meanCONC + dplyr::lead(.data$meanCONC)) / 2,
      ymid = (.data$meandQTC + dplyr::lead(.data$meandQTC)) / 2
    ) |>
    dplyr::ungroup()

  .p <- mean_qtc_conc_df |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$meanCONC,
        y = .data$meandQTC,
        color = .data$group,
        label = .data$time
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::geom_segment(
      data = mean_qtc_conc_df |>
        dplyr::filter(!is.na(.data$xmid) & !is.na(.data$ymid)),
      ggplot2::aes(
        xend = .data$xmid,
        yend = .data$ymid
      ),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "open"),
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$time),
      vjust = 1.5,
      size = 3
    )

  ylabel <- if (!is.null(reference_dose)) {
    bquote("Mean " ~ Delta ~ Delta ~ "QTc (ms)")
  } else {
    bquote("Mean " ~ Delta ~ "QTc (ms)")
  }

  .p <- cqtkit_style_plot(
    .p,
    style,
    xlabel = "Mean Plasma Concentration (ng/mL)",
    ylabel = ylabel,
    legends = ggstylekit::legend_spec(channel = "color", title = "Dose")
  )

  if (show_hysteresis_warning) {
    .p <- .p +
      ggplot2::facet_wrap(~ .data$dosef_hys, scales = "free")
  } else {
    .p <- .p +
      ggplot2::facet_wrap(~ .data$group, scales = "free")
  }

  return(.p)
}

#' EDA Mean DV Over Time
#'
#' Plots mean dependent variable over time.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param dv_col An unquoted column name for dependent variable
#' @param ntime_col An unquoted column name for nominal Time grouping
#' @param dosef_col An unquoted column name for Dose grouping
#' @param secondary_data_col Optional unquoted column name to overlay on secondary y-axis
#' @param group_col An unquoted column name for additional grouping column
#' @param reference_dose Reference dose value for comparison calculations
#' @param reference_threshold Optional - a numeric or vector of numerics to add dashed lines to plot
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param scale_factor Optional scale factor for scaling secondary_data_col
#' @param shift_factor Optional additive factor for shifting secondary data
#' @param error_bars A string for setting which errorbars are shown, CI, SE, SD
#' @param sec_ylabel A string for secondary ylabel, default is Concentration (ng/mL)
#' @param style A ggstylekit::style_spec() object.
#'
#' @return A line plot of mean dependent variable over time with optional error bars, reference lines, and secondary axis
#' @export
#'
#' @examples
#'
#' eda_mean_dv_over_time(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   NTLD,
#'   DOSEF,
#'   group_col = TRTG,
#'   reference_dose = "0 mg",
#'   reference_threshold = 10,
#'   style = ggstylekit::style_spec(ylabel = bquote('Mean '~Delta~Delta~'QTc (ms)')))
eda_mean_dv_over_time <- function(
  data,
  dv_col,
  ntime_col,
  dosef_col,
  secondary_data_col = NULL,
  group_col = NULL,
  reference_dose = NULL,
  reference_threshold = NULL,
  conf_int = 0.90,
  scale_factor = NULL,
  shift_factor = NULL,
  error_bars = "CI",
  sec_ylabel = "Concentration (ng/mL)",
  style = ggstylekit::style_spec()
) {
  # Check inputs
  checkmate::assertDataFrame(data)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)
  checkmate::assertNumeric(reference_threshold, null.ok = TRUE)
  checkmate::assert_choice(error_bars, c("CI", "SE", "SD"), null.ok = TRUE)
  checkmate::assertNumeric(scale_factor, null.ok = TRUE)

  # enquo variables
  dv <- rlang::enquo(dv_col)
  time <- rlang::enquo(ntime_col)
  dosef <- rlang::enquo(dosef_col)
  group <- rlang::enquo(group_col)
  sec_dv <- rlang::enquo(secondary_data_col)

  # check data has variables
  required_cols <- unlist(lapply(
    c(dv, time, dosef, group, sec_dv),
    name_quo_if_not_null
  ))
  checkmate::assertNames(names(data), must.include = required_cols)

  # compute average groupped over time and group col
  dv_time_df <- compute_grouped_mean_sd(
    data = data,
    dv_col = !!dv,
    ntime_col = !!time,
    dose_col = !!dosef,
    reference_dose = reference_dose,
    conf_int = conf_int,
    group_col = !!group
  )

  # create same dataset if sec_col supplied
  if (!rlang::quo_is_null(sec_dv)) {
    sec_dv_time_df <- compute_grouped_mean_sd(
      data = data,
      dv_col = !!sec_dv,
      ntime_col = !!time,
      dose_col = !!dosef,
      reference_dose = reference_dose,
      conf_int = conf_int,
      group_col = !!group
    )
  }

  # Check reference dose to grab correct y-value column either meanDV or mean_delta_DV
  if (!is.null(reference_dose)) {
    y_data <- "mean_delta_dv"
    dv_time_df <- dv_time_df |> dplyr::filter(.data$dose != reference_dose)
    if (!rlang::quo_is_null(sec_dv)) {
      sec_dv_time_df <- sec_dv_time_df |>
        dplyr::filter(.data$dose != reference_dose)
    }
  } else {
    y_data <- "mean_dv"
  }

  # Check if additional group col was supplied and create plot
  if (rlang::quo_is_null(group)) {
    if (!rlang::quo_is_null(sec_dv)) {
      # Include DV names when sec_dv is provided
      dv_time_df <- dv_time_df |>
        dplyr::mutate(grouping = paste(.data$dose, rlang::quo_name(dv)))

      sec_dv_time_df <- sec_dv_time_df |>
        dplyr::mutate(grouping = paste(.data$dose, rlang::quo_name(sec_dv)))
    } else {
      # Just use dose without DV name when sec_dv is NULL
      dv_time_df <- dv_time_df |>
        dplyr::mutate(grouping = as.character(.data$dose))
    }
  } else {
    if (!rlang::quo_is_null(sec_dv)) {
      # Include DV names when sec_dv is provided
      dv_time_df <- dv_time_df |>
        dplyr::mutate(
          grouping = as.factor(paste(.data$group, rlang::quo_name(dv)))
        )

      sec_dv_time_df <- sec_dv_time_df |>
        dplyr::mutate(
          grouping = as.factor(paste(.data$group, rlang::quo_name(sec_dv)))
        )
    } else {
      # Just use group without DV name when sec_dv is NULL
      dv_time_df <- dv_time_df |>
        dplyr::mutate(grouping = as.factor(.data$group))
    }
  }
  p <- dv_time_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$time,
      y = .data[[y_data]],
      color = .data$grouping,
      group = .data$grouping
    ))

  p <- add_horizontal_references(
    p,
    reference_threshold,
    legend_channel = "color"
  )

  p <- p +
    ggplot2::geom_point() +
    ggplot2::geom_line()

  p <- add_error_bars_to_plot(
    dv_time_df,
    p,
    reference_dose,
    error_bars,
    conf_int
  )

  if (is.null(style)) style <- ggstylekit::style_spec()
  ylabel <- style$ylabel %||% bquote("Mean " ~ Delta ~ "QTc (ms)")

  if (!rlang::quo_is_null(sec_dv)) {
    p <- add_secondary_data(
      dv_time_df,
      sec_dv_time_df,
      reference_threshold,
      p,
      y_data,
      group,
      scale_factor,
      shift_factor,
      sec_ylabel,
      ylabel
    )
  }

  p <- cqtkit_style_plot(
    p,
    style,
    xlabel = "Nominal time since last dose (h)",
    ylabel = ylabel,
    legends = ggstylekit::legend_spec(channel = "color", title = "Legend")
  )

  return(p)
}
