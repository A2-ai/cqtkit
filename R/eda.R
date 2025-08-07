#' Plot QT against RR
#'
#' @param data dataframe containing QT dataset
#' @param rr_col an unquoted column name of RR data
#' @param qt_col an unquoted column name of QT data
#' @param id_col an unquoted column name of ID data
#' @param trt_col an unquoted column name of Treatment group data
#' @param conf_int confidence interval for lm regression coefficients, default 0.90
#' @param model_type lm or lme, which model to fit for showing on plot
#' @param show_model_results a bool for showing regression slope on plot.
#' @param method method for nlme::lme fitting
#' @param remove_rr_iiv a boolean for removing IIV on RR slope
#' @param style a named list of any argument that can be passed to style_plot
#'
#' @return a plot of the input QT against RR
#' @export
#'
#' @examples
#' data <- preprocess(data)
#'
#' eda_qt_rr_plot(data, RR, QT, ID, model_type = "lme")
eda_qt_rr_plot <- function(
  data,
  rr_col,
  qt_col,
  id_col = NULL,
  trt_col = NULL,
  conf_int = 0.90,
  model_type = c("lm", "lme"),
  show_model_results = TRUE,
  method = "REML",
  remove_rr_iiv = FALSE,
  style = list()
) {
  checkmate::assertDataFrame(data)

  qt <- rlang::enquo(qt_col)
  rr <- rlang::enquo(rr_col)
  id <- rlang::enquo(id_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(qt, rr, id, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  model_type = match.arg(model_type)
  if (rlang::quo_is_null(id) && (model_type == "lme") && show_model_results) {
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

  qt_rr_plot <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = !!rr, y = !!qt)) +
    ggplot2::geom_point(ggplot2::aes(
      color = .data$.trt_group,
      shape = .data$.trt_group
    )) +
    ggplot2::theme_bw()

  if (model_type == "lm" && show_model_results) {
    lm_results <- compute_lm_fit_df(
      data,
      xdata_col = !!rr,
      ydata_col = !!qt,
      conf_int = conf_int
    )

    label = paste0(
      "Linear Regression Slope [",
      round(conf_int * 100),
      "% CI]: ",
      round(lm_results$slope, 3),
      " [",
      round(lm_results$slope_ci_lower, 3),
      ", ",
      round(lm_results$slope_ci_upper, 3),
      "]"
    )

    qt_rr_plot <- qt_rr_plot +
      ggplot2::geom_smooth(
        method = 'lm',
        se = FALSE,
        formula = y ~ x,
        color = 'black'
      )
  } else if ((model_type == "lme") && show_model_results) {
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
      conf_int = conf_int,
      trt_col_name = name_quo_if_not_null(trt),
      id_col_name = name_quo_if_not_null(id)
    )

    slope <- estimates %>%
      dplyr::filter(.data$Parameters == rlang::quo_name(rr)) %>%
      dplyr::pull(.data$Value)

    slope_ci_lower <- estimates %>%
      dplyr::filter(.data$Parameters == rlang::quo_name(rr)) %>%
      dplyr::pull(.data$CIl)

    slope_ci_upper <- estimates %>%
      dplyr::filter(.data$Parameters == rlang::quo_name(rr)) %>%
      dplyr::pull(.data$CIu)

    label <- paste0(
      "Linear Mixed Effects Slope [",
      round(conf_int * 100),
      "% CI]: ",
      round(slope, 3),
      " [",
      round(slope_ci_lower, 3),
      ", ",
      round(slope_ci_upper, 3),
      "]"
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

  if (is.null(style)) style <- list()
  style$xlabel <- style$xlabe %||% "RR (ms)"
  style$ylabel <- style$ylabel %||% "QT (ms)"
  style$legend <- style$legend %||% "Treatment Group"

  qt_rr_plot <- do.call(style_plot, c(list(p = qt_rr_plot), style))
  return(qt_rr_plot)
}

#' plots different corrections of QT against RR to compare which to use.
#'
#' @param data dataframe containing QTc data
#' @param rr_col an unquoted column name of RR data
#' @param qt_col an unquoted column name of QT data
#' @param qtcb_col an unquoted column name of Bazett corrected QT data
#' @param qtcf_col an unquoted column name of Fridericia corrected QT data
#' @param qtcp_col Optional - an unquoted column name of Population corrected QT data
#' @param id_col an unquoted column name of ID data
#' @param trt_col Optional - an unquoted column name of treatment group data
#' @param legend_location string for legend location, top,bottom,left,right
#' @param model_type lm or lme, which model to fit for showing on plot
#' @param show_model_results a bool for showing regression slope on plot.
#' @param method method for nlme::lme fitting
#' @param remove_rr_iiv a boolean for removing IIV on RR slope
#' @param conf_int confidence interval for lm results default 0.90
#' @param style named list of any argument that can be passed to style_plots
#'
#' @return a plot
#' @export
#'
#' @examples
#' data <- preprocess(data)
#'
#' eda_qtc_comparison_plot(
#'   data,
#'   RR,
#'   QT,
#'   QTCB,
#'   QTCF,
#'   id_col = ID,
#'   trt_col = TRTG,
#'   model_type = "lme",
#'   show_model_results = TRUE,
#'   remove_rr_iiv = TRUE,
#'   legend_location = 'top')
eda_qtc_comparison_plot <- function(
  data,
  rr_col,
  qt_col,
  qtcb_col,
  qtcf_col,
  qtcp_col = NULL,
  id_col = NULL,
  trt_col = NULL,
  legend_location = "top",
  model_type = c("lm", "lme"),
  show_model_results = TRUE,
  method = "REML",
  remove_rr_iiv = FALSE,
  conf_int = 0.90,
  style = list()
) {
  checkmate::assertDataFrame(data)

  rr <- rlang::enquo(rr_col)
  qt <- rlang::enquo(qt_col)
  id <- rlang::enquo(id_col)
  qtcb <- rlang::enquo(qtcb_col)
  qtcf <- rlang::enquo(qtcf_col)
  qtcp <- rlang::enquo(qtcp_col)
  trt <- rlang::enquo(trt_col)

  model_type = match.arg(model_type)
  if (rlang::quo_is_null(id) && (model_type == "lme") && show_model_results) {
    stop(
      "Must supply id_col if fitting LME model. Otherwise use show_lm_results = TRUE"
    )
  }

  legend_location = match.arg(
    legend_location,
    c("top", "bottom", "right", "left")
  )
  vars <- c(rr, qt, id, qtcb, qtcf, qtcp, trt)
  required_cols <- unlist(lapply(vars, name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  qtcs_quos <- c(qt, qtcb, qtcf, qtcp)
  qtcs <- unlist(sapply(qtcs_quos, name_quo_if_not_null))

  if (is.null(style)) style <- list()
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
      conf_int,
      model_type,
      show_model_results,
      method,
      remove_rr_iiv,
      style
    )
    return(p)
  })

  if (rlang::quo_is_null(trt)) {
    ggpubr::ggarrange(plotlist = plots, ncol = 1, legend = "none")
  } else {
    ggpubr::ggarrange(
      plotlist = plots,
      ncol = 1,
      common.legend = TRUE,
      legend = legend_location
    )
  }
}

#' plots the observed decile-decile scatter plot of x-data vs y-data with linear regression.
#'
#' @param data a dataframe of QTc dataset
#' @param xdata_col an unquoted column name of x data
#' @param ydata_col an unquoted column name of y data
#' @param trt_col an unquoted column name of treatment column to stratify the data by
#' @param conf_int a fractional value to set confidence interval, default = 0.9
#' @param error_bars a string for setting which errorbars are shown, CI, SE, SD
#' @param style a named list of any argument that can be passed to style_plot
#'
#' @return a plot
#'
#' @export
#'
#' @examples
#' data <- preprocess(data)
#'
#' eda_quantiles_plot(
#'   data,
#'   RR,
#'   QTCF,
#'   trt_col = TRTG,
#'	 style = set_style(
#'	  legend = "Treatment Group",
#'	  ylims = c(300, 500),
#'	  xlabel = "RR (ms)",
#'	  ylabel = "QTcF (ms)",
#'	  legend.position = "top"
#'	 )
#' )
eda_quantiles_plot <- function(
  data,
  xdata_col,
  ydata_col,
  trt_col = NULL,
  conf_int = 0.90,
  error_bars = 'CI',
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)
  checkmate::assert_choice(error_bars, c('CI', 'SE', 'SD'), null.ok = TRUE)

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

  obs <- data %>%
    dplyr::group_by(.data$.trt_group) %>%
    dplyr::group_modify(~ compute_quantiles_obs_df(.x, !!xdata, !!ydata)) %>%
    dplyr::ungroup()

  p <- obs %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$xdata,
        y = .data$mean_dv,
        group = .data$.trt_group,
        color = .data$.trt_group
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape = .data$.trt_group
      )
    ) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, level = conf_int) +
    ggplot2::theme_bw()

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

  if (is.null(style)) style <- list()
  style$legend <- style$legend %||% "Treatment Group"

  p <- do.call(style_plot, c(list(p = p), style))
  return(p)
}

#' plots scatter plot with with linear and loess regressions. Can be used to check for linearity.
#'
#' @param data a dataframe of QT dataset
#' @param ydata_col an unquoted column name of dependent variable measurements
#' @param xdata_col an unquoted column name of independent variable measurements
#' @param trt_col Optional - an unquoted column name of treatment group
#' @param reference_threshold optional vector of numbers to add as horizontal dashed lines
#' @param loess_line a bool to add LOESS regression line
#' @param linear_line a bool to add a linear regression line
#' @param span a fractional value for LOESS span parameter in geom_smooth if LOESS is used, default 0.99
#' @param conf_int a fractional numeric for setting confidence interval, default = 9
#' @param style a named list of any argument that can be passed to style_plot. Shapes are mapped to treatment groups and can be controlled via the shapes parameter in style
#'
#' @return a plot
#' @export
#'
#' @examples
#' data <- preprocess(data)
#'
#' eda_scatter_with_regressions(
#'   data,
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
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)

  ydata <- rlang::enquo(ydata_col)
  xdata <- rlang::enquo(xdata_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(ydata, xdata, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  dqtcf_conc_df <- tibble::tibble(
    ydata = data %>% dplyr::pull(!!ydata),
    xdata = data %>% dplyr::pull(!!xdata)
  )

  if (!rlang::quo_is_null(trt)) {
    dqtcf_conc_df$trt <- data %>% dplyr::pull(!!trt)
  } else {
    dqtcf_conc_df$trt <- as.factor("Treatment")
  }

  p <- dqtcf_conc_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$xdata,
        y = .data$ydata,
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = .data$trt,
        shape = .data$trt
      )
    ) +
    ggplot2::theme_bw()

  if (loess_line) {
    p <- p +
      ggplot2::geom_smooth(
        method = "loess",
        span = span,
        level = conf_int,
        formula = y ~ x,
        color = "blue",
        fill = "lightblue",
        ggplot2::aes(linetype = "LOESS Regression"),
        linewidth = 0.5
      )
  }

  if (linear_line) {
    p <- p +
      ggplot2::geom_smooth(
        method = "lm",
        ggplot2::aes(linetype = "Linear Regression"),
        formula = y ~ x,
        color = "black",
        level = conf_int
      )
  }

  # Add horizontal references
  p <- p %>% add_horizontal_references(reference_threshold)

  # Set linetype attribute for styling
  linetype_values <- c()
  if (linear_line) linetype_values["Linear Regression"] <- "dashed"
  if (loess_line) linetype_values["LOESS Regression"] <- "dashed"

  if (length(linetype_values) > 0) {
    attr(p, "linetype_values") <- linetype_values
  }
  #https://stackoverflow.com/questions/39119917/how-to-add-a-legend-to-hline

  caption <- paste0("Shaded region represents ", round(conf_int * 100), "% CI")
  if (loess_line) {
    caption <- paste0(caption, "\n", "LOESS span = ", span)
  }

  p <- p +
    ggplot2::labs(
      caption = caption
    )
  if (is.null(style)) style <- list()
  style$xlabel <- style$xlabel %||% "Concentration (ng/mL)"
  style$ylabel <- style$ylabel %||% bquote(Delta ~ "QTc (ms)")
  style$legend <- style$legend %||% "Treatment Group"
  style$color_order <- style$color_order %||% 1
  style$shape_order <- style$shape_order %||% 1
  style$linetype_order <- style$linetype_order %||% 2

  p <- do.call(style_plot, c(list(p = p), style))

  return(p)
}

#' Hysteresis loop plot to visually inspect hysteresis
#' @param data a dataframe of QTc dataset
#' @param ntime_col an unquoted column name of nominal time points
#' @param deltaqtc_col an unquoted column name of dQTC measurements at the time points in NTLD
#' @param conc_col an unquoted column name of drug concentrations at each NTLD
#' @param dosef_col an unquoted column name of DOSE factors.
#' @param group_col an unquoted column name of additional grouping column
#' @param reference_dose Optional - DOSE of reference (i.e. placebo, DOSE == 0) measurements
#' @param show_hysteresis_warning boolean, if TRUE adds Hysteresis Detected to facet wrap label for groups with hysteresis detected
#' @param style a named list of any argument that can be passed to style_plot
#'
#' @return a plot
#' @export
#'
#' @examples
#' data <- preprocess(data)
#'
#' eda_hysteresis_loop_plot(
#'   data,
#'   NTLD,
#'   deltaQTCF,
#'   CONC,
#'   DOSEF,
#'   reference_dose = "0 mg",
#'   style = set_style(
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
  style = list()
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
      as.character(data %>% dplyr::pull(!!dosef))
    )
  }

  checkmate::assert_factor(data %>% dplyr::pull(!!dosef))

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
      levels = levels(data %>% dplyr::pull(!!dosef))
    ),
    meanCONC = mean_conc_df$mean_dv,
    meandQTC = mean_qtc_df$mean_dv,
    group = mean_conc_df$group
  )

  if (!is.null(reference_dose)) {
    mean_qtc_conc_df$meandQTC <- mean_qtc_df$mean_delta_dv
    mean_qtc_conc_df$meanCONC <- mean_conc_df$mean_delta_dv
    mean_qtc_conc_df <- mean_qtc_conc_df %>%
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

  mean_qtc_conc_df <- mean_qtc_conc_df %>%
    dplyr::mutate(
      dosef_hys = factor(
        as.character(dose_labeller()[as.character(.data$group)]),
        levels = hysteresis_labels
      )
    )

  # Add mid point calculation for adding arrows
  mean_qtc_conc_df <- mean_qtc_conc_df %>%
    dplyr::group_by(.data$group) %>%
    dplyr::arrange(.data$time) %>%
    dplyr::mutate(
      xmid = (.data$meanCONC + dplyr::lead(.data$meanCONC)) / 2,
      ymid = (.data$meandQTC + dplyr::lead(.data$meandQTC)) / 2
    ) %>%
    dplyr::ungroup()

  .p <- mean_qtc_conc_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$meanCONC,
        y = .data$meandQTC,
        color = .data$group,
        label = .data$time,
        shape = .data$group
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::geom_segment(
      data = mean_qtc_conc_df %>%
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
    ) +
    ggplot2::theme_bw()

  if (is.null(style)) style <- list()

  style$xlabel <- style$xlabel %||% "Mean Plasma Concentration (ng/mL)"
  style$legend <- style$legend %||% "Dose"

  if (!(is.null(reference_dose))) {
    style$ylabel <- style$ylabel %||%
      bquote("Mean " ~ Delta ~ Delta ~ "QTc (ms)")
  } else {
    style$ylabel <- style$ylabel %||% bquote("Mean " ~ Delta ~ "QTc (ms)")
  }

  .p <- do.call(style_plot, c(list(p = .p), style))

  if (show_hysteresis_warning) {
    .p <- .p +
      ggplot2::facet_wrap(~ .data$dosef_hys, scales = "free")
  } else {
    .p <- .p +
      ggplot2::facet_wrap(~ .data$group, scales = "free")
  }

  return(.p)
}

#' Plots mean dependent variable over time
#'
#' @param data dataframe containing QTc dataset
#' @param dv_col an unquoted column name of dependent variable
#' @param ntime_col an unquoted column name of nominal Time grouping
#' @param dosef_col an unquoted column name of Dose grouping
#' @param secondary_data_col optional unquoted column name to overlay on secondary y-axis
#' @param group_col an unquoted column name of additional grouping column
#' @param reference_dose Optional - DOSE of reference (i.e. placebo, DOSE == 0) measurements
#' @param reference_threshold Optional - a numeric or vector of numerics to add dashed lines to plot
#' @param conf_int fractional confidence interval, default = 0.9
#' @param scale_factor optional scale factor for scaling secondary_data_col
#' @param shift_factor optional additive factory for shifting secondary data
#' @param error_bars a string for setting which errorbars are shown, CI, SE, SD
#' @param sec_ylabel a string for secondary ylabel, default is Concentration (ng/mL)
#' @param style a named list of any argument that can be passed to style_plots. Shapes are mapped to grouping variables and can be controlled via the shapes parameter in style
#'
#' @return a plot
#' @export
#'
#' @examples
#' data <- preprocess(data)
#'
#' eda_mean_dv_over_time(
#'   data,
#'   deltaQTCF,
#'   NTLD,
#'   DOSEF,
#'   group_col = TRTG,
#'   reference_dose = "0 mg",
#'   reference_threshold = 10,
#'   style = set_style(ylabel = bquote('Mean '~Delta~Delta~'QTc (ms)')))
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
  style = list()
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
    dv_time_df <- dv_time_df %>% dplyr::filter(.data$dose != reference_dose)
    if (!rlang::quo_is_null(sec_dv)) {
      sec_dv_time_df <- sec_dv_time_df %>%
        dplyr::filter(.data$dose != reference_dose)
    }
  } else {
    y_data <- "mean_dv"
  }

  # Check if additional group col was supplied and create plot
  if (rlang::quo_is_null(group)) {
    if (!rlang::quo_is_null(sec_dv)) {
      # Include DV names when sec_dv is provided
      dv_time_df <- dv_time_df %>%
        dplyr::mutate(groupping = paste(.data$dose, rlang::quo_name(dv)))

      sec_dv_time_df <- sec_dv_time_df %>%
        dplyr::mutate(groupping = paste(.data$dose, rlang::quo_name(sec_dv)))
    } else {
      # Just use dose without DV name when sec_dv is NULL
      dv_time_df <- dv_time_df %>%
        dplyr::mutate(groupping = as.character(.data$dose))
    }
  } else {
    if (!rlang::quo_is_null(sec_dv)) {
      # Include DV names when sec_dv is provided
      dv_time_df <- dv_time_df %>%
        dplyr::mutate(
          groupping = as.factor(paste(.data$group, rlang::quo_name(dv)))
        )

      sec_dv_time_df <- sec_dv_time_df %>%
        dplyr::mutate(
          groupping = as.factor(paste(.data$group, rlang::quo_name(sec_dv)))
        )
    } else {
      # Just use group without DV name when sec_dv is NULL
      dv_time_df <- dv_time_df %>%
        dplyr::mutate(groupping = as.factor(.data$group))
    }
  }
  p <- dv_time_df %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$time,
      y = .data[[y_data]],
      color = .data$groupping,
      shape = .data$groupping,
      group = .data$groupping
    ))

  p <- add_horizontal_references(p, reference_threshold)

  p <- p +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::geom_line()

  p <- add_error_bars_to_plot(
    dv_time_df,
    p,
    reference_dose,
    error_bars,
    conf_int
  )

  if (is.null(style)) style <- list()
  style$ylabel <- style$ylabel %||% bquote("Mean " ~ Delta ~ "QTc (ms)")

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
      style$ylabel
    )
  }

  style$xlabel <- style$xlabel %||% "Nominal time since last dose (h)"
  style$legend <- style$legend %||% "Legend"
  style$color_order <- style$color_order %||% 1
  style$shape_order <- style$shape_order %||% 1
  style$linetype_order <- style$linetype_order %||% 2

  p <- do.call(style_plot, c(list(p = p), style))

  return(p)
}
