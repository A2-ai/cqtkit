#' GOF Plots
#'
#' Makes goodness of fit plots.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param trt_col An unquoted column name for treatment group"
#' @param conc_xlabel A string for concentration plot xlabel
#' @param dv_label A string of dv label (default: bquote(Delta ~ 'QTc (ms)'))
#' @param legend_location String for legend position (top, bottom, left, right)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return A 2x2 panel with concordance plots, residual distributions, Q-Q plots, and residual histograms
#' @export
#'
#' @importFrom stats density
#'
#' @examples
#' fit <- fit_prespecified_model(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#'
#' gof_plots(
#'   cqtkit_data_verapamil,
#'   fit,
#'   deltaQTCF,
#'   CONC,
#'   NTLD,
#'   TRTG)
gof_plots <- function(
  data,
  fit,
  dv_col,
  conc_col,
  ntime_col,
  trt_col = NULL,
  conc_xlabel = "Concentration ng/mL",
  dv_label = bquote(Delta ~ "QTc (ms)"),
  legend_location = c("top", "bottom", "left", "right", "none"),
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))

  legend_location <- match.arg(legend_location)

  dv <- rlang::enquo(dv_col)
  conc <- rlang::enquo(conc_col)
  time <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(dv, conc, time, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  fit_results_df <- compute_fit_results(data, fit, !!dv, !!conc, !!time, !!trt)

  # Add .trt_group for shape aesthetic
  fit_results_df$.trt_group <- if (!rlang::quo_is_null(trt)) {
    fit_results_df$TRTG
  } else {
    "All"
  }

  #Predicted vs Observed
  p1_all_values <- c(
    fit_results_df$PRED,
    fit_results_df$IPRED,
    fit_results_df$dv
  )

  p1_axis_limits <- range(p1_all_values, na.rm = TRUE)

  p1 <- fit_results_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$PRED,
      y = .data$dv,
    )) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$.trt_group)
    ) +
    ggplot2::geom_abline(slope = 1, color = "black") |>
      ggstylekit::series_layer(name = "identity") +
    ggplot2::geom_smooth(
      method = "loess",
      span = 0.99,
      color = "red",
      formula = y ~ x,
      se = FALSE
    ) |>
      ggstylekit::series_layer(name = "loess") +
    ggplot2::labs(
      x = bquote("Predicted " ~ .(dv_label)),
      y = bquote("Observed " ~ .(dv_label))
    ) +
    ggplot2::coord_equal(xlim = p1_axis_limits, ylim = p1_axis_limits)

  if (is.null(style)) style <- list()

  p1 <- cqtkit_style_plot(p1, style, theme = cqtkit_square_theme())

  #qq plot
  p2_all_values <- c(fit_results_df$IWRES)
  p2_axis_limits <- range(p2_all_values, na.rm = TRUE)

  p2 <- fit_results_df |>
    ggplot2::ggplot(ggplot2::aes(
      sample = .data$IWRES,
      color = .data$.trt_group
    )) +
    ggplot2::stat_qq() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
    ggplot2::geom_abline(slope = 1, color = "black") |>
      ggstylekit::series_layer(name = "identity") +
    ggplot2::coord_equal(xlim = p2_axis_limits, ylim = p2_axis_limits)

  p2 <- cqtkit_style_plot(p2, style, theme = cqtkit_square_theme())

  #residuals vs concentration
  p3 <- fit_results_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$conc,
      y = .data$IWRES,
    ))

  p3 <- p3 +
    ggplot2::geom_point(ggplot2::aes(color = .data$.trt_group)) +
    ggplot2::geom_smooth(
      method = "loess",
      span = 0.99,
      color = "red",
      formula = y ~ x,
      se = FALSE
    ) |>
      ggstylekit::series_layer(name = "loess") +
    ggplot2::labs(x = conc_xlabel, y = "Standardized Residuals")

  p3 <- cqtkit_style_plot(p3, style, theme = cqtkit_square_theme())

  p4 <- fit_results_df |>
    ggplot2::ggplot(ggplot2::aes(x = .data$IWRES)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      binwidth = 0.5,
      fill = "grey",
      color = "black"
    ) +
    ggplot2::stat_function(
      fun = stats::dnorm,
      args = list(mean = 0, sd = 1),
      color = "black"
    ) +
    ggplot2::stat_density(geom = "line", color = "red") +
    ggplot2::labs(x = "Standardized residuals", y = "Density") +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1)

  .p <- combine_panels(
    list(p1, p2, p3, p4),
    nrow = 2,
    ncol = 2,
    legend_position = legend_location,
    title = style$title
  )

  return(.p)
}

#' GOF Concordance Plots
#'
#' Concordance plots between population and individual predictions.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param trt_col An unquoted column name for treatment group"
#' @param dv_label A string of dv label (default: bquote(Delta ~ 'QTc (ms)'))
#' @param legend_location String for legend position (top, bottom, left, right)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return A 2-panel plot comparing population (PRED) and individual (IPRED) predictions vs observed values
#' @export
#'

#'
#' @examples
#' fit <- fit_prespecified_model(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#' gof_concordance_plots(
#'   cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG,
#'   legend_location = "top"
#' )
gof_concordance_plots <- function(
  data,
  fit,
  dv_col,
  conc_col,
  ntime_col,
  trt_col = NULL,
  dv_label = bquote(Delta ~ "QTc (ms)"),
  legend_location = c("top", "bottom", "left", "right", "none"),
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))

  legend_location <- match.arg(legend_location)
  dv <- rlang::enquo(dv_col)
  conc <- rlang::enquo(conc_col)
  time <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(dv, conc, time, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  fit_results_df <- compute_fit_results(data, fit, !!dv, !!conc, !!time, !!trt)

  # Add .trt_group for shape aesthetic
  fit_results_df$.trt_group <- if (!rlang::quo_is_null(trt)) {
    fit_results_df$TRTG
  } else {
    "All"
  }

  #Concordance Plots
  xdata <- list("PRED", "IPRED")
  xlabels <- list(
    bquote("Population Predicted " ~ .(dv_label)),
    bquote("Individual Predicted " ~ .(dv_label))
  )

  all_values <- c(fit_results_df$PRED, fit_results_df$IPRED, fit_results_df$dv)
  axis_limits <- range(all_values, na.rm = TRUE)

  if (is.null(style)) style <- list()
  if (is.null(style$xlims)) style$xlims <- axis_limits
  if (is.null(style$ylims)) style$ylims <- axis_limits

  plots <- lapply(seq_along(xdata), function(i) {
    .p <- fit_results_df |>
      ggplot2::ggplot(
        ggplot2::aes(x = .data[[xdata[[i]]]], y = .data$dv)
      ) +
      ggplot2::geom_point(ggplot2::aes(color = .data$.trt_group)) +
      ggplot2::geom_abline(slope = 1) |>
        ggstylekit::series_layer(name = "identity") +
      ggplot2::geom_smooth(
        method = "lm",
        se = FALSE,
        formula = y ~ x,
        color = "red",
        linetype = "dashed"
      ) |>
        ggstylekit::series_layer(name = "regression")

    this_style <- style
    this_style$xlabel <- this_style$xlabel %||% xlabels[[i]]
    this_style$ylabel <- this_style$ylabel %||%
      bquote("Observed  " ~ .(dv_label))

    .p <- cqtkit_style_plot(.p, this_style, theme = cqtkit_square_theme())
  })

  .p <- combine_panels(
    plots,
    legend_position = legend_location,
    title = style$title
  )

  return(.p)
}

#' GOF Residuals Plots
#'
#' Plots residuals vs predicted dQTCF and concentration.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param trt_col An unquoted column name for treatment group"
#' @param conc_xlabel A string of concentration xlabel
#' @param dv_label A string of dv label (default: bquote(Delta ~ 'QTc (ms)'))
#' @param residual_references Numeric vector of reference residual lines to add, default -2 and 2
#' @param legend_location String for legend position (top, bottom, left, right)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return A 4-panel plot of WRES and IWRES residuals vs predicted values and concentration
#' @export
#'
#' @examples
#' fit <- fit_prespecified_model(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#' gof_residuals_plots(
#'   cqtkit_data_verapamil,
#'   fit,
#'   deltaQTCF,
#'   CONC,
#'   NTLD,
#'   TRTG,
#'   legend_location = "top")
gof_residuals_plots <- function(
  data,
  fit,
  dv_col,
  conc_col,
  ntime_col,
  trt_col = NULL,
  conc_xlabel = "Concentration (ng/mL)",
  dv_label = bquote(Delta ~ "QTc (ms)"),
  residual_references = c(-2, 2),
  legend_location = c("top", "bottom", "left", "right", "none"),
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assert_numeric(residual_references, null.ok = TRUE)

  legend_location <- match.arg(legend_location)

  dv <- rlang::enquo(dv_col)
  conc <- rlang::enquo(conc_col)
  time <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(dv, conc, time, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  fit_results_df <- compute_fit_results(data, fit, !!dv, !!conc, !!time, !!trt)

  # Add .trt_group for shape aesthetic
  fit_results_df$.trt_group <- if (!rlang::quo_is_null(trt)) {
    fit_results_df$TRTG
  } else {
    "All"
  }

  xdata <- c("PRED", "conc", "IPRED", "conc")
  ydata <- c("WRES", "WRES", "IWRES", "IWRES")

  xlabel <- c(
    bquote("Population Predicted " ~ .(dv_label)),
    conc_xlabel,
    bquote("Individual Predicted " ~ .(dv_label)),
    conc_xlabel
  )

  if (is.null(style)) style <- list()

  plots <- lapply(seq_along(xdata), function(i) {
    .p <- fit_results_df |>
      ggplot2::ggplot(
        ggplot2::aes(x = .data[[xdata[[i]]]], y = .data[[ydata[[i]]]])
      ) +
      ggplot2::geom_point(ggplot2::aes(color = .data$.trt_group))

    if (!is.null(residual_references)) {
      .p <- add_horizontal_references(.p, residual_references)
    }

    this_style <- style
    this_style$xlabel <- this_style$xlabel %||% xlabel[[i]]
    this_style$ylabel <- this_style$ylabel %||% ydata[[i]]

    .p <- cqtkit_style_plot(.p, this_style)
  })

  # Arrange plots
  legend_pos <- if (!rlang::quo_is_null(trt)) legend_location else "none"
  .p <- combine_panels(
    plots,
    legend_position = legend_pos,
    title = style$title
  )

  return(.p)
}

#' GOF QQ Plots
#'
#' Plots QQ plot of WRES and IWRES.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param trt_col An unquoted column name for treatment group"
#' @param legend_location String for legend position (top, bottom, left, right)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return A 2-panel Q-Q plot comparing WRES and IWRES to normal distribution
#' @export
#'
#' @examples
#' fit <- fit_prespecified_model(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#'
#' gof_qq_plots(cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG, legend_location = "top")
gof_qq_plots <- function(
  data,
  fit,
  dv_col,
  conc_col,
  ntime_col,
  trt_col = NULL,
  legend_location = c("top", "bottom", "left", "right", "none"),
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  legend_location <- match.arg(legend_location)

  dv <- rlang::enquo(dv_col)
  conc <- rlang::enquo(conc_col)
  time <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(dv, conc, time, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  fit_results_df <- compute_fit_results(data, fit, !!dv, !!conc, !!time, !!trt)

  # Add .trt_group for shape aesthetic
  fit_results_df$.trt_group <- if (!rlang::quo_is_null(trt)) {
    fit_results_df$TRTG
  } else {
    "All"
  }

  sample_data <- list("WRES", "IWRES")
  plots <- list()

  if (is.null(style)) style <- list()

  for (r in sample_data) {
    all_values <- c(fit_results_df$WRES, fit_results_df$IWRES)
    axis_limits <- range(all_values, na.rm = TRUE)

    .qqp <- fit_results_df |>
      ggplot2::ggplot(
        ggplot2::aes(sample = .data[[r]])
      ) +
      ggplot2::stat_qq(ggplot2::aes(color = .data$.trt_group)) +
      ggplot2::geom_abline(slope = 1, linetype = "dashed") |>
        ggstylekit::series_layer(name = "identity")

    this_style <- style
    this_style$xlabel <- this_style$xlabel %||% "Theoretical Quantiles"
    this_style$ylabel <- this_style$ylabel %||% paste0("Quantiles of ", r)
    this_style$xlims <- this_style$xlims %||% axis_limits
    this_style$ylims <- this_style$ylims %||% axis_limits

    .qqp <- cqtkit_style_plot(.qqp, this_style)

    plots[[r]] <- .qqp
  }

  legend_pos <- if (!rlang::quo_is_null(trt)) legend_location else "none"
  .p <- combine_panels(
    plots,
    legend_position = legend_pos,
    title = style$title
  )
  return(.p)
}

#' GOF Residuals Time Boxplots
#'
#' Plots boxplots of residuals over Nominal Times.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param trt_col An unquoted column name for treatment group" will use for filling boxplots
#' @param residual_references Numeric vector of reference residual lines to add, default -2 and 2
#' @param legend_location String for legend position (top, bottom, left, right)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return A 2-panel boxplot of WRES and IWRES residuals by nominal time
#' @export
#'
#' @examples
#' fit <- fit_prespecified_model(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#'
#' gof_residuals_time_boxplots(
#'   cqtkit_data_verapamil,
#'   fit,
#'   deltaQTCF,
#'   CONC,
#'   NTLD,
#'   TRTG,
#'   legend_location = "top")
gof_residuals_time_boxplots <- function(
  data,
  fit,
  dv_col,
  conc_col,
  ntime_col,
  trt_col = NULL,
  residual_references = c(-2, 2),
  legend_location = c("top", "bottom", "left", "right", "none"),
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assert_numeric(residual_references, null.ok = TRUE)

  legend_location <- match.arg(legend_location)

  dv <- rlang::enquo(dv_col)
  conc <- rlang::enquo(conc_col)
  time <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(dv, conc, time, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  fit_results_df <- compute_fit_results(data, fit, !!dv, !!conc, !!time, !!trt)

  time_plots <- list()
  ydata <- c("WRES", "IWRES")

  if (is.null(style)) style <- list()

  for (i in seq_along(ydata)) {
    .rbp <- fit_results_df |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = as.factor(.data$time),
          y = .data[[ydata[[i]]]]
        )
      )

    if (!rlang::quo_is_null(trt)) {
      .rbp <- .rbp +
        ggplot2::geom_boxplot(ggplot2::aes(fill = .data$TRTG))
    } else {
      .rbp <- .rbp +
        ggplot2::geom_boxplot()
    }

    if (!is.null(residual_references)) {
      .rbp <- add_horizontal_references(.rbp, residual_references)
    }

    .rbp <- cqtkit_style_plot(
      .rbp,
      style,
      xlabel = "Nominal Time Since Last Dose (h)",
      ylabel = ydata[[i]],
      legends = list(
        ggstylekit::legend_spec(
          channel = "fill",
          title = "Treatment Group",
          order = 1
        ),
        ggstylekit::legend_spec(channel = "color", title = "", order = 2)
      )
    )

    time_plots[[i]] <- .rbp
  }

  legend_pos <- if (!rlang::quo_is_null(trt)) legend_location else "none"
  .p <- combine_panels(
    time_plots,
    ncol = 1,
    legend_position = legend_pos,
    title = style$title
  )

  return(.p)
}

#' GOF Residuals Treatment Boxplots
#'
#' Generates boxplots of residuals for treatment group.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param trt_col An unquoted column name for treatment group"
#' @param residual_references Numeric vector of reference residual lines to add, default -2 and 2
#' @param legend_location String for legend position (top, bottom, left, right)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return A 2-panel boxplot of WRES and IWRES residuals by treatment group
#' @export
#'
#' @examples
#' fit <- fit_prespecified_model(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#' gof_residuals_trt_boxplots(cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG)
gof_residuals_trt_boxplots <- function(
  data,
  fit,
  dv_col,
  conc_col,
  ntime_col,
  trt_col = NULL,
  residual_references = c(-2, 2),
  legend_location = c("top", "bottom", "left", "right", "none"),
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assert_numeric(residual_references, null.ok = TRUE)

  legend_location <- match.arg(legend_location)

  dv <- rlang::enquo(dv_col)
  conc <- rlang::enquo(conc_col)
  time <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(dv, conc, time, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  fit_results_df <- compute_fit_results(data, fit, !!dv, !!conc, !!time, !!trt)

  trtg_plots <- list()
  ydata <- c("WRES", "IWRES")

  if (is.null(style)) style <- list()

  for (i in seq_along(ydata)) {
    .rbpt <- fit_results_df |>
      ggplot2::ggplot(
        if (!rlang::quo_is_null(trt)) {
          ggplot2::aes(
            x = .data$TRTG,
            y = .data[[ydata[[i]]]],
            fill = .data$TRTG
          )
        } else {
          ggplot2::aes(y = .data[[ydata[[i]]]])
        }
      ) +
      ggplot2::geom_boxplot()

    if (!is.null(residual_references)) {
      .rbpt <- add_horizontal_references(.rbpt, residual_references)
    }

    .rbpt <- cqtkit_style_plot(
      .rbpt,
      style,
      xlabel = "Treatment Group",
      ylabel = ydata[[i]],
      legends = list(
        ggstylekit::legend_spec(
          channel = "fill",
          title = "Treatment Group",
          order = 1
        ),
        ggstylekit::legend_spec(channel = "color", title = "", order = 2)
      )
    )

    trtg_plots[[i]] <- .rbpt
  }

  trt_plot <- combine_panels(
    trtg_plots,
    ncol = 1,
    legend_position = legend_location,
    title = style$title
  )

  return(trt_plot)
}

#' GOF VPC Plot
#'
#' Performs a visual predictive check and plots the results.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param xdata_col An unquoted column name for independent variable
#' @param dv_col An unquoted column name for dependent variable
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param nruns Integer number of simulations to run
#' @param nbins Integer number of bins to break independent variable into - OR - a user specified vector for non-uniform binning
#' @param type Integer for type parameter of stats::quantile
#' @param style A named list of arguments passed to style_plot()
#'
#' @return A visual predictive check plot with observed quantiles overlaid on simulated prediction intervals
#' @export
#'
#' @examples
#' fit <- fit_prespecified_model(
#'   cqtkit_data_verapamil,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#'
#' gof_vpc_plot(cqtkit_data_verapamil, fit, CONC, deltaQTCF, nruns = 2)
#'
gof_vpc_plot <- function(
  data,
  fit,
  xdata_col,
  dv_col,
  conf_int = 0.90,
  nruns = 500,
  nbins = 10,
  type = 2,
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)
  checkmate::assertIntegerish(nruns)
  checkmate::assertNumeric(nbins)
  checkmate::assertIntegerish(type, lower = 1, upper = 9)

  xdata <- rlang::enquo(xdata_col)
  dv <- rlang::enquo(dv_col)

  required_cols <- unlist(lapply(c(xdata, dv), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  # need to better understand lme variance matrix stuff for this.
  if (typeof(fit$apVar) == "character") {
    if (fit$apVar == "Non-positive definite approximate variance-covariance") {
      stop("Fit has issues, try a new model")
    }
  }

  # Add grouping for observations
  data$.group <- "Observations"

  obs <- compute_quantiles_obs_df(
    data,
    !!xdata,
    !!dv,
    conf_int = conf_int,
    nbins = nbins,
    type = type
  )
  stat_cs <- compute_summary_statistics_of_simulations(
    data,
    fit,
    !!xdata,
    conf_int,
    nruns,
    nbins,
    type
  )

  upper_caption <- "95th percentile"
  lower_caption <- "5th percentile"

  # Reshape obs data to long format with groups
  obs_long <- data.frame(
    xdata = rep(obs$xdata, 3),
    ydata = c(obs$high_p_y, obs$med_y, obs$low_p_y),
    .group = rep(
      c("95th percentile", "Median", "5th percentile"),
      each = nrow(obs)
    ),
    linetype = rep(c("dashed", "solid", "dashed"), each = nrow(obs))
  )

  # Reshape stat_cs data for ribbons
  stat_cs_long <- rbind(
    data.frame(
      stat_cs,
      .group = upper_caption,
      ymin = stat_cs$low_high_pred,
      ymax = stat_cs$high_high_pred,
      y = stat_cs$mean_high_pred
    ),
    data.frame(
      stat_cs,
      .group = "Median",
      ymin = stat_cs$low_med_pred,
      ymax = stat_cs$high_med_pred,
      y = stat_cs$mean_med_pred
    ),
    data.frame(
      stat_cs,
      .group = lower_caption,
      ymin = stat_cs$low_low_pred,
      ymax = stat_cs$high_low_pred,
      y = stat_cs$mean_low_pred
    )
  )

  .p <- ggplot2::ggplot(data, ggplot2::aes(x = !!xdata, y = !!dv)) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$.group)
    ) +
    ggplot2::geom_ribbon(
      data = stat_cs_long,
      ggplot2::aes(
        ymin = .data$ymin,
        ymax = .data$ymax,
        y = .data$y,
        x = .data$med_xdata,
        fill = .data$.group
      ),
      alpha = 0.5
    ) +
    suppressWarnings(
      ggplot2::geom_line(
        data = obs_long,
        ggplot2::aes(
          x = .data$xdata,
          y = .data$ydata,
          color = .data$.group
        )
      )
    )

  vpc_colors <- c(
    "Observations" = "grey",
    "95th percentile" = "darkseagreen",
    "Median" = "cornflowerblue",
    "5th percentile" = "darkseagreen"
  )
  .p <- cqtkit_style_plot(
    .p,
    style,
    xlabel = "Concentration (ng/mL)",
    ylabel = bquote(Delta ~ "QTc (ms)"),
    fill_alpha = 0.5,
    colors = vpc_colors,
    fill = vpc_colors,
    legends = list(
      ggstylekit::legend_spec(
        channel = "color",
        title = "Observations",
        order = 1
      ),
      ggstylekit::legend_spec(
        channel = "fill",
        title = paste0(conf_int * 100, "% Confidence Intervals"),
        order = 2
      )
    )
  )
  return(.p)
}
