#' Plots predictions of the model with observed values
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param id_col An unquoted column name for subject ID
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param trt_col An unquoted column name for treatment group
#' @param treatment_predictors A list for predictions with model. Should contain a value for each predictor in the model.
#' @param control_predictors An optional list for contrast predictions
#' @param reference_threshold Optional vector of numbers to add as horizontal dashed lines
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param contrast_method A string specifying contrast method when using control_predictors: "matched" for individual ID+time matching (crossover studies), "group" for group-wise subtraction (parallel studies)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return a plot
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#' fit <- fit_prespecified_model(
#'   data_proc,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#' predict_with_observations_plot(
#'   data_proc,
#'   fit,
#'   CONC,
#'   deltaQTCF,
#'   treatment_predictors = list(
#'     CONC = 0,
#'     TRTG = "Verapamil HCL",
#'     TAFD = "2 HR",
#'     deltaQTCFBL = 0
#'   ),
#'   conf_int = 0.9
#' )
predict_with_observations_plot <- function(
  data,
  fit,
  conc_col,
  dv_col,
  id_col = NULL,
  ntime_col = NULL,
  trt_col = NULL,
  treatment_predictors,
  control_predictors = NULL,
  reference_threshold = c(10),
  conf_int = 0.9,
  contrast_method = c("matched", "group"),
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assertList(treatment_predictors)
  checkmate::assertList(control_predictors, null.ok = TRUE)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1, len = 1)

  conc <- rlang::enquo(conc_col)
  dv <- rlang::enquo(dv_col)
  id <- rlang::enquo(id_col)
  ntime <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(
    c(dv, conc, id, ntime, trt),
    name_quo_if_not_null
  ))
  checkmate::assertNames(names(data), must.include = required_cols)

  observed_df <- compute_contrast_observations(
    data,
    !!conc,
    !!dv,
    !!id,
    !!ntime,
    !!trt,
    treatment_predictors,
    control_predictors,
    contrast_method
  )

  ci_label <- paste0(round(conf_int * 100), "% CI")
  prediction_df <- compute_exposure_predictions(
    data,
    fit,
    !!conc,
    treatment_predictors,
    control_predictors,
    conf_int = conf_int
  ) %>%
    dplyr::mutate(
      group = "Predictions",
      fill = ci_label
    )

  # Combine predictions and observations for unified legend mapping
  combined_data <- dplyr::bind_rows(
    observed_df,
    prediction_df %>% dplyr::select("conc", dv = "pred", "group")
  )

  p <- combined_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$conc,
      y = .data$dv,
      color = .data$group,
      shape = .data$group
    )) +
    ggplot2::geom_ribbon(
      data = prediction_df,
      inherit.aes = FALSE,
      ggplot2::aes(
        x = conc,
        ymin = .data$lower,
        ymax = .data$upper,
        fill = .data$fill
      )
    ) +
    ggplot2::geom_line(
      data = prediction_df %>% dplyr::select("conc", dv = "pred", "group")
    ) +
    ggplot2::geom_point(data = observed_df) +
    ggplot2::theme_bw()

  # Styling attributes for scale functions
  attr(p, "fill_colors") <- stats::setNames("grey", ci_label)
  # default open circle for line group
  attr(p, "secondary_shapes") <- stats::setNames(1, "Predictions")
  # default black color for predictions line
  attr(p, "prediction_colors") <- stats::setNames("black", "Predictions")

  # Add reference line(s)
  p <- add_horizontal_references(p, reference_threshold)

  # Caption
  caption <- paste0(
    "Shaded region represents ",
    round(conf_int * 100),
    "% CI\n"
  )

  if (is.null(control_predictors)) {
    pred_strings <- c()

    for (pred_name in names(treatment_predictors)) {
      if (pred_name != rlang::as_name(conc)) {
        pred_strings <- c(
          pred_strings,
          paste0(pred_name, " = ", treatment_predictors[[pred_name]])
        )
      }
    }

    if (length(pred_strings) > 0) {
      caption <- paste0(
        caption,
        "Predictions made with: ",
        paste(pred_strings, collapse = ", ")
      )
    }
  }

  p <- p + ggplot2::labs(caption = caption)

  # Final styling
  if (is.null(style)) style <- list()

  style$xlabel <- style$xlabel %||% "Concentration (ng/mL)"
  style$ylabel <- style$ylabel %||% bquote(Delta ~ "QTcF (ms)")
  style$colors <- style$colors %||% c("Predictions" = "black")
  style$labels <- style$labels %||%
    c(
      "Predictions" = "Population Predicted dQTcF (ms)",
      "Observations" = "Observations"
    )
  style$fill_alpha <- style$fill_alpha %||% 0.5
  style$fill_legend <- style$fill_legend %||% "Confidence Interval"
  style$legend <- style$legend %||% "Legend"
  style$fill_order <- style$fill_order %||% 2
  style$color_order <- style$color_order %||% 1
  style$shape_order <- style$shape_order %||% 1 # ensures legend stays unified

  p <- do.call(style_plot, c(list(p = p), style))

  return(p)
}


#' Plots predictions and 90% CI
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param conc_col An unquoted column name for concentration measurements
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param id_col An unquoted column name for subject ID
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param trt_col An unquoted column name for treatment group
#' @param treatment_predictors List of a values for contrast. CONC will update
#' @param control_predictors List of b values for contrast
#' @param reference_threshold Optional vector of numbers to add as horizontal dashed lines
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param nbins Number of bins for quantiles, or vector of cut points for computing average
#' @param error_bars A string to denote which errorbars to show, CI, SE, SD or none.
#' @param contrast_method A string specifying contrast method when using control_predictors: "matched" for individual ID+time matching (crossover studies), "group" for group-wise subtraction (parallel studies)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return a plot
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#' fit <- fit_prespecified_model(
#'   data_proc,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#' predict_with_quantiles_plot(
#'   data_proc,
#'   fit,
#'   CONC,
#'   deltaQTCF,
#'   treatment_predictors = list(
#'     CONC = 0,
#'     TRTG = "Verapamil HCL",
#'     TAFD = "2 HR",
#'     deltaQTCFBL = 0
#'   )
#' )
predict_with_quantiles_plot <- function(
  data,
  fit,
  conc_col,
  dv_col,
  id_col = NULL,
  ntime_col = NULL,
  trt_col = NULL,
  treatment_predictors,
  control_predictors = NULL,
  reference_threshold = c(10),
  conf_int = 0.9,
  nbins = 10,
  error_bars = "CI",
  contrast_method = c("matched", "group"),
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assertList(treatment_predictors)
  checkmate::assertList(control_predictors, null.ok = TRUE)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1, len = 1)
  checkmate::assert_choice(error_bars, c("CI", "SE", "SD"), null.ok = TRUE)
  checkmate::assertNumeric(nbins)

  conc <- rlang::enquo(conc_col)
  dv <- rlang::enquo(dv_col)
  id <- rlang::enquo(id_col)
  ntime <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(
    c(dv, conc, id, ntime, trt),
    name_quo_if_not_null
  ))
  checkmate::assertNames(names(data), must.include = required_cols)

  observed_df <- compute_contrast_observations(
    data,
    !!conc,
    !!dv,
    !!id,
    !!ntime,
    !!trt,
    treatment_predictors,
    control_predictors,
    contrast_method
  )

  obs <- compute_quantiles_obs_df(
    observed_df,
    conc,
    dv,
    conf_int,
    nbins = nbins
  ) %>%
    dplyr::mutate(group = "Quantiles")

  ci_label <- paste0(round(conf_int * 100), "% CI")
  qtc_pred <- compute_exposure_predictions(
    data,
    fit,
    !!conc,
    treatment_predictors,
    control_predictors,
    conf_int
  ) %>%
    dplyr::mutate(
      group = "Predictions",
      fill = ci_label
    )

  # Set up unified plot mapping
  p <- ggplot2::ggplot(
    data = obs,
    ggplot2::aes(
      x = .data$xdata,
      y = .data$mean_dv,
      color = .data$group,
      shape = .data$group
    )
  ) +
    ggplot2::geom_ribbon(
      data = qtc_pred,
      ggplot2::aes(
        x = .data$conc,
        y = .data$pred,
        ymin = .data$lower,
        ymax = .data$upper,
        fill = .data$fill
      ),
      inherit.aes = FALSE,
      color = NA
    ) +
    ggplot2::geom_line(
      data = qtc_pred,
      ggplot2::aes(
        x = .data$conc,
        y = .data$pred,
        color = .data$group,
        group = .data$group,
      ),
    ) +
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(shape = .data$group, color = .data$group)
    )

  # Error bars
  p <- add_error_bars_to_plot(obs, p, NULL, error_bars, conf_int)

  # Caption
  caption <- p$labels$caption
  if (is.null(control_predictors)) {
    pred_strings <- c()

    for (pred_name in names(treatment_predictors)) {
      if (pred_name != rlang::as_name(conc)) {
        pred_strings <- c(
          pred_strings,
          paste0(pred_name, " = ", treatment_predictors[[pred_name]])
        )
      }
    }

    if (length(pred_strings) > 0) {
      caption <- paste0(
        caption,
        "Predictions made with: ",
        paste(pred_strings, collapse = "\n ")
      )
    }
  }

  # Add attributes for styling
  attr(p, "fill_colors") <- stats::setNames("grey", ci_label)
  attr(p, "secondary_shapes") <- stats::setNames(1, "Predictions")
  # default black color for predictions line
  attr(p, "prediction_colors") <- stats::setNames("black", "Predictions")

  p <- add_horizontal_references(p, reference_threshold)
  p <- p + ggplot2::theme_bw() + ggplot2::labs(caption = caption)

  # Style
  if (is.null(style)) style <- list()
  style$xlabel <- style$xlabel %||% "Concentration (ng/mL)"
  style$ylabel <- style$ylabel %||% bquote(Delta ~ "QTc (ms)")
  style$fill_alpha <- style$fill_alpha %||% 0.5
  style$legend <- style$legend %||% "Legend"
  style$fill_legend <- style$fill_legend %||% "Confidence Interval"
  style$colors <- style$colors %||% c("Predictions" = "black")
  style$color_order <- style$color_order %||% 1
  style$shape_order <- style$shape_order %||% 1
  style$fill_order <- style$fill_order %||% 2

  p <- do.call(style_plot, c(list(p = p), style))

  return(p)
}


#' Plots model predictions with therapeutic and supra therapeutic Cmax
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param treatment_predictors List of a values for contrast. CONC will update
#' @param control_predictors List of b values for contrast
#' @param reference_threshold Optional vector of numbers to add as horizontal dashed lines
#' @param cmaxes Optional - numeric vector of Cmax values to add as reference lines
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param style A named list of arguments passed to style_plot()
#'
#' @return a plot
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#' fit <- fit_prespecified_model(
#'   data_proc,
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   TRUE
#' )
#' pk_df <- compute_pk_parameters(
#'   data_proc %>% dplyr::filter(DOSE != 0), ID, DOSEF, CONC, NTLD)
#'
#' predict_with_exposure_plot(
#'   data_proc,
#'   fit,
#'   CONC,
#'   treatment_predictors = list(
#'     CONC = 0,
#'     deltaQTCFBL = 0,
#'     TRTG = "Verapamil HCL",
#'     TAFD = "2 HR"
#'   ),
#'   control_predictors = list(
#'     CONC = 0,
#'     deltaQTCFBL = 0,
#'     TRTG = "Placebo",
#'     TAFD = "2 HR"
#'   ),
#'   cmaxes = pk_df[[1, "Cmax_gm"]], # Dose = 120
#' )
predict_with_exposure_plot <- function(
  data,
  fit,
  conc_col,
  treatment_predictors,
  control_predictors = NULL,
  reference_threshold = c(10),
  cmaxes = NULL,
  conf_int = 0.90,
  style = list()
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assertList(treatment_predictors)
  checkmate::assertList(control_predictors, null.ok = TRUE)
  checkmate::assertNumeric(cmaxes, null.ok = TRUE)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1, len = 1)

  conc <- rlang::enquo(conc_col)

  required_cols <- unlist(lapply(c(conc), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  # need to better understand lme variance matrix stuff for this.
  if (typeof(fit$apVar) == "character") {
    if (fit$apVar == "Non-positive definite approximate variance-covariance") {
      stop("Fit has issues, try a new model")
    }
  }

  pred_df <- compute_exposure_predictions(
    data,
    fit,
    !!conc,
    treatment_predictors,
    control_predictors,
    cmaxes,
    conf_int
  )

  ci_label <- paste0(round(conf_int * 100), "% CI")
  pred_df$fill <- ci_label

  p <- pred_df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$conc, y = .data$pred)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper, fill = .data$fill)
    ) +
    ggplot2::theme_bw()

  attr(p, "fill_colors") <- stats::setNames("grey", ci_label)

  p <- add_horizontal_references(p, reference_threshold)

  if (!is.null(cmaxes)) {
    # Create data frame for Cmax reference lines
    cmax_data <- data.frame(
      cmax = cmaxes,
      group = paste0("Cmax_", round(cmaxes, 2)),
      stringsAsFactors = FALSE
    )

    # Add corresponding dQTC values
    cmax_data$dqtc <- sapply(cmaxes, function(x) {
      pred_df$upper[which(pred_df$conc == x)]
    })

    # Add vertical lines
    p <- p +
      ggplot2::geom_segment(
        data = cmax_data,
        ggplot2::aes(
          x = .data$cmax,
          y = -Inf,
          xend = .data$cmax,
          yend = .data$dqtc,
          color = .data$group
        ),
        linetype = "dashed"
      ) +
      ggplot2::geom_segment(
        data = cmax_data,
        ggplot2::aes(
          x = -Inf,
          y = .data$dqtc,
          xend = .data$cmax,
          yend = .data$dqtc,
          color = .data$group
        ),
        linetype = "dashed"
      )
  }

  if (is.null(style)) style <- list()
  style$xlabel <- style$xlabel %||% "Concentration (ng/mL)"
  style$ylabel <- style$ylabel %||%
    if (!is.null(control_predictors)) {
      bquote(Delta ~ Delta ~ "QTc (ms)")
    } else {
      bquote(Delta ~ "QTc (ms)")
    }
  style$fill_alpha <- style$fill_alpha %||% 0.5
  style$fill_legend <- style$fill_legend %||% "Confidence Interval"
  style$color_order <- style$color_order %||% 1
  style$fill_order <- style$fill_order %||% 2
  style$legend <- style$legend %||% "Exposure"

  caption <- paste0(
    "Shaded region represents ",
    round(conf_int * 100),
    "% CI\n"
  )

  if (is.null(control_predictors)) {
    pred_strings <- c()

    for (pred_name in names(treatment_predictors)) {
      if (pred_name != rlang::as_name(conc)) {
        pred_strings <- c(
          pred_strings,
          paste0(pred_name, " = ", treatment_predictors[[pred_name]])
        )
      }
    }

    if (length(pred_strings) > 0) {
      caption <- paste0(
        caption,
        "Predictions made with: ",
        paste(pred_strings, collapse = ", ")
      )
    }
  }

  p <- do.call(style_plot, c(list(p = p), style))
  p <- p + ggplot2::labs(caption = caption)

  return(p)
}
