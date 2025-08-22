#QTc computation functions for background calculations

#' Fits a linear model of input dataframe
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param xdata_col An unquoted column name for independent variable measurements
#' @param ydata_col An unquoted column name for dependent variable measurements
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @return the fitted parameters of a lm of y ~ x
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' compute_lm_fit_df(data_proc, RR, QT)
compute_lm_fit_df <- function(data, xdata_col, ydata_col, conf_int = 0.95) {
  checkmate::assertDataFrame(data)

  x_data <- rlang::enquo(xdata_col)
  y_data <- rlang::enquo(ydata_col)

  required_cols <- unlist(lapply(c(x_data, y_data), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  x_str <- rlang::as_label(x_data)
  y_str <- rlang::as_label(y_data)

  ###### ADD IN TRT SPECIFIC LM...? ######
  f <- stats::as.formula(
    paste(y_str, "~", x_str)
  )

  lm_model <- stats::lm(f, data = data)
  intercept <- stats::coef(lm_model)[[1]]
  slope <- stats::coef(lm_model)[[2]]

  ci <- stats::confint(lm_model, level = conf_int)

  p_value_intercept <- summary(lm_model)$coefficients[[
    "(Intercept)",
    "Pr(>|t|)"
  ]]
  p_value_slope <- summary(lm_model)$coefficients[[x_str, "Pr(>|t|)"]]

  return(tibble::tibble(
    "intercept" = intercept,
    "slope" = slope,
    "intercept_ci_lower" = ci["(Intercept)", 1],
    "intercept_ci_upper" = ci["(Intercept)", 2],
    "slope_ci_lower" = ci[x_str, 1],
    "slope_ci_upper" = ci[x_str, 2],
    "p_value_intercept" = p_value_intercept,
    "p_value_slope" = p_value_slope,
    "conf_int" = conf_int
  ))
}

#' Takes the slope from a lme model and its confidence interval
#'
#' @param lme_mod An nlme::lme model object from model fitting
#' @param xdata_col An unquoted name of column used as independent data in LME
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @return tibble of slope, lower_ci, upper_ci
#' @export
#'
#' @examples
#' df <- cqtkit_data_verapamil %>% preprocess()
#'
#' lme_mod <- df %>%
#'   fit_qtc_linear_model(
#'       QT,
#'       RR,
#'       ID,
#'       method = 'REML',
#'       remove_rr_iiv = FALSE
#'   )
#'
#' slope_data <- compute_lme_slope_df(lme_mod, RR, 0.9)
compute_lme_slope_df <- function(lme_mod, xdata_col, conf_int = 0.95) {
  ###QC: Check model type and that slope exists???
  xdata <- rlang::enquo(xdata_col)

  estimates <- compute_model_fit_parameters(
    lme_mod,
    trt_col_name = NULL,
    tafd_col_name = NULL,
    id_col_name = NULL,
    conf_int = conf_int
  )

  slope <- estimates %>%
    dplyr::filter(.data$Parameters == rlang::quo_name(xdata)) %>%
    dplyr::pull(.data$Value)

  slope_ci_lower <- estimates %>%
    dplyr::filter(.data$Parameters == rlang::quo_name(xdata)) %>%
    dplyr::pull(.data$CIl)

  slope_ci_upper <- estimates %>%
    dplyr::filter(.data$Parameters == rlang::quo_name(xdata)) %>%
    dplyr::pull(.data$CIu)

  return(tibble::tibble(
    "slope" = slope,
    "slope_ci_lower" = slope_ci_lower,
    "slope_ci_upper" = slope_ci_upper,
  ))
}

#' Generates pk parameters Cmax and Tmax for exporsure predictions.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param id_col An unquoted column name for subject ID
#' @param dose_col An unquoted column name for dose measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param ntime_col An unquoted column name for nominal times
#' @param group_col An unquoted column name for additional grouping variable
#'
#' @importFrom rlang .data
#'
#' @return a dataframe of pk parameters
#' @export
#'
#' @examples
#' compute_pk_parameters(
#'   preprocess(cqtkit_data_verapamil) %>% dplyr::filter(DOSE != 0),
#'   ID,
#'   DOSE,
#'   CONC,
#'   NTLD)
compute_pk_parameters <- function(
  data,
  id_col,
  dose_col,
  conc_col,
  ntime_col,
  group_col = NULL
) {
  checkmate::assertDataFrame(data)

  id <- rlang::enquo(id_col)
  dose <- rlang::enquo(dose_col)
  conc <- rlang::enquo(conc_col)
  ntld <- rlang::enquo(ntime_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(lapply(
    c(id, dose, conc, ntld, group),
    name_quo_if_not_null
  ))
  checkmate::assertNames(names(data), must.include = required_cols)

  pk_df <- tibble::tibble(
    id = data %>% dplyr::pull(!!id),
    dose = data %>% dplyr::pull(!!dose),
    conc = data %>% dplyr::pull(!!conc),
    ntld = data %>% dplyr::pull(!!ntld)
  )
  if (!rlang::quo_is_null(group)) {
    pk_df$group <- data %>% dplyr::pull(!!group)
  } else {
    pk_df$group <- pk_df$dose
  }

  if (any(is.na(pk_df$conc))) {
    warning(
      "Your CONC data contains NA and is removed in calculations of this function"
    )
  }
  if (any(is.na(pk_df$ntld))) {
    warning(
      "Your TIME data contains NA and is removed in calculations of this function"
    )
  }

  if (rlang::quo_is_null(group)) {
    pk_df$group <- pk_df$dose
  } else {
    pk_df$group <- paste(pk_df$group, pk_df$dose)
  }

  pk_params_df <- pk_df %>%
    dplyr::group_by(.data$id, .data$group) %>%
    dplyr::mutate(
      Tmax = .data$ntld[which.max(.data$conc)],
      Cmax = .data$conc[which.max(.data$conc)] #what if CMax is 0?
    )
  if (0 %in% pk_params_df$Cmax) {
    message("0 Cmax found, maybe try filter(DOSE != 0)")
  }
  checkmate::assert(all(pk_params_df$Cmax > 0))

  pk_params_df <- pk_params_df %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(
      N = dplyr::n_distinct(.data$id),
      Tmax_median = stats::median(.data$Tmax, na.rm = TRUE),
      Tmax_max = max(.data$Tmax, na.rm = TRUE),
      Tmax_min = min(.data$Tmax, na.rm = TRUE),
      Cmax_gm = exp(mean(log(.data$Cmax))),
      Cmax_cv = sqrt(exp(stats::sd(log(.data$Cmax), na.rm = TRUE)**2) - 1) *
        100,
      Cmax_median = stats::median(.data$Cmax, na.rm = TRUE),
      Cmax_max = max(.data$Cmax, na.rm = TRUE),
      Cmax_min = min(.data$Cmax, na.rm = TRUE)
    )
  return(pk_params_df)
}

#' Computes the number of subjects with QTc > 450, 500 as well as deltaQTc > 30, 60
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param qtc_col An unquoted column name for containing QTc data
#' @param deltaqtc_col An unquoted column name for containing deltaQTc data
#' @param group_col An optional column name for grouping data
#'
#' @return a tibble containing the number of subjects with high QTc values
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' compute_high_qtc_sub(data_proc, QTCF, deltaQTCF)
compute_high_qtc_sub <- function(
  data,
  qtc_col,
  deltaqtc_col,
  group_col = NULL
) {
  checkmate::assertDataFrame(data)

  qtc <- rlang::enquo(qtc_col)
  deltaqtc <- rlang::enquo(deltaqtc_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(lapply(c(qtc, deltaqtc, group), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  qtdf <- tibble::tibble(
    qtc = data %>% dplyr::pull(!!qtc),
    deltaqtc = data %>% dplyr::pull(!!deltaqtc)
  )

  if (!rlang::quo_is_null(group)) {
    qtdf <- qtdf %>%
      dplyr::mutate(group = data %>% dplyr::pull(!!group))
    n_gt <- qtdf %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarise(
        n_QTc_gt_450 = sum(.data$qtc > 450, na.rm = TRUE),
        n_QTc_gt_480 = sum(.data$qtc > 480, na.rm = TRUE),
        n_QTc_gt_500 = sum(.data$qtc > 500, na.rm = TRUE),
        n_dQTc_gt_30 = sum(.data$deltaqtc > 30, na.rm = TRUE),
        n_dQTc_gt_60 = sum(.data$deltaqtc > 60, na.rm = TRUE)
      )
  } else {
    n_gt <- tibble::tibble(
      group = "Total",
      n_QTc_gt_450 = qtdf %>% dplyr::filter(.data$qtc > 450) %>% nrow(),
      n_QTc_gt_480 = qtdf %>% dplyr::filter(.data$qtc > 480) %>% nrow(),
      n_QTc_gt_500 = qtdf %>% dplyr::filter(.data$qtc > 500) %>% nrow(),
      n_dQTc_gt_30 = qtdf %>% dplyr::filter(.data$deltaqtc > 30) %>% nrow(),
      n_dQTc_gt_60 = qtdf %>% dplyr::filter(.data$deltaqtc > 60) %>% nrow()
    )
  }
  return(n_gt)
}

#' Creates a dataframe summarizing number of subjects in each trtreatment group
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param trt_col Column name of treatment group
#' @param id_col Column name of ID
#' @param group_col Optional additional grouping column
#'
#' @return a tibble of number of subjects in trt_col (or trt_col + group_col) along with total id_col
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' compute_study_summary(data_proc, TRTG, ID)
compute_study_summary <- function(data, trt_col, id_col, group_col = NULL) {
  checkmate::assertDataFrame(data)

  trt <- rlang::enquo(trt_col)
  group <- rlang::enquo(group_col)
  id <- rlang::enquo(id_col)

  required_cols <- unlist(lapply(c(trt, id, group), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  df <- tibble::tibble(
    id = data %>% dplyr::pull(!!id),
    trt = data %>% dplyr::pull(!!trt)
  )
  if (!rlang::quo_is_null(group)) {
    df$group <- data %>% dplyr::pull(!!group)
  }

  if (!rlang::quo_is_null(group)) {
    df <- df %>%
      dplyr::mutate(
        grouping = paste(.data$trt, .data$group)
      )
  } else {
    df <- df %>%
      dplyr::mutate(
        grouping = paste(.data$trt)
      )
  }

  table <- df %>%
    dplyr::group_by(.data$grouping) %>%
    dplyr::summarize(n_sub = dplyr::n_distinct(.data$id)) %>%
    dplyr::add_row(
      grouping = "Total",
      n_sub = dplyr::n_distinct(df$id),
      .before = 1
    )

  return(table)
}

#' Generates a tibble of summary of QTc, dQTc and ddQTc over time stratified by dose
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param ntime_col An unquoted column name for nominal time data
#' @param dose_col An unquoted column name for dose data
#' @param ecg_param_col An unquoted column name for QTc measurements
#' @param deltaecg_param_col An unquoted column name for deltaQTc measurements
#' @param group_col An unquoted column name for additional grouping column
#' @param reference_dose Reference dose value for comparison calculations
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @return a tibble of QTc/deltaQTc/delta delta QTc summary over dose and time.
#' @export
#'
#' @examples
#' data_proc <- cqtkit_data_verapamil %>% preprocess()
#'
#' compute_ecg_param_summary(data_proc, NTLD, DOSEF, QTCF, deltaQTCF)
compute_ecg_param_summary <- function(
  data,
  ntime_col,
  dose_col,
  ecg_param_col,
  deltaecg_param_col,
  group_col = NULL,
  reference_dose = NULL,
  conf_int = 0.95
) {
  checkmate::assertDataFrame(data)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)

  ntld <- rlang::enquo(ntime_col)
  dose <- rlang::enquo(dose_col)
  ecg <- rlang::enquo(ecg_param_col)
  deltaecg <- rlang::enquo(deltaecg_param_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(lapply(
    c(ntld, dose, ecg, deltaecg, group),
    name_quo_if_not_null
  )) #helper.R
  checkmate::assertNames(names(data), must.include = required_cols)

  selections <- c("dose", "time", "n", "mean_dv", "ci_low", "ci_high", "group")

  ecg_summ <-
    compute_grouped_mean_sd(
      data,
      !!ecg,
      !!ntld,
      !!dose,
      group_col = !!group,
      reference_dose = reference_dose,
      conf_int = conf_int
    ) %>%
    dplyr::arrange(dose) %>%
    dplyr::select(selections) %>%
    dplyr::rename(
      mean_ecg = .data$mean_dv,
      ecg_low = .data$ci_low,
      ecg_high = .data$ci_high
    )

  if (!is.null(reference_dose)) {
    selections <- c(
      selections,
      c("mean_delta_dv", "ci_low_delta", "ci_up_delta")
    )
  }

  decg_summ <- compute_grouped_mean_sd(
    data,
    !!deltaecg,
    !!ntld,
    !!dose,
    !!group,
    reference_dose = reference_dose,
    conf_int = conf_int
  ) %>%
    dplyr::arrange(dose) %>%
    dplyr::select(selections) %>%
    dplyr::rename(
      mean_decg = .data$mean_dv,
      decg_low = .data$ci_low,
      decg_high = .data$ci_high
    )

  if (!is.null(reference_dose)) {
    decg_summ <- decg_summ %>%
      dplyr::rename(
        mean_ddecg = .data$mean_delta_dv,
        ddecg_low = .data$ci_low_delta,
        ddecg_high = .data$ci_up_delta
      )
  }

  summ <- dplyr::left_join(
    ecg_summ,
    decg_summ,
    by = c("dose", "time", "n", "group")
  )
  return(tibble::as_tibble(summ))
}

#' computes a the mean and standard deviation of the dependent variable data
#' in DV_col grouped by time and dose.
#'
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param dv_col An unquoted column name for dependent variable
#' @param ntime_col An unquoted column name for the Time group
#' @param dose_col An unquoted column name for dose group
#' @param group_col An unquoted column of optional grouping column
#' @param reference_dose Reference dose value for comparison calculations
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @return a dataframe of the dv averaged over the grouped time and dose
#' @export
#' @importFrom rlang .data
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#' data_proc <- dplyr::mutate(data_proc, DOSEF = as.factor(DOSEF))
#'
#' compute_grouped_mean_sd(
#'   data_proc, deltaQTCF, NTLD, DOSE, reference_dose = 0
#' )
compute_grouped_mean_sd <- function(
  data,
  dv_col,
  ntime_col,
  dose_col,
  group_col = NULL,
  reference_dose = NULL,
  conf_int = 0.95
) {
  checkmate::assertDataFrame(data)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)

  dv <- rlang::enquo(dv_col)
  time <- rlang::enquo(ntime_col)
  dose <- rlang::enquo(dose_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(lapply(
    c(dv, time, dose, group),
    name_quo_if_not_null
  ))
  checkmate::assertNames(names(data), must.include = required_cols)

  ##########################
  df <- tibble::tibble(
    dv = data %>% dplyr::pull(!!dv),
    time = data %>% dplyr::pull(!!time),
    dose = data %>% dplyr::pull(!!dose),
  )
  if (!rlang::quo_is_null(group)) {
    df$group <- data %>% dplyr::pull(!!group)
  }

  if (!is.null(reference_dose)) {
    checkmate::assert_choice(
      as.character(reference_dose),
      as.character(df$dose)
    )
  }

  nrow_df <- df %>% nrow()
  nrow_time_grouped_df <- df$time %>% unique() %>% length()
  nrow_dose_grouped_df <- df$dose %>% unique() %>% length()

  if (nrow_df == nrow_time_grouped_df) {
    stop(
      "Grouping by ntime_col does not reduce size. Ensure correct data is supplied."
    )
  }

  if (nrow_df == nrow_dose_grouped_df) {
    stop(
      "Grouping by dosef_col does not reduce size. Ensure correct data is supplied."
    )
  }

  if (!rlang::quo_is_null(group)) {
    nrow_group_grouped_df <- df$group %>% unique() %>% length()

    if (nrow_df == nrow_group_grouped_df) {
      stop(
        "Grouping by group_col does not reduce size. Ensure correct data is supplied."
      )
    }
  }

  if (any(is.na(df$dv))) {
    warning(
      "Your DV data contains NA and is removed in calculations of this function"
    )
    #give rows that were filtered out
  }
  if (any(is.na(df$time))) {
    warning(
      "Your TIME data contains NA and is removed in calculations of this function"
    )
  }
  if (any(is.na(df$dose))) {
    warning(
      "Your DOSE data contains NA and is removed in calculations of this function"
    )
  }
  ############################ - this could be prep_data function return qc_df

  if (!rlang::quo_is_null(group)) {
    df <- df %>%
      dplyr::mutate(
        grouping = paste(.data$dose, .data$group)
      ) %>%
      dplyr::group_by(.data$time, .data$dose, .data$group)
  } else {
    df <- df %>%
      dplyr::mutate(
        grouping = paste(.data$dose)
      ) %>%
      dplyr::group_by(.data$time, .data$dose)
  }

  df <- df %>%
    dplyr::summarize(
      mean_dv = mean(.data$dv, na.rm = TRUE),
      sd = stats::sd(.data$dv, na.rm = TRUE),
      geo_mean_dv = ifelse(
        any(.data$dv == 0, na.rm = TRUE),
        0,
        ifelse(
          any(.data$dv < 0, na.rm = TRUE),
          NA,
          exp(mean(log(.data$dv[!is.na(.data$dv)]), na.rm = TRUE))
        )
      ),
      geo_sd_dv = ifelse(
        any(.data$dv == 0, na.rm = TRUE),
        0,
        ifelse(
          any(.data$dv < 0, na.rm = TRUE),
          NA,
          exp(stats::sd(log(.data$dv[!is.na(.data$dv)]), na.rm = TRUE))
        )
      ),
      n = sum(!is.na(.data$dv)),
      se = .data$sd / sqrt(.data$n),
      ci_low = .data$mean_dv -
        stats::qt((1 + conf_int) / 2, df = .data$n - 1) * .data$se,
      ci_high = .data$mean_dv +
        stats::qt((1 + conf_int) / 2, df = .data$n - 1) * .data$se,
      group = dplyr::first(.data$grouping),
      .groups = "keep"
    )

  if (!is.null(reference_dose)) {
    delta_df <- df %>%
      dplyr::group_by(.data$time) %>%
      dplyr::mutate(
        mean_delta_dv = .data$mean_dv -
          .data$mean_dv[.data$dose == reference_dose],
        geomean_delta_dv = .data$geo_mean_dv -
          .data$geo_mean_dv[.data$dose == reference_dose],
        delta_sd = sqrt(
          (.data$sd)**2 + (.data$sd[.data$dose == reference_dose])**2
        ),
        delta_se = sqrt(
          .data$sd**2 /
            .data$n +
            .data$sd[.data$dose == reference_dose]**2 /
              .data$n[.data$dose == reference_dose]
        ),
        ci_low_delta = .data$mean_delta_dv -
          stats::qt(
            (1 + conf_int) / 2,
            df = .data$n + .data$n[.data$dose == reference_dose] - 2
          ) *
            .data$delta_se,
        ci_up_delta = .data$mean_delta_dv +
          stats::qt(
            (1 + conf_int) / 2,
            df = .data$n + .data$n[.data$dose == reference_dose] - 2
          ) *
            .data$delta_se
      )

    return(delta_df)
  } else {
    return(df)
  }
}

#' Computes R-squared and Adjusted R-squared of loess regression
#' compared to linear regression. Used for determining linearity
#' of C-QT data
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param deltaqtc_col An unquoted column name for dQTCF measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param span A fractional value for LOESS span parameter in geom_smooth if LOESS is used, default 0.99
#'
#' @return a tibble of R_squared and adjusted R_squared
#' @export
#'
#' @examples
#' compute_loess_linear_r_squared(cqtkit_data_verapamil %>% preprocess(), deltaQTCF, CONC)
compute_loess_linear_r_squared <- function(
  data,
  deltaqtc_col,
  conc_col,
  span = 0.99
) {
  dqtc <- rlang::enquo(deltaqtc_col)
  conc <- rlang::enquo(conc_col)

  conc_str <- rlang::as_label(dqtc)
  dqtc_str <- rlang::as_label(conc)

  f <- stats::as.formula(
    paste(dqtc_str, "~", conc_str, "+ 1")
  )

  lin_reg <- stats::lm(formula = f, data = data)
  loess_reg <- stats::loess(formula = f, data = data, span = span)

  lin_reg_pred <- stats::predict(lin_reg)
  loess_reg_pred <- stats::predict(loess_reg)

  ss_res <- sum(
    (lin_reg_pred - loess_reg_pred)^2
  )
  ss_tot <- sum(
    (lin_reg_pred - mean(lin_reg_pred))^2
  )

  n <- lin_reg$model %>% nrow()
  enp <- loess_reg$enp

  r_squared <- 1 - ss_res / ss_tot
  r_squared_adj <- 1 - (1 - r_squared) * ((n - 1) / (n - enp - 1))

  # Coefficient of partial determination
  r_parial_det <- 1 - sum(loess_reg$residuals^2) / sum(lin_reg$residuals^2)

  return(
    tibble::tibble(
      R_squared = c(r_squared),
      R_squared_adj = c(r_squared_adj),
      R_partial_det = c(r_parial_det)
    )
  )
}

#' Detects the pressence of hysteresis
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param ntime_col An unquoted column name for nominal timepoints
#' @param deltaqtc_col An unquoted column name for dQTC measurements
#' @param conc_col An unquoted column name for concentration measurements
#' @param group_col An unquoted column name for grouping column - usually DOSEF
#'
#' @return a bool of TRUE if hysteresis detected else FALSE
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#' data_proc <- dplyr::filter(data_proc, DOSE == 120)
#'
#' compute_potential_hysteresis(
#'   data_proc,
#'   NTLD,
#'   deltaQTCF,
#'   CONC,
#'   DOSEF)
compute_potential_hysteresis <- function(
  data,
  ntime_col,
  deltaqtc_col,
  conc_col,
  group_col
) {
  checkmate::assertDataFrame(data)

  ntld <- rlang::enquo(ntime_col)
  deltaqtc <- rlang::enquo(deltaqtc_col)
  conc <- rlang::enquo(conc_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(lapply(
    c(ntld, deltaqtc, conc, group),
    name_quo_if_not_null
  ))
  checkmate::assertNames(names(data), must.include = required_cols)

  qtc_conc_df <- tibble::tibble(
    ntld = data %>% dplyr::pull(!!ntld),
    deltaqtc = data %>% dplyr::pull(!!deltaqtc),
    conc = data %>% dplyr::pull(!!conc),
    group = data %>% dplyr::pull(!!group)
  )

  if (any(is.na(qtc_conc_df$deltaqtc))) {
    warning(
      "Your deltaQTC data contains NA and is removed in calculations of this function"
    )
  }
  if (any(is.na(qtc_conc_df$conc))) {
    warning(
      "Your CONC data contains NA and is removed in calculations of this function"
    )
  }

  if (length(qtc_conc_df$group %>% unique()) != 1)
    message("Only input single dose data")
  stopifnot(length(qtc_conc_df$group %>% unique()) == 1)

  if (length(qtc_conc_df$ntld %>% unique()) < 3)
    message("Three time points are needed within NTIME")
  stopifnot(length(qtc_conc_df$ntld %>% unique()) > 3)

  qtc_conc_df <- qtc_conc_df %>%
    dplyr::group_by(.data$ntld, .groups = "keep") %>%
    dplyr::mutate(
      mean_conc = mean(.data$conc, na.rm = TRUE),
      mean_dqtc = mean(.data$deltaqtc, na.rm = TRUE)
    ) %>%
    dplyr::select(-conc, -deltaqtc) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$ntld) %>%
    dplyr::ungroup()

  high_qtc_counter <- 0
  for (dqtc in qtc_conc_df$mean_dqtc) {
    if (dqtc > 5) {
      #Might want to change this to a variable filter in future
      high_qtc_counter <- high_qtc_counter + 1
    }
  }

  #check for multiple times with max dQTCF or CONC
  max_conc <- max(qtc_conc_df$mean_conc)
  max_dqtc <- max(qtc_conc_df$mean_dqtc)

  if (length(which(qtc_conc_df$mean_conc == max_conc)) > 1) {
    warning("Multiple times have max CONC - first time is used")
  }
  if (length(which(qtc_conc_df$mean_dqtc == max_dqtc)) > 1) {
    warning("Multiple times have max dQTCF - first time is used")
  }

  tmax <- qtc_conc_df$ntld[which.max(qtc_conc_df$mean_conc)]
  umax <- qtc_conc_df$ntld[which.max(qtc_conc_df$mean_dqtc)]

  large_time_diff <- FALSE
  if (umax - tmax >= 1) {
    large_time_diff <- TRUE
  }

  if (high_qtc_counter > 3 && large_time_diff) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Gets labeller for hysteresis_loop_plot. Should most likely not be used by user
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param ntime_col An unquoted column name for Nominal time since last dose (h)
#' @param deltaqtc_col An unquoted column name for dQTC measurements (ms)
#' @param conc_col An unquoted column name for CONC measurements (units)
#' @param dosef_col An unquoted column name for doses as factor
#' @param group_col An unquoted column name for additional grouping variable.
#'
#' @return list with compute_potential_hysteresis results for each dose.
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#' data_proc <- dplyr::mutate(data_proc, DOSEF = as.factor(DOSEF))
#'
#' compute_hysteresis_labeller(
#'   data_proc,
#'   NTLD,
#'   deltaQTCF,
#'   CONC,
#'   DOSEF)
compute_hysteresis_labeller <- function(
  data,
  ntime_col,
  deltaqtc_col,
  conc_col,
  dosef_col,
  group_col = NULL
) {
  checkmate::assertDataFrame(data)

  ntld <- rlang::enquo(ntime_col)
  deltaqtc <- rlang::enquo(deltaqtc_col)
  conc <- rlang::enquo(conc_col)
  dosef <- rlang::enquo(dosef_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(
    lapply(
      c(ntld, deltaqtc, conc, dosef, group),
      name_quo_if_not_null
    )
  )
  checkmate::assertNames(names(data), must.include = required_cols)

  qtc_conc_df <- tibble::tibble(
    ntld = data %>% dplyr::pull(!!ntld),
    deltaqtc = data %>% dplyr::pull(!!deltaqtc),
    conc = data %>% dplyr::pull(!!conc),
    dosef = data %>% dplyr::pull(!!dosef),
  )

  if (!rlang::quo_is_null(group)) {
    qtc_conc_df$group <- data %>% dplyr::pull(!!group)
  } else {
    qtc_conc_df$group <- factor(
      data %>% dplyr::pull(!!dosef),
      levels = levels(data %>% dplyr::pull(!!dosef))
    )
  }
  checkmate::assert_factor(qtc_conc_df$dosef)

  if (is.factor(qtc_conc_df$ntld)) {
    message(
      "ntime should not be a factor - try supplying non factor time vector"
    )
  }
  stopifnot(!is.factor(qtc_conc_df$ntld))

  group_values <- qtc_conc_df$group
  group_levels <- if (is.factor(group_values)) {
    levels(group_values)
  } else {
    gtools::mixedsort(unique(group_values))
  }

  hysteresis <- list()
  for (dose in group_levels) {
    h <- compute_potential_hysteresis(
      data = qtc_conc_df %>% dplyr::filter(.data$group == dose),
      ntime_col = ntld,
      deltaqtc_col = deltaqtc,
      conc_col = conc,
      group_col = group
    )

    hysteresis[[dose]] <- if (h) {
      paste(dose, ": Hysteresis detected")
    } else {
      dose
    }
  }

  dose_labeller <- function(variable, dose) {
    return(hysteresis)
  }
  return(dose_labeller)
}

#' Exposure Normalized GRI computation
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param conc_gm_col CONC geometric mean column name
#' @param ddqtc_col Delta delta QTc column name
#'
#' @return a numeric (ms) of enGRI score
#' @export
#'
#' @examples
#' compute_enGRI(preprocess(cqtkit_data_verapamil), CONC, deltaQTCF)
compute_enGRI <- function(data, conc_gm_col, ddqtc_col) {
  #calculate the exposure normalised Glomb-Ring Index
  checkmate::assertDataFrame(data)

  # This comes from supplemental info for [@Ferber2020]
  # CONC geometric mean concentration
  # dd mean effect, both ordered by time.
  conc <- rlang::enquo(conc_gm_col)
  ddqtc <- rlang::enquo(ddqtc_col)

  required_cols <- unlist(lapply(c(conc, ddqtc), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  df <- tibble::tibble(
    conc = data %>% dplyr::pull(!!conc),
    ddqtc = data %>% dplyr::pull(!!ddqtc)
  )

  if (length(df$conc) != length(df$ddqtc)) {
    message("CONC and ddQTCF are different lengths. Check inputs.")
  }
  stopifnot(length(df$conc) == length(df$ddqtc))

  CP <- c(df$conc, 0)
  ddQ <- c(df$ddqtc, 0)
  (t(utils::head(CP, -1) - utils::tail(CP, -1)) %*%
    (utils::head(ddQ, -1) + utils::tail(ddQ, -1)) /
    2)[1, 1] /
    max(df$conc)
}

#' returns a dataframe of quantiles of concentrations and deltaQTcs
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param xdata_col An unquoted column name for concentration measurements
#' @param ydata_col An unquoted column name for deltaQTc measurements
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param nbins Integer number of bins to break independent variable into - OR - a user specified vector for non-uniform binning
#' @param type Algorithm for quantile. Default (2), is SAS quantile algorithm
#'
#' @return a tibble of conc, deltaQTC quantiles
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' compute_quantiles_obs_df(data_proc, CONC, deltaQTCF)
compute_quantiles_obs_df <- function(
  data,
  xdata_col,
  ydata_col,
  conf_int = 0.90,
  nbins = 10,
  type = 2
) {
  checkmate::assertDataFrame(data)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)
  checkmate::assertNumeric(nbins)
  checkmate::assertIntegerish(type, lower = 1, upper = 9)

  lower_p <- 1 - (1 + conf_int) / 2
  upper_p <- (1 + conf_int) / 2

  xdata <- rlang::enquo(xdata_col)
  ydata <- rlang::enquo(ydata_col)

  required_cols <- unlist(lapply(c(xdata, ydata), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  input_df <- tibble::tibble(
    xdata = data %>% dplyr::pull(!!xdata),
    ydata = data %>% dplyr::pull(!!ydata)
  )

  if (any(is.na(input_df$xdata))) {
    warning(
      "Your X data contains NA and is removed in calculations of this function"
    )
    input_df <- input_df %>%
      dplyr::filter(!is.na(.data$xdata))
  }
  if (any(is.na(input_df$ydata))) {
    warning(
      "Your Y data contains NA and is removed in calculations of this function"
    )
    input_df <- input_df %>%
      dplyr::filter(!is.na(.data$ydata))
  }

  if (length(nbins) == 1) {
    quantiles <- stats::quantile(
      input_df$xdata,
      probs = seq(0, 1, length.out = nbins + 1),
      type = type
    )

    # Check if quantiles is correct length
    if (quantiles %>% unique() %>% length() < nbins + 1) {
      q_unique <- unique(quantiles)

      repeated_idx <- which(duplicated(quantiles))
      repeated_val <- quantiles[repeated_idx[1]]

      warning(
        paste0(
          "Your xdata quantiles had duplicates. Filtering for x values > ",
          repeated_val
        )
      )
      quantiles_fixed <- input_df %>%
        dplyr::filter(.data$xdata > repeated_val) %>%
        dplyr::pull(.data$xdata) %>%
        stats::quantile(probs = seq(0, 1, length.out = nbins), type = type)

      q_combined <- c(q_unique[q_unique <= repeated_val], quantiles_fixed)
      q_combined <- unique(q_combined)
      if (q_combined %>% unique() %>% length() < nbins + 1) {
        stop(
          "Duplicates still present. Please manually supply nbins with desired cut points"
        )
      }
      quantiles <- q_combined
    }
  } else {
    quantiles <- nbins # use the vector supplied by user.
  }

  obs_par <- input_df %>%
    dplyr::mutate(
      decile = .data$xdata %>% cut(breaks = quantiles, include.lowest = TRUE)
    ) %>%
    dplyr::group_by(.data$decile) %>%
    dplyr::summarize(
      xdata = stats::median(.data$xdata, na.rm = TRUE),
      mean_dv = mean(.data$ydata, na.rm = TRUE),
      sd = stats::sd(.data$ydata, na.rm = TRUE),
      n = sum(!is.na(.data$ydata)),
      se = .data$sd / sqrt(.data$n),
      ci_low = .data$mean_dv -
        stats::qt((1 + conf_int) / 2, df = .data$n - 1) * .data$se,
      ci_high = .data$mean_dv +
        stats::qt((1 + conf_int) / 2, df = .data$n - 1) * .data$se,
      med_y = stats::median(.data$ydata, na.rm = TRUE),
      low_p_y = stats::quantile(.data$ydata, probs = lower_p, type = type),
      high_p_y = stats::quantile(.data$ydata, probs = upper_p, type = type)
    ) %>%
    dplyr::ungroup()
  return(obs_par)
}


#' Simulates a dataset used to fit the model.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param xdata_col An unquoted column name for xdata
#' @param sim_num An optional simulation number
#'
#' @return a dataframe of simulation results
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#' fit <- nlme::lme(
#'   fixed = deltaQTCF ~ 1 + CONC,
#'   random = ~ 1 | ID,
#'   data = data_proc
#' )
#'
#' compute_dataset_simulation(data_proc, fit, CONC)
compute_dataset_simulation <- function(data, fit, xdata_col, sim_num = 0) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assertIntegerish(sim_num)

  xdata <- rlang::enquo(xdata_col)

  required_cols <- unlist(lapply(c(xdata), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  #Copy fit to overwrite parameters generated with MASS
  dummy_fit_var <- fit

  sampled_parameters <- MASS::mvrnorm(
    n = 1,
    mu = nlme::fixef(dummy_fit_var),
    Sigma = stats::vcov(dummy_fit_var)
  )

  dummy_fit_var$coefficients$fixed <- sampled_parameters
  #because we overwrote parameters we want population pred
  fitted_values <- stats::predict(dummy_fit_var, level = 0)
  residuals <- stats::residuals(fit) #use a random sampling from eps (residuals)

  qtc_pred <- tibble::tibble(
    pred = fitted_values + sample(residuals, replace = TRUE),
    xdata = data[[rlang::quo_name(xdata)]],
    sim_num = sim_num
  )
  return(qtc_pred)
}


#' Wrapper for calling compute_dataset_simulation nruns time and computing summary statsitics of the simulations
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param xdata_col An unquoted column name for xdata
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param nruns Integer number of simulations to run
#' @param nbins Integer number of bins to break independent variable into - OR - a user specified vector for non-uniform binning
#' @param type Algorithm for quantile. Default (2), is SAS quantile algorithm
#'
#' @return a tibble of summary statistics of nruns worth of dataset simulations for a VPC.
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' fit <- nlme::lme(
#'   fixed = deltaQTCF ~ 1 + CONC,
#'   random = ~ 1 | ID,
#'   data = data_proc,
#'   method = "REML",
#'   na.action = "na.exclude"
#' )
#'
#' compute_summary_statistics_of_simulations(
#'   data = data_proc,
#'   fit = fit,
#'   xdata_col = CONC,
#'   conf_int = 0.9,
#'   nruns = 50,
#'   nbins = 10,
#'   type = 2)
compute_summary_statistics_of_simulations <- function(
  data,
  fit,
  xdata_col,
  conf_int,
  nruns,
  nbins,
  type = 2
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)
  checkmate::assertIntegerish(nruns)
  checkmate::assertNumeric(nbins)
  checkmate::assertIntegerish(type, lower = 1, upper = 9)

  xdata <- rlang::enquo(xdata_col)

  required_cols <- unlist(lapply(c(xdata), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  lower_p <- 1 - (1 + conf_int) / 2
  upper_p <- (1 + conf_int) / 2

  sim_list <- lapply(1:nruns, function(x) {
    compute_dataset_simulation(data, fit, !!xdata, sim_num = x)
  })
  combined_sim <- do.call(rbind, sim_list)

  if (length(nbins) == 1) {
    quantiles <- stats::quantile(
      combined_sim$xdata,
      probs = seq(0, 1, 1 / nbins),
      type = type
    ) %>%
      unique()
  } else {
    quantiles <- nbins # use the vector supplied by user.
  }

  stat_cs <- combined_sim %>%
    dplyr::mutate(
      decile = .data$xdata %>% cut(quantiles, include.lowest = TRUE)
    ) %>%
    dplyr::group_by(.data$decile, .data$sim_num) %>%
    dplyr::summarize(
      med_xdata = stats::median(.data$xdata),
      med_pred = stats::median(.data$pred),
      low_pred = stats::quantile(.data$pred, probs = 0.05), #These are not supposed to change with conf_int, but fixed for VPC
      high_pred = stats::quantile(.data$pred, probs = 0.95), #These are not supposed to change with conf_int, but fixed for VPC
      .groups = "keep"
    ) %>%
    dplyr::group_by(.data$med_xdata) %>%
    dplyr::mutate(
      mean_med_pred = mean(.data$med_pred),
      low_med_pred = stats::quantile(
        .data$med_pred,
        probs = lower_p,
        type = type
      ),
      high_med_pred = stats::quantile(
        .data$med_pred,
        probs = upper_p,
        type = type
      ),
      mean_low_pred = mean(.data$low_pred),
      low_low_pred = stats::quantile(
        .data$low_pred,
        probs = lower_p,
        type = type
      ),
      high_low_pred = stats::quantile(
        .data$low_pred,
        probs = upper_p,
        type = type
      ),
      mean_high_pred = mean(.data$high_pred),
      low_high_pred = stats::quantile(
        .data$high_pred,
        probs = lower_p,
        type = type
      ),
      high_high_pred = stats::quantile(
        .data$high_pred,
        probs = upper_p,
        type = type
      )
    ) %>%
    tibble::as_tibble()

  return(stat_cs)
}

#' Computes the concentration needed for a upper conf_int prediction
#' of the threshold value.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param conc_col_name String of concentration (independent variable) column name
#' @param trt_col_name String of treatment group column name
#' @param treatment_group String of treatment group to make prediction for
#' @param threshold Value used as upper CI prediction, default = 10
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @returns list of the two potential solutions.
#' @export
#'
#' @examples
#' mod <- fit_prespecified_model(
#'   cqtkit_data_verapamil %>% preprocess(),
#'   deltaQTCF,
#'   ID,
#'   CONC,
#'   deltaQTCFBL,
#'   TRTG,
#'   TAFD,
#'   "REML",
#'   remove_conc_iiv = TRUE
#' )
#' compute_conc_for_upper_pred(
#'   cqtkit_data_verapamil %>% preprocess(),
#'   mod,
#'   "CONC",
#'   "TRTG",
#'   "Verapamil HCL"
#' )
compute_conc_for_upper_pred <- function(
  data,
  fit,
  conc_col_name,
  trt_col_name,
  treatment_group,
  threshold = 10,
  conf_int = 0.9
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)

  #Pull out conc and trtg estimates for predictions
  theta_1 <- fit$coefficients$fixed[[conc_col_name]]
  theta_3 <- fit$coefficients$fixed[[paste0(trt_col_name, treatment_group)]]

  # Get degrees of freedom - need to be more sure this is always correct?
  df <- stats::coef(summary(fit))[1, 3]
  v <- stats::vcov(fit) %>% as.data.frame()

  # Pull of var and cov for conc/trtg params
  var_theta_1 <- v[conc_col_name, conc_col_name]
  var_theta_3 <- v[
    paste0(trt_col_name, treatment_group),
    paste0(trt_col_name, treatment_group)
  ]
  cov_theta_1_3 <- v[paste0(trt_col_name, treatment_group), conc_col_name]

  # compute t value with correct p based on conf_int
  t <- stats::qt(1 - (1 - conf_int) / 2, df)

  # define polynomial coefficients
  a <- theta_1^2 - var_theta_1 * t^2
  b <- 2 * theta_1 * (theta_3 - threshold) - 2 * cov_theta_1_3 * t^2
  c <- (threshold - theta_3)^2 - t^2 * var_theta_3

  quad_form(a, b, c)
}

#' Predicts dQTC over range of concentration values with contrast.
#' To help keep the predictions quick, the concentration values to predict at are
#' done at on order of magnitude of concentration values, if max(conc) = 7340, then by = 100
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param conc_col An unquoted column name for concentration measurements used to fit the model
#' @param treatment_predictors List of a values for contrast. conc will update
#' @param control_predictors List of b values for contrast
#' @param cmaxes Vector of Cmax for each dose
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @return A data frame that contains median concentration,
#'  lower and upper bounds CI.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' fit <- nlme::lme(
#'    fixed = deltaQTCF ~ 1 + CONC,
#'    random = ~ 1 | ID,
#'    method = "REML",
#'    data = data_proc
#' )
#'
#' compute_exposure_predictions(
#'   data_proc,
#'   fit,
#'   CONC,
#'   list(CONC = 10)
#' )
compute_exposure_predictions <- function(
  data,
  fit,
  conc_col,
  treatment_predictors,
  control_predictors = NULL,
  cmaxes = NULL,
  conf_int = 0.90
) {
  checkmate::assertDataFrame(data)
  checkmate::assert_class(fit, "lme")
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)
  checkmate::assertNumeric(cmaxes, null.ok = TRUE)

  conc <- rlang::enquo(conc_col)

  required_cols <- unlist(lapply(c(conc), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  conc_data <- data %>% dplyr::pull(!!conc)

  # need to better understand lme variance matrix stuff for this.
  if (typeof(fit$apVar) == "character") {
    if (fit$apVar == "Non-positive definite approximate variance-covariance") {
      stop(
        "Fit has issues, try a new model fit with method = REML or without IIV on concentration slope."
      )
    }
  }

  by <- 10**(floor(log10(max(conc_data))) - 1)
  # having min(conc_data) actually breaks things if
  # it's say -0.001 because then when I round
  # tpx_cmax later it can"t find it here...
  prediction_df <- tibble::tibble(
    conc = seq(0, max(conc_data), by = by)
  )

  if (!(min(conc_data) %in% prediction_df$conc)) {
    new_conc <- c(prediction_df$conc, min(conc_data))
    prediction_df <- tibble::tibble(conc = sort(new_conc))
  }
  if (!(max(conc_data) %in% prediction_df$conc)) {
    new_conc <- c(prediction_df$conc, max(conc_data))
    prediction_df <- tibble::tibble(conc = sort(new_conc))
  }

  if (!rlang::is_empty(cmaxes)) {
    new_conc <- c(prediction_df$conc, cmaxes)
    prediction_df <- tibble::tibble(conc = sort(new_conc))
  }

  ########## SHOULD THIS BE ITS OWN FUNCTION? ############
  get_pred <- lapply(seq_len(nrow(prediction_df)), function(x) {
    current_treatment_predictors <- treatment_predictors
    current_treatment_predictors[[rlang::as_name(conc)]] <- prediction_df$conc[[
      x
    ]]

    args_list <- list(
      fit,
      a = current_treatment_predictors,
      conf.int = conf_int
    )
    if (!is.null(control_predictors)) {
      args_list$b <- control_predictors
    }

    contrast_df <- do.call(contrast::contrast, args_list)

    pred <- cbind(
      conc = prediction_df$conc[[x]],
      pred = contrast_df$Contrast,
      lower = contrast_df$Lower,
      upper = contrast_df$Upper
    ) %>%
      tibble::as_tibble()
    return(pred)
  })
  #########################################################
  qtc_pred <- do.call(rbind, get_pred)

  return(qtc_pred)
}

#' Compute contrast observations for prediction plots
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param dv_col An unquoted column name for dQTC measurements
#' @param id_col An unquoted column name for subject ID, used when control predictors is provided to compute delta delta dv
#' @param ntime_col An unquoted column name for Nominal time data, used when control predictors is provided to compute delta delta dv
#' @param trt_col An unquoted column name for Treatment group data, used when control predictors is provided to compute delta delta dv
#' @param treatment_predictors A list for predictions with model. Should contain a value for each predictor in the model.
#' @param control_predictors An optional list for contrast predictions
#' @param contrast_method A string specifying contrast method: "matched" for individual ID+time matching (crossover studies), "group" for group-wise subtraction (parallel studies)
#'
#' @return a tibble with columns: group, conc, dv
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' # Simple case: no control group
#' obs_data <- compute_contrast_observations(
#'   data_proc,
#'   CONC,
#'   deltaQTCF,
#'   treatment_predictors = list(TRTG = "Verapamil HCL")
#' )
#' obs_data
#'
#' # Matched contrast (crossover study)
#' contrast_data <- compute_contrast_observations(
#'   data_proc,
#'   CONC,
#'   deltaQTCF,
#'   ID,
#'   NTLD,
#'   TRTG,
#'   treatment_predictors = list(TRTG = "Verapamil HCL"),
#'   control_predictors = list(TRTG = "Placebo"),
#'   contrast_method = "matched"
#' )
#' contrast_data
compute_contrast_observations <- function(
  data,
  conc_col,
  dv_col,
  id_col = NULL,
  ntime_col = NULL,
  trt_col = NULL,
  treatment_predictors,
  control_predictors = NULL,
  contrast_method = c("matched", "group")
) {
  conc <- rlang::enquo(conc_col)
  dv <- rlang::enquo(dv_col)
  id <- rlang::enquo(id_col)
  ntime <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  if (!is.null(control_predictors)) {
    contrast_method <- match.arg(contrast_method)

    if (contrast_method == "matched") {
      required_quos <- list(id_col = id, trt_col = trt, ntime_col = ntime)
      missing_args <- names(required_quos)[purrr::map_lgl(
        required_quos,
        rlang::quo_is_null
      )]

      if (length(missing_args) > 0) {
        stop(
          "When using contrast_method = 'matched', you must also supply: ",
          paste(missing_args, collapse = ", ")
        )
      }
    } else if (contrast_method == "group") {
      required_quos <- list(trt_col = trt, ntime_col = ntime)
      missing_args <- names(required_quos)[purrr::map_lgl(
        required_quos,
        rlang::quo_is_null
      )]

      if (length(missing_args) > 0) {
        stop(
          "When using contrast_method = 'group', you must also supply: ",
          paste(missing_args, collapse = ", ")
        )
      }
    }
  }

  if (is.null(control_predictors)) {
    # Simple case: no control group subtraction
    if (rlang::quo_is_null(trt)) {
      # No treatment column provided, use default grouping
      observed_df <- tibble::tibble(
        group = "Observations",
        conc = data %>% dplyr::pull(!!conc),
        dv = data %>% dplyr::pull(!!dv)
      )
    } else {
      # Use treatment column for grouping
      observed_df <- tibble::tibble(
        group = data %>% dplyr::pull(!!trt),
        conc = data %>% dplyr::pull(!!conc),
        dv = data %>% dplyr::pull(!!dv)
      )
    }
  } else {
    # Control group subtraction case
    trt_str <- rlang::as_name(trt)
    treatment_value <- treatment_predictors[[trt_str]]
    control_value <- control_predictors[[trt_str]]

    if (contrast_method == "matched") {
      # Individual ID+time matching (crossover studies)
      treatment_df <- data %>%
        dplyr::filter(!!rlang::sym(trt_str) == !!treatment_value) %>%
        dplyr::select(!!id, !!ntime, !!conc, !!trt, treatment_dv = !!dv)

      control_df <- data %>%
        dplyr::filter(!!rlang::sym(trt_str) == !!control_value) %>%
        dplyr::select(!!id, !!ntime, control_dv = !!dv)

      observed_df <- treatment_df %>%
        dplyr::left_join(
          control_df,
          by = c(rlang::as_name(id), rlang::as_name(ntime))
        ) %>%
        dplyr::mutate(dv = .data$treatment_dv - .data$control_dv) %>%
        dplyr::transmute(
          group = !!trt,
          conc = !!conc,
          dv
        )

      if (any(is.na(observed_df$dv))) {
        warning("Observed data contained NA and are removed in plot")
        observed_df <- observed_df %>% dplyr::filter(!is.na(dv))
      }
    } else if (contrast_method == "group") {
      # Group-wise subtraction (parallel studies)
      control_means <- data %>%
        dplyr::filter(!!rlang::sym(trt_str) == !!control_value) %>%
        dplyr::group_by(!!ntime) %>%
        dplyr::summarise(
          control_mean_dv = mean(!!dv, na.rm = TRUE),
          .groups = "drop"
        )

      observed_df <- data %>%
        dplyr::filter(!!rlang::sym(trt_str) == !!treatment_value) %>%
        dplyr::left_join(control_means, by = rlang::as_name(ntime)) %>%
        dplyr::mutate(dv = !!dv - .data$control_mean_dv) %>%
        dplyr::transmute(
          group = !!trt,
          conc = !!conc,
          dv
        )

      if (any(is.na(observed_df$dv))) {
        warning(
          "Control group means contained NA, resulting in NA observations that are removed"
        )
        observed_df <- observed_df %>% dplyr::filter(!is.na(dv))
      }
    }
  }

  return(observed_df)
}
