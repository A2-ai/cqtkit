#' Fits QT(c) data to linear mixed effects model with fixed effects of intercept and
#' RR slope, with random effects on intercept and slope.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param qt_col An unquoted column name for QT measurements
#' @param rr_col An unquoted column name for RR measurements
#' @param id_col An unquoted column name for subject ID
#' @param method Method for nlme::lme fitting (ML or REML)
#' @param remove_rr_iiv Boolean for removing IIV on slope
#'
#' @return nlme::lme model
#' @export
#'
#' @examples
#' bl <- compute_qtcb_qtcf(cqtkit_data_bl_verapamil, qtbl_col = NULL, rrbl_col = NULL)
#'
#' qt_mod <- fit_qtc_linear_model(bl, QT, RR, ID)
#' qtcb_mod <- fit_qtc_linear_model(bl, QTCB, RR, ID)
#' qtcf_mod <- fit_qtc_linear_model(bl, QTCF, RR, ID)
fit_qtc_linear_model <- function(
  data,
  qt_col,
  rr_col,
  id_col,
  method = 'REML',
  remove_rr_iiv = FALSE
) {
  checkmate::assertDataFrame(data)

  qt <- rlang::enquo(qt_col)
  rr <- rlang::enquo(rr_col)
  id <- rlang::enquo(id_col)

  required_cols <- unlist(lapply(c(qt, rr, id), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  # Ensure required columns are present
  vars <- c(qt, rr, id)
  required_cols <- unlist(lapply(vars, name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  new_data <- data

  fixed_formula <- paste0(rlang::quo_name(qt), " ~ 1 + ", rlang::quo_name(rr))

  fixed_formula <- stats::as.formula(fixed_formula)

  lme_params <- list(
    fixed = fixed_formula,
    data = new_data,
    method = method,
    na.action = 'na.exclude'
  )

  if (!rlang::quo_is_null(id)) {
    if (remove_rr_iiv) {
      random_formula <- stats::as.formula(paste0(
        "~ 1",
        " | ",
        rlang::quo_name(id)
      ))
    } else {
      random_formula <- stats::as.formula(paste0(
        "~ 1 + ",
        rlang::quo_name(rr),
        " | ",
        rlang::quo_name(id)
      ))
    }
    lme_params$random <- random_formula
  }

  mod <- do.call(
    "lme",
    lme_params
  )

  if (
    all(mod$apVar == "Non-positive definite approximate variance-covariance")
  ) {
    message(
      "variance-covariance issues detected, try running again with method = 'REML'\nIf issues persist, try removing IIV."
    )
  }

  return(mod)
}

#' generates nlme::lme model either prespecified or without TRT and TIME.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param dv_col An unquoted column name for dependent variable measurements
#' @param id_col An unquoted column name for ID data
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param delta_bl_col An unquoted column name for delta_bl values, e.g. deltaQTCFBL, deltaHRBL
#' @param trt_col An unquoted column name for treatment group
#' @param tafd_col An unquoted column name for time measurements
#' @param method Method for nlme::lme fitting (ML or REML)
#' @param remove_conc_iiv Boolean for removing IIV on concentration slope parameter
#'
#' @return an nlme::lme model fit to the data
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' fit_prespecified_model(data_proc, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD)
fit_prespecified_model <- function(
  data,
  dv_col,
  id_col,
  conc_col,
  delta_bl_col,
  trt_col = NULL,
  tafd_col = NULL,
  method = 'ML',
  remove_conc_iiv = FALSE
) {
  checkmate::assertDataFrame(data)

  dv <- rlang::enquo(dv_col)
  conc <- rlang::enquo(conc_col)
  deltaqtcbl <- rlang::enquo(delta_bl_col)
  trt <- rlang::enquo(trt_col)
  tafd <- rlang::enquo(tafd_col)
  id <- rlang::enquo(id_col)

  required_cols <- unlist(lapply(
    c(dv, conc, deltaqtcbl, trt, tafd, id),
    name_quo_if_not_null
  ))
  checkmate::assertNames(names(data), must.include = required_cols)

  # Ensure required columns are present
  vars <- c(dv, conc, deltaqtcbl, trt, tafd, id)
  required_cols <- unlist(lapply(vars, name_quo_if_not_null)) #from helper.R
  checkmate::assertNames(names(data), must.include = required_cols)

  #copy data to new variable to overwrite TAFD column if present in model.
  new_data <- data

  fixed_formula <- paste0(
    rlang::quo_name(dv),
    " ~ 1 + ",
    rlang::quo_name(conc),
    " + ",
    rlang::quo_name(deltaqtcbl)
  )
  if (!rlang::quo_is_null(trt)) {
    fixed_formula <- paste0(fixed_formula, " + ", rlang::quo_name(trt))
  }

  if (!rlang::quo_is_null(tafd)) {
    new_data <- data
    new_data[[rlang::quo_name(
      tafd
    )]] <- forcats::fct_inorder(data[[rlang::quo_name(tafd)]])
    fixed_formula <- paste0(fixed_formula, " + ", rlang::quo_name(tafd))
  }

  fixed_formula <- stats::as.formula(fixed_formula)
  if (remove_conc_iiv) {
    random_formula <- stats::as.formula(paste0(
      "~ 1",
      " | ",
      rlang::quo_name(id)
    ))
  } else {
    random_formula <- stats::as.formula(paste0(
      "~ 1 + ",
      rlang::quo_name(conc),
      " | ",
      rlang::quo_name(id)
    ))
  }

  # if I make a call directly to nlme::lme, the resulting model has fixef(mod) = fixed_formula, wrapping the call in
  # do.call fixed the errors this caused when trying to do predict_with_x plot functions.
  mod <- do.call(
    "lme",
    list(
      fixed = fixed_formula,
      random = random_formula,
      data = new_data,
      method = method,
      na.action = 'na.exclude'
    )
  )
  if (
    all(mod$apVar == "Non-positive definite approximate variance-covariance")
  ) {
    message(
      "variance-covariance issues detected, try running again with method = 'REML'\nIf issues persist, try removing IIV."
    )
  }

  return(mod)
}

#' Converts tTable of summary(model_fit) to tibble and adds CIs.
#'
#' @param fit An nlme::lme model object from model fitting
#' @param trt_col_name String of column name of trt used in model fitting
#' @param tafd_col_name String of column name of tafd used in model fitting
#' @param id_col_name String of column name of the id used in model fitting for random effects
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @return a tibble of model_fit parameters
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
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
#' compute_model_fit_parameters(fit)
compute_model_fit_parameters <- function(
  fit,
  trt_col_name = "TRTG",
  tafd_col_name = "TAFD",
  id_col_name = "ID",
  conf_int = 0.95
) {
  checkmate::assert_class(fit, 'lme')
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)

  sum <- summary(fit)$tTable
  new_names <- gsub(paste0("^", trt_col_name), "", rownames(sum))
  new_names <- gsub(paste0("^", tafd_col_name), "", new_names)
  new_names <- gsub("[\\(\\)]", "", new_names)
  rownames(sum) <- new_names

  sum <- sum %>%
    as.data.frame() %>% #The reason this is as.data.frame instead of tibble is to convert the rownames to a column, then convert to tibble at the end.
    tibble::rownames_to_column(var = 'Parameters') %>%
    dplyr::mutate(
      'CIl' = .data$Value -
        stats::qt((1 + conf_int) / 2, .data$DF) * .data$Std.Error,
      'CIu' = .data$Value +
        stats::qt((1 + conf_int) / 2, .data$DF) * .data$Std.Error
    ) %>%
    tibble::as_tibble()

  # add residuals
  sigmav <- nlme::intervals(fit, conf_int)$sigma %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Parameters = c("Residual Error"),
      Value = .data$`est.`,
      Std.Error = NA_real_,
      DF = NA_integer_,
      `t-value` = NA_real_,
      `p-value` = NA_real_,
      CIl = .data$lower,
      CIu = .data$upper,
    ) %>%
    dplyr::select(
      "Parameters",
      "Value",
      "Std.Error",
      "DF",
      "t-value",
      "p-value",
      "CIl",
      "CIu"
    )

  # add random effects
  if (!is.null(id_col_name)) {
    intervals_result <- nlme::intervals(fit, conf_int)$reStruct[[id_col_name]]

    random_eff <- tibble::tibble(
      Parameters = gsub("sd", "IIV", rownames(intervals_result)),
      Value = intervals_result[, "est."],
      CIl = intervals_result[, "lower"],
      CIu = intervals_result[, "upper"]
    ) %>%
      dplyr::filter(startsWith(.data$Parameters, "IIV")) %>%
      dplyr::mutate(
        Parameters = gsub("\\(\\(", "\\(", .data$Parameters),
        Parameters = gsub("\\)\\)", "\\)", .data$Parameters),
        Std.Error = NA_real_,
        DF = NA_integer_,
        `t-value` = NA_real_,
        `p-value` = NA_real_
      ) %>%
      dplyr::select(
        "Parameters",
        "Value",
        "Std.Error",
        "DF",
        "t-value",
        "p-value",
        "CIl",
        "CIu"
      )

    parameters <- rbind(sum, random_eff, sigmav)
  } else {
    parameters <- rbind(sum, sigmav)
  }

  return(parameters)
}

#' computes all fitted results and residuals for GOF plots
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param dv_col An unquoted column name for observed model dependent variable
#' @param conc_col An unquoted column name for observed Concentration measurements
#' @param ntime_col An unquoted column name for time points for measurements
#' @param trt_col An unquoted column name for treatment group, default NULL
#'
#' @importFrom nlme lme
#'
#' @return a dataframe of predictions and residuals.
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
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
#'
#' compute_fit_results(data_proc, fit, deltaQTCF, CONC, NTLD)
compute_fit_results <- function(
  data,
  fit,
  dv_col,
  conc_col,
  ntime_col,
  trt_col = NULL
) {
  checkmate::assertDataFrame(data)
  checkmate::assert_class(fit, 'lme')

  dv <- rlang::enquo(dv_col)
  conc <- rlang::enquo(conc_col)
  time <- rlang::enquo(ntime_col)
  trt <- rlang::enquo(trt_col)

  required_cols <- unlist(lapply(c(dv, conc, time, trt), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  fit_results_df <- tibble::tibble(
    dv = data %>% dplyr::pull(!!dv),
    conc = data %>% dplyr::pull(!!conc),
    time = data %>% dplyr::pull(!!time),
    PRED = stats::fitted(fit, level = 0),
    IPRED = stats::fitted(fit, level = 1),
    RES = stats::residuals(fit, level = 0),
    IRES = stats::residuals(fit, level = 1),
    WRES = stats::residuals(fit, level = 0, type = 'pearson'),
    IWRES = stats::residuals(fit, level = 1, type = 'pearson')
  )
  if (!rlang::quo_is_null(trt)) {
    fit_results_df <- fit_results_df %>%
      dplyr::mutate(TRTG = data %>% dplyr::pull(!!trt))
  } else {
    fit_results_df <- fit_results_df %>%
      dplyr::mutate(TRTG = "")
  }
  return(fit_results_df)
}
