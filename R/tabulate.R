#' Creates a gt table of study summary for number of subjects in each grouping
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param trt_col An unquoted column name for treatment group
#' @param id_col An unquoted column name for subject ID
#' @param group_col An unquoted column name for additional grouping variable
#' @param protocol_number A string of protocol number
#' @param title A string for table title
#' @param study_status A string for study status
#' @param grouping_col_name A string for naming grouping column (default: "Dose Regimen")
#' @param n_sub_col_name A string for n_sub column (default: "N")
#' @param ... Optional additional args to gt::tab_options
#'
#' @return a gt table
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' tabulate_study_summary(data_proc, TRTG, ID,
#' protocol_number = "A2AI201",
#' title = "C-QT Analysis Study",
#' study_status = "Completed")
tabulate_study_summary <- function(
  data,
  trt_col,
  id_col,
  group_col = NULL,
  protocol_number,
  title,
  study_status,
  grouping_col_name = "Dose Regimen",
  n_sub_col_name = "N",
  ...
) {
  checkmate::assertDataFrame(data)

  trt <- rlang::enquo(trt_col)
  group <- rlang::enquo(group_col)
  id <- rlang::enquo(id_col)

  required_cols <- unlist(lapply(c(trt, id, group), name_quo_if_not_null)) #helper.R
  checkmate::assertNames(names(data), must.include = required_cols)

  study_sum <- compute_study_summary(data, !!trt, !!id, !!group)
  t <- study_sum %>%
    gt::gt() %>%
    gt::cols_label(
      grouping = grouping_col_name,
      n_sub = n_sub_col_name
    ) %>%
    gt::tab_header(
      title = gt::md(title),
      subtitle = paste(sprintf("Protocol Number: %s", protocol_number))
    ) %>%
    gt::tab_source_note(
      source_note = sprintf("Study Status: %s", study_status)
    )

  args <- rlang::list2(...)
  tab_option_args <- args[names(args) %in% names(formals(gt::tab_options))]
  tab_option_args$data <- t

  t <- do.call(gt::tab_options, tab_option_args)

  return(t)
}

#' Converts pk_parameters df into gt table for printing
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param id_col An unquoted column name for subject ID
#' @param dose_col An unquoted column name for dose measurements
#' @param conc_col An unquoted column name for drug concentration measurements
#' @param ntime_col An unquoted column name for nominal time since dose
#' @param group_col An unquoted column name for additional grouping column
#' @param decimals Number of decimals to format the table to (default: 2, format for N is 0 decimals)
#' @param title Optional title for the table, it will be wrapped in gt::md()
#' @param ... Optional additional arguments to gt::tab_options
#'
#' @importFrom rlang .data
#'
#' @return a gt table
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' tabulate_pk_parameters(data_proc %>% dplyr::filter(DOSE != 0), ID, DOSE, CONC, NTLD)
tabulate_pk_parameters <- function(
  data,
  id_col,
  dose_col,
  conc_col,
  ntime_col,
  group_col = NULL,
  decimals = 2,
  title = NULL,
  ...
) {
  checkmate::assertDataFrame(data)

  id <- rlang::enquo(id_col)
  dose <- rlang::enquo(dose_col)
  conc <- rlang::enquo(conc_col)
  time <- rlang::enquo(ntime_col)
  group <- rlang::enquo(group_col)

  pk_params_df <- compute_pk_parameters(
    data = data,
    id_col = !!id,
    dose_col = !!dose,
    conc_col = !!conc,
    ntime_col = !!time,
    group_col = !!group
  )

  pk_params_table <- pk_params_df %>%
    gt::gt() %>%
    gt::cols_merge(
      columns = c(.data$Tmax_median, .data$Tmax_min, .data$Tmax_max),
      pattern = "{1} ({2}, {3})"
    ) %>%
    gt::cols_merge(
      columns = c(.data$Cmax_gm, .data$Cmax_cv),
      pattern = "{1} ({2})"
    ) %>%
    gt::cols_merge(
      columns = c(.data$Cmax_median, .data$Cmax_min, .data$Cmax_max),
      pattern = "{1} ({2}, {3})"
    ) %>%
    gt::cols_label(
      group = "Dose Group",
      Tmax_median = "Tmax Median (min, max)",
      Cmax_gm = "Cmax geometric mean (%CV)",
      Cmax_median = "Cmax Median (min, max)"
    ) %>%
    gt::fmt_number(decimals = decimals) %>%
    gt::fmt_number(
      decimals = 0,
      columns = .data$N
    )

  if (!is.null(title)) {
    pk_params_table <- pk_params_table %>%
      gt::tab_header(
        title = gt::md(title)
      )
  }

  args <- rlang::list2(...)
  tab_option_args <- args[names(args) %in% names(formals(gt::tab_options))]
  tab_option_args$data <- pk_params_table

  pk_params_table <- do.call(gt::tab_options, tab_option_args)

  return(pk_params_table)
}

#' Generates table of model parameter esitmates and statistics
#'
#' @param fit An nlme::lme model object from model fitting
#' @param trt_col_name A string of column name of trt used in model fitting
#' @param tafd_col_name A string of column name of tafd used in model fitting
#' @param id_col_name A string of column name of id used in model fitting for random effects
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param decimals Number of decimals to format table to, default is 2
#' @param show_standard_error Boolean that displays standard error of fixed effects estimates in table
#' @param scientific Boolean for converting numbers to scientific notation if less than decimals
#' @param title Optional string for adding tab_header. It will be wrapped in gt::md()
#' @param ... Optional additional arguments for gt::tab_options
#'
#' @importFrom rlang .data
#'
#' @return a gt table
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
#'
#' tabulate_model_fit_parameters(fit, "TRTG", "TAFD", "ID")
tabulate_model_fit_parameters <- function(
  fit,
  trt_col_name = "TRTG",
  tafd_col_name = "TAFD",
  id_col_name = "ID",
  conf_int = 0.95,
  decimals = 2,
  show_standard_error = FALSE,
  scientific = TRUE,
  title = NULL,
  ...
) {
  checkmate::assert_class(fit, "lme")
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1)

  fit_result_df <- compute_model_fit_parameters(
    fit,
    trt_col_name,
    tafd_col_name,
    id_col_name,
    conf_int
  )

  if (!show_standard_error) {
    fit_result_df <- fit_result_df %>%
      dplyr::select(-"Std.Error")
  }

  fit_result_table <- fit_result_df %>%
    dplyr::select(-"DF", -"t-value") %>%
    gt::gt() %>%
    gt::cols_merge(
      columns = c(.data$Value, .data$CIl, .data$CIu),
      pattern = "{1} [{2}, {3}]"
    ) %>%
    gt::cols_label(
      Value = paste0("Estimate [", conf_int * 100, "% CI]")
    )
  if (show_standard_error) {
    fit_result_table <- fit_result_table %>%
      gt::cols_label(
        Std.Error = "Standard Error"
      )
  }

  fit_result_table <- fit_result_table %>%
    gt::fmt_number(decimals = decimals) %>%
    gt::sub_missing()

  if (!is.null(title)) {
    fit_result_table <- fit_result_table %>%
      gt::tab_header(
        title = gt::md(title)
      )
  }
  if (scientific) {
    fit_result_table <- fit_result_table %>%
      gt::fmt_scientific(
        columns = gt::everything(),
        decimals = decimals
      )
  }

  args <- rlang::list2(...)
  tab_option_args <- args[names(args) %in% names(formals(gt::tab_options))]
  tab_option_args$data <- fit_result_table

  fit_result_table <- do.call(gt::tab_options, tab_option_args)

  return(fit_result_table)
}

#' Generates a gt table of summary of QTc, dQTc and ddQTc over time stratified by dose
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param ntime_col An unquoted column name for nominal time data
#' @param dose_col An unquoted column name for dose data
#' @param ecg_param_col An unquoted column name for QTc measurements
#' @param delta_ecg_param_col An unquoted column name for deltaQTc measurements
#' @param ecg_param_name A string of the ecg parameter being summarized, e.g. QTc or HR
#' @param unit A string of the unit for ecg_parameter
#' @param group_col An unquoted column name for additional grouping column
#' @param reference_dose A reference dose value for comparison calculations
#' @param ecg_param_conf_int Confidence interval for QTc summary stats default 95%
#' @param delta_ecg_param_conf_int Confidence interval for dQTc summary stats default 90%
#' @param decimals Number of decimals to fmt the table to default is 2, N column is 0
#' @param row_group_label Optional label for the dose/dose + group column
#' @param title Optional title for the table, it will be wrapped in gt::md()
#' @param ... Optional arguments for gt::tab_options
#'
#' @return a gt table of the QTc/deltaQTc/delta delta QTc summary
#' @export
#'
#' @examples
#' data_proc <- cqtkit_data_verapamil %>% preprocess()
#' tabulate_ecg_param_summary(
#'  data_proc,
#'  NTLD,
#'  DOSEF,
#'  QTCF,
#'  deltaQTCF,
#'  "QTc",
#'  "ms",
#'  reference_dose = "0 mg")
tabulate_ecg_param_summary <- function(
  data,
  ntime_col,
  dose_col,
  ecg_param_col,
  delta_ecg_param_col,
  ecg_param_name,
  unit = "",
  group_col = NULL,
  reference_dose = NULL,
  ecg_param_conf_int = 0.95,
  delta_ecg_param_conf_int = 0.9,
  decimals = 2,
  row_group_label = NULL,
  title = NULL,
  ...
) {
  checkmate::assertDataFrame(data)

  ntld <- rlang::enquo(ntime_col)
  dose <- rlang::enquo(dose_col)
  ecg <- rlang::enquo(ecg_param_col)
  deltaecg <- rlang::enquo(delta_ecg_param_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(lapply(
    c(ntld, dose, ecg, deltaecg, group),
    name_quo_if_not_null
  )) #helper.R
  checkmate::assertNames(names(data), must.include = required_cols)
  args <- rlang::list2(...)
  tab_option_args <- args[names(args) %in% names(formals(gt::tab_options))]

  summary <- compute_ecg_param_summary(
    data,
    !!ntld,
    !!dose,
    !!ecg,
    !!deltaecg,
    group_col = !!group,
    reference_dose = reference_dose,
    conf_int = ecg_param_conf_int
  )
  if (delta_ecg_param_conf_int != ecg_param_conf_int) {
    decg_summary <- compute_ecg_param_summary(
      data,
      !!ntld,
      !!dose,
      !!ecg,
      !!deltaecg,
      !!group,
      reference_dose,
      delta_ecg_param_conf_int
    )
    summary$decg_low <- decg_summary$decg_low
    summary$decg_high <- decg_summary$decg_high
    if (!is.null(reference_dose)) {
      summary$ddecg_low <- decg_summary$ddecg_low
      summary$ddecg_high <- decg_summary$ddecg_high
    }
  }

  if (!is.null(reference_dose)) {
    s_proc <- summary %>%
      dplyr::mutate(
        mean_ddecg = dplyr::if_else(
          .data$dose == reference_dose,
          NA,
          .data$mean_ddecg
        ),
        ddecg_low = dplyr::if_else(
          .data$dose == reference_dose,
          NA,
          .data$ddecg_low
        ),
        ddecg_high = dplyr::if_else(
          .data$dose == reference_dose,
          NA,
          .data$ddecg_high
        )
      )
  } else {
    s_proc <- summary
  }
  s_proc <- s_proc %>% dplyr::select(-"dose")

  s_gt <- s_proc %>%
    dplyr::group_by(group) %>%
    gt::gt() %>%
    gt::tab_options(
      row_group.as_column = TRUE
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        v_align = "middle",
        align = "center"
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_spanner(
      label = paste0("Observed ", ecg_param_name, " (", unit, ")"),
      columns = c(.data$mean_ecg, .data$ecg_low, .data$ecg_high)
    ) %>%
    gt::tab_spanner(
      label = "Baseline Corrected",
      columns = c(.data$mean_decg, .data$decg_low, .data$decg_high),
      level = 2
    ) %>%
    gt::tab_spanner(
      label = paste0(ecg_param_name, " (", unit, ")"),
      columns = c(.data$mean_decg, .data$decg_low, .data$decg_high),
      level = 1
    ) %>%
    gt::cols_merge(
      columns = c(.data$ecg_low, .data$ecg_high),
      pattern = "[{1}, {2}]"
    ) %>%
    gt::cols_merge(
      columns = c(.data$decg_low, .data$decg_high),
      pattern = "[{1}, {2}]"
    ) %>%
    gt::cols_label(
      time = "Time (hr)",
      n = "N",
      mean_ecg = "Mean",
      mean_decg = "Mean",
      ecg_low = paste0(round(ecg_param_conf_int * 100), "% CI"),
      decg_low = paste0(round(delta_ecg_param_conf_int * 100), "% CI"),
    )
  if (!is.null(reference_dose)) {
    s_gt <- s_gt %>%
      gt::tab_spanner(
        label = "Baseline and Placebo",
        columns = c(.data$mean_ddecg, .data$ddecg_low, .data$ddecg_high),
        level = 2
      ) %>%
      gt::tab_spanner(
        label = paste0("Corrected ", ecg_param_name, " (", unit, ")"),
        columns = c(.data$mean_ddecg, .data$ddecg_low, .data$ddecg_high),
        level = 1
      ) %>%
      gt::cols_merge(
        columns = c(.data$ddecg_low, .data$ddecg_high),
        pattern = "[{1}, {2}]"
      ) %>%
      gt::cols_label(
        mean_ddecg = "Mean",
        ddecg_low = paste0(round(delta_ecg_param_conf_int * 100), "% CI")
      )
  }

  if (!is.null(row_group_label)) {
    s_gt <- s_gt %>%
      gt::tab_stubhead(label = row_group_label)
  }

  if (!is.null(title)) {
    s_gt <- s_gt %>%
      gt::tab_header(
        title = gt::md(title)
      )
  }

  s_gt <- s_gt %>%
    gt::fmt_number(decimals = decimals) %>%
    gt::fmt_number(
      decimals = 0,
      columns = .data$n
    ) %>%
    gt::sub_missing()

  tab_option_args$data <- s_gt
  s_gt <- do.call(gt::tab_options, tab_option_args)

  return(s_gt)
}

#' Tabulates number of high QTc/deltaQTc observations.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param qtc_col An unquoted column name for QTc data
#' @param deltaqtc_col An unquoted column name for deltaQTC data
#' @param group_col An optional unquoted column name of grouping column
#' @param group_label An optional label to use for group column
#' @param title Optional string to give the table a title, wrapped in gt::md()
#' @param ... Optional additional args to gt::tab_options
#'
#' @return a gt table
#' @export
#'
#' @examples
#' data_proc <- preprocess(cqtkit_data_verapamil)
#'
#' tabulate_high_qtc_sub(data_proc, QTCF, deltaQTCF)
tabulate_high_qtc_sub <- function(
  data,
  qtc_col,
  deltaqtc_col,
  group_col = NULL,
  group_label = NULL,
  title = NULL,
  ...
) {
  checkmate::assertDataFrame(data)

  qtc <- rlang::enquo(qtc_col)
  deltaqtc <- rlang::enquo(deltaqtc_col)
  group <- rlang::enquo(group_col)

  required_cols <- unlist(lapply(c(qtc, deltaqtc, group), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  n_gt <- compute_high_qtc_sub(data, !!qtc, !!deltaqtc, !!group)

  # Convert to long format manually to prevent inclusion of tidyr just for pivot_longer.
  t <- n_gt %>%
    gt::gt() %>%
    gt::cols_label(
      n_QTc_gt_450 = gt::md("QTc > 450"),
      n_QTc_gt_480 = gt::md("QTc > 480"),
      n_QTc_gt_500 = gt::md("QTc > 500"),
      n_dQTc_gt_30 = gt::md("&Delta; QTc > 30"),
      n_dQTc_gt_60 = gt::md("&Delta; QTc > 60")
    )

  if (!is.null(title)) {
    t <- t %>%
      gt::tab_header(
        title = gt::md(title)
      )
  }

  if (is.null(group_label)) {
    if (!rlang::quo_is_null(group)) {
      group_label <- name_quo_if_not_null(group)
    } else {
      group_label = ""
    }
  }
  t <- t %>%
    gt::cols_label(group = group_label)

  args <- rlang::list2(...)
  tab_option_args <- args[names(args) %in% names(formals(gt::tab_options))]
  tab_option_args$data <- t

  t <- do.call(gt::tab_options, tab_option_args)

  return(t)
}

#' Tablulates exposure predictions at therapeutic and supratherapuetic Cmax.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param fit An nlme::lme model object from model fitting
#' @param conc_col An unquoted column name for concentration measurements used to fit the model
#' @param treatment_predictors A list of a values for contrast. conc will update
#' @param control_predictors A list of b values for contrast
#' @param doses A vector of doses to show prediction at
#' @param cmaxes A vector of Cmax for each dose
#' @param qtc_label A label for QTc column name, default QTc (ms)
#' @param conc_units Units for concentration default ng/mL
#' @param conf_int Numeric confidence interval level (default: 0.9)
#' @param decimals Number of decimals to format numbers to. default is 2
#' @param scientific Boolean for converting to scientific notation
#' @param title Optional string for table title. Wrapped in gt::md()
#' @param ... Optional additional args to gt::tab_options
#'
#' @importFrom rlang .data
#'
#' @return a gt table of predicted dQTc/ddQTc values and their CI at tpx and or
#' stpx dose/Cmax
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
#' pk_df <- compute_pk_parameters(
#'   data_proc %>% dplyr::filter(DOSE != 0),
#'   ID,
#'   DOSEF,
#'   CONC,
#'   NTLD
#' )
#'
#' tabulate_exposure_predictions(
#'   data_proc,
#'   fit,
#'   CONC,
#'   list(
#'     CONC = 10,
#'     deltaQTCFBL = 0,
#'     TRTG = "Verapamil HCL",
#'     TAFD = "2 HR"),
#'   list(
#'     CONC = 0,
#'     deltaQTCFBL = 0,
#'     TRTG = "Placebo",
#'     TAFD = "2 HR"),
#'   doses <- c(120),
#'   cmaxes <- c(pk_df[[1, "Cmax_gm"]])
#' )
#'
tabulate_exposure_predictions <- function(
  data,
  fit,
  conc_col,
  treatment_predictors,
  control_predictors = NULL,
  doses,
  cmaxes,
  qtc_label = "QTc (ms)",
  conc_units = "ng/mL",
  conf_int = 0.90,
  decimals = 2,
  scientific = TRUE,
  title = NULL,
  ...
) {
  checkmate::assertDataFrame(data)
  checkmate::assert(checkmate::check_class(fit, "lme"))
  checkmate::assertList(treatment_predictors)
  checkmate::assertList(control_predictors, null.ok = TRUE)
  checkmate::assert_vector(doses, null.ok = FALSE)
  checkmate::assertNumeric(cmaxes)
  checkmate::assertNumeric(conf_int, lower = 0, upper = 1, len = 1)

  if (length(doses) != length(cmaxes)) {
    stop(
      "Please provide equal number of Doses to cmaxes, you can pad with 'N/A', if needed."
    )
  }

  conc <- rlang::enquo(conc_col)

  required_cols <- unlist(lapply(c(conc), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  if (typeof(fit$apVar) == "character") {
    if (fit$apVar == "Non-positive definite approximate variance-covariance") {
      stop("Fit has issues, try a new model")
    }
  }

  if (!is.null(control_predictors)) {
    label <- paste("&Delta; &Delta;", qtc_label)
  } else {
    label <- paste("&Delta;", qtc_label)
  }

  pred_df <- compute_exposure_predictions(
    data,
    fit,
    !!conc,
    treatment_predictors,
    control_predictors,
    cmaxes,
    conf_int
  ) %>%
    dplyr::filter(conc %in% cmaxes) %>%
    dplyr::mutate(conc = factor(conc, levels = cmaxes)) %>%
    dplyr::arrange(conc) %>%
    dplyr::mutate(conc = as.numeric(as.character(conc))) %>%
    dplyr::mutate(Dose = doses) %>%
    dplyr::rename(Cmax = .data$conc) %>%
    dplyr::select("Dose", "Cmax", "pred", "lower", "upper") %>%
    gt::gt() %>%
    gt::cols_merge(
      columns = c(.data$lower, .data$upper),
      pattern = "[{1}, {2}]"
    ) %>%
    gt::cols_label(
      Dose = "Dose",
      Cmax = paste0("Cmax (", conc_units, ")"),
      pred = gt::md(label),
      lower = paste0("[", round(conf_int * 100), "% CI]")
    ) %>%
    gt::fmt_number(columns = dplyr::everything(), decimals = decimals)

  if (!is.null(title)) {
    pred_df <- pred_df %>%
      gt::tab_header(
        title = gt::md(title)
      )
  }

  if (scientific) {
    pred_df <- pred_df %>%
      gt::fmt_scientific(
        decimals = decimals
      )
  }

  args <- rlang::list2(...)
  tab_option_args <- args[names(args) %in% names(formals(gt::tab_options))]
  tab_option_args$data <- pred_df

  pred_df <- do.call(gt::tab_options, tab_option_args)

  return(pred_df)
}
