#' Add a column unless it is already present
#'
#' Existing values are never overwritten, so a column the caller supplied takes
#' precedence over the one cqtkit would derive.
#'
#' @param data A data frame.
#' @param colname Name of the column to add.
#' @param expr Expression evaluated in `data` to produce the column.
#' @return `data`, with the column added if it was absent.
#' @noRd
add_column_if_absent <- function(data, colname, expr) {
  if (!(colname %in% names(data))) {
    data <- dplyr::mutate(data, !!colname := !!rlang::enquo(expr))
  }
  data
}

#' Compute QTcB QTcF
#'
#' @param data A data frame containing QT, RR, QTBL, RRBL
#' @param qt_col An unquoted column name for QT measurements
#' @param qtbl_col An unquoted column name for baseline QT measurements
#' @param rr_col An unquoted column name for RR measurements
#' @param rrbl_col An unquoted column name for baseline RR measurements
#'
#' @importFrom rlang .data
#'
#' @return A data frame with QTCF, QTCB, QTCFBL, and QTCBBL columns added
#' @export
#'
#' @examples
#' compute_qtcb_qtcf(
#'   dplyr::select(cqtkit_data_bl_verapamil, -QTCB, -QTCF),
#'   qtbl_col = NULL,
#'   rrbl_col = NULL
#' )
compute_qtcb_qtcf <- function(
  data,
  qt_col = QT,
  qtbl_col = QTBL,
  rr_col = RR,
  rrbl_col = RRBL
) {
  checkmate::assertDataFrame(data)

  qt <- rlang::enquo(qt_col)
  qtbl <- rlang::enquo(qtbl_col)
  rr <- rlang::enquo(rr_col)
  rrbl <- rlang::enquo(rrbl_col)

  required_cols <- unlist(lapply(c(qt, qtbl, rr, rrbl), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  if (!rlang::quo_is_null(qt) && !rlang::quo_is_null(rr)) {
    data <- add_column_if_absent(data, "QTCB", !!qt / ((!!rr / 1000)^(1 / 2)))
    data <- add_column_if_absent(data, "QTCF", !!qt / ((!!rr / 1000)^(1 / 3)))
  }

  if (!rlang::quo_is_null(qtbl) && !rlang::quo_is_null(rrbl)) {
    data <- add_column_if_absent(
      data,
      "QTCBBL",
      !!qtbl / ((!!rrbl / 1000)^(1 / 2))
    )
    data <- add_column_if_absent(
      data,
      "QTCFBL",
      !!qtbl / ((!!rrbl / 1000)^(1 / 3))
    )
  }

  return(data)
}

#' Compute heart rate (HR) from RR
#'
#' Adds an `HR` column to `data` computed as `60000 / RR` (ms -> bpm), and
#' optionally an `HRBL` column computed from a baseline RR column.
#'
#' Intended for ECG data that has not yet been averaged across replicates.
#' On replicate-averaged data the result is `60000` divided by a mean RR,
#' which is not the mean of the replicate heart rates.
#'
#' @param data A data frame containing an RR column.
#' @param rr_col Unquoted column name for RR measurements. Default `RR`.
#'   Pass `NULL` to skip the HR computation.
#' @param rrbl_col Unquoted column name for baseline RR measurements.
#'   Default `RRBL`. Pass `NULL` to skip the HRBL computation.
#'
#' @return `data` with `HR` and/or `HRBL` columns added. Existing columns
#'   of those names are left unchanged.
#' @export
#'
#' @examples
#' compute_hr(
#'   dplyr::select(cqtkit_data_bl_verapamil, -HR),
#'   rrbl_col = NULL
#' )
compute_hr <- function(
  data,
  rr_col = RR,
  rrbl_col = RRBL
) {
  checkmate::assertDataFrame(data)

  rr <- rlang::enquo(rr_col)
  rrbl <- rlang::enquo(rrbl_col)

  required_cols <- unlist(lapply(c(rr, rrbl), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  if (!rlang::quo_is_null(rr)) {
    data <- add_column_if_absent(data, "HR", 60000 / !!rr)
  }

  if (!rlang::quo_is_null(rrbl)) {
    data <- add_column_if_absent(data, "HRBL", 60000 / !!rrbl)
  }

  return(data)
}

#' Compute Deltas
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param qt_col An unquoted column name for QT measurements
#' @param qtbl_col An unquoted column name for baseline QT measurements
#' @param rr_col An unquoted column name for RR measurements
#' @param rrbl_col An unquoted column name for baseline RR measurements
#' @param hr_col An unquoted column name for HR measurements, HR by default
#' @param hrbl_col An unquoted column name for baseline HR measurements, HRBL by default
#' @param qtcf_col An unquoted column name for QTCF measurements, QTCF by default
#' @param qtcfbl_col An unquoted column name for baseline QTCF measurements, QTCFBL by default
#' @param qtcb_col An unquoted column name for QTCB measurements, QTCB by default
#' @param qtcbbl_col An unquoted column name for baseline QTCB measurements, QTCBBL by default
#'
#' @return A data frame with deltaPARAM columns added
#' @export
#'
#' @examples
#' compute_deltas(
#'   dplyr::select(cqtkit_data_verapamil, -dplyr::starts_with("delta"))
#' )
compute_deltas <- function(
  data,
  qt_col = QT,
  qtbl_col = QTBL,
  rr_col = RR,
  rrbl_col = RRBL,
  hr_col = HR,
  hrbl_col = HRBL,
  qtcf_col = QTCF,
  qtcfbl_col = QTCFBL,
  qtcb_col = QTCB,
  qtcbbl_col = QTCBBL
) {
  checkmate::assertDataFrame(data)

  qt <- rlang::enquo(qt_col)
  qtbl <- rlang::enquo(qtbl_col)
  qtcb <- rlang::enquo(qtcb_col)
  qtcbbl <- rlang::enquo(qtcbbl_col)
  qtcf <- rlang::enquo(qtcf_col)
  qtcfbl <- rlang::enquo(qtcfbl_col)
  rr <- rlang::enquo(rr_col)
  rrbl <- rlang::enquo(rrbl_col)
  hr <- rlang::enquo(hr_col)
  hrbl <- rlang::enquo(hrbl_col)

  required_cols <- unlist(lapply(
    c(qt, qtbl, qtcf, qtcfbl, qtcb, qtcbbl, hr, hrbl, rr, rrbl),
    name_quo_if_not_null
  ))

  checkmate::assertNames(names(data), must.include = required_cols)

  if (!rlang::quo_is_null(qtcb) && !rlang::quo_is_null(qtcbbl)) {
    data <- add_column_if_absent(data, "deltaQTCB", !!qtcb - !!qtcbbl)
  }

  if (!rlang::quo_is_null(qtcf) && !rlang::quo_is_null(qtcfbl)) {
    data <- add_column_if_absent(data, "deltaQTCF", !!qtcf - !!qtcfbl)
  }

  if (!rlang::quo_is_null(rr) && !rlang::quo_is_null(rrbl)) {
    data <- add_column_if_absent(data, "deltaRR", !!rr - !!rrbl)
  }

  if (!rlang::quo_is_null(hr) && !rlang::quo_is_null(hrbl)) {
    data <- add_column_if_absent(data, "deltaHR", !!hr - !!hrbl)
  }

  if (!rlang::quo_is_null(qt) && !rlang::quo_is_null(qtbl)) {
    data <- add_column_if_absent(data, "deltaQT", !!qt - !!qtbl)
  }

  return(data)
}

#' Subtract a population baseline mean from a per-subject baseline
#'
#' @param data A data frame containing a C-QT analysis dataset.
#' @param bl Quosure for the per-subject baseline column.
#' @param blm Quosure for the population baseline mean column, which must
#'   already be on `data`.
#' @param delta_name Name of the delta column to add.
#'
#' @return `data` with the delta column added.
#' @noRd
compute_delta_blm <- function(data, bl, blm, delta_name) {
  checkmate::assertDataFrame(data)

  blm_name <- rlang::as_name(blm)
  if (!blm_name %in% names(data)) {
    stop(
      "`",
      blm_name,
      "` not found in `data`. ",
      blm_deprecate_details(),
      " See `vignette(\"data-assembly\")`. To average the baseline values ",
      "already on `data` instead, set ",
      "`options(cqtkit.override_preprocessing_error = TRUE)`.",
      call. = FALSE
    )
  }

  required_cols <- unlist(lapply(c(bl, blm), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  add_column_if_absent(data, delta_name, !!bl - !!blm)
}


#' Add one baseline-from-mean delta, honouring the preprocessing override
#'
#' Skips the delta when either column is `NULL`. Under
#' `preprocessing_override()` the population mean is averaged from the
#' baseline values on `data`, otherwise `blm` must already be on `data`.
#'
#' @param data A data frame containing a C-QT analysis dataset.
#' @param bl Quosure for the per-subject baseline column.
#' @param blm Quosure for the population baseline mean column.
#' @param delta_name Name of the delta column to add.
#' @param id Quosure for the subject identifier column, override path only.
#' @param deduplicate Whether to deduplicate before averaging, override path
#'   only.
#'
#' @return `data`, with the delta column added unless it was skipped.
#' @noRd
add_blm_delta <- function(data, bl, blm, delta_name, id, deduplicate) {
  if (rlang::quo_is_null(bl) || rlang::quo_is_null(blm)) {
    return(data)
  }

  if (preprocessing_override()) {
    return(legacy_delta_blm(
      data = data,
      id = id,
      bl = bl,
      deduplicate = deduplicate,
      blm_name = rlang::as_name(blm),
      delta_name = delta_name
    ))
  }

  compute_delta_blm(data, bl = bl, blm = blm, delta_name = delta_name)
}

#' Compute Delta HR Baseline Mean
#'
#' Adds a `deltaHRBL` column computed as `HRBL - HRBLM`. `HRBLM`, the
#' population mean baseline heart rate, must already be on `data`; add it
#' with [compute_blm()].
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param id_col `r lifecycle::badge("deprecated")` An unquoted column name
#'   for subject ID. Only used when
#'   `options(cqtkit.override_preprocessing_error = TRUE)`.
#' @param hrbl_col An unquoted column name for baseline HR measurements, default is HRBL
#' @param deduplicate `r lifecycle::badge("deprecated")` Logical, whether to
#'   remove duplicate baseline values before averaging. Only used when
#'   `options(cqtkit.override_preprocessing_error = TRUE)`.
#' @param hrblm_col An unquoted column name for the population mean baseline
#'   HR, default is HRBLM
#'
#' @return A data frame with deltaHRBL column added
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' compute_delta_hrblm(dplyr::select(cqtkit_data_verapamil, -deltaHRBL))
compute_delta_hrblm <- function(
  data,
  id_col = lifecycle::deprecated(),
  hrbl_col = HRBL,
  deduplicate = lifecycle::deprecated(),
  hrblm_col = HRBLM
) {
  hrbl <- rlang::enquo(hrbl_col)
  hrblm <- rlang::enquo(hrblm_col)

  legacy <- deprecated_blm_delta(
    data,
    id = rlang::enquo(id_col),
    bl = hrbl,
    deduplicate = deduplicate,
    blm = hrblm,
    delta_name = "deltaHRBL",
    fn = "compute_delta_hrblm",
    user_env = rlang::caller_env()
  )
  if (!is.null(legacy)) {
    return(legacy)
  }

  compute_delta_blm(data, bl = hrbl, blm = hrblm, delta_name = "deltaHRBL")
}

#' Compute Delta QTcB Baseline Mean
#'
#' Adds a `deltaQTCBBL` column computed as `QTCBBL - QTCBBLM`. `QTCBBLM`,
#' the population mean baseline Bazett-corrected QT, must already be on
#' `data`; add it with [compute_blm()].
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param id_col `r lifecycle::badge("deprecated")` An unquoted column name
#'   for subject ID. Only used when
#'   `options(cqtkit.override_preprocessing_error = TRUE)`.
#' @param qtcbbl_col An unquoted column name for baseline QTCB measurements, default is QTCBBL
#' @param deduplicate `r lifecycle::badge("deprecated")` Logical, whether to
#'   remove duplicate baseline values before averaging. Only used when
#'   `options(cqtkit.override_preprocessing_error = TRUE)`.
#' @param qtcbblm_col An unquoted column name for the population mean
#'   baseline QTCB, default is QTCBBLM
#'
#' @return A data frame with deltaQTCBBL column added
#' @export
#' @importFrom rlang .data
#' @examples
#' compute_delta_qtcbblm(dplyr::select(cqtkit_data_verapamil, -deltaQTCBBL))
compute_delta_qtcbblm <- function(
  data,
  id_col = lifecycle::deprecated(),
  qtcbbl_col = QTCBBL,
  deduplicate = lifecycle::deprecated(),
  qtcbblm_col = QTCBBLM
) {
  qtcbbl <- rlang::enquo(qtcbbl_col)
  qtcbblm <- rlang::enquo(qtcbblm_col)

  legacy <- deprecated_blm_delta(
    data,
    id = rlang::enquo(id_col),
    bl = qtcbbl,
    deduplicate = deduplicate,
    blm = qtcbblm,
    delta_name = "deltaQTCBBL",
    fn = "compute_delta_qtcbblm",
    user_env = rlang::caller_env()
  )
  if (!is.null(legacy)) {
    return(legacy)
  }

  compute_delta_blm(
    data,
    bl = qtcbbl,
    blm = qtcbblm,
    delta_name = "deltaQTCBBL"
  )
}

#' Compute Delta QTcF Baseline Mean
#'
#' Adds a `deltaQTCFBL` column computed as `QTCFBL - QTCFBLM`. `QTCFBLM`,
#' the population mean baseline Fridericia-corrected QT, must already be on
#' `data`; add it with [compute_blm()].
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param id_col `r lifecycle::badge("deprecated")` An unquoted column name
#'   for subject ID. Only used when
#'   `options(cqtkit.override_preprocessing_error = TRUE)`.
#' @param qtcfbl_col An unquoted column name for baseline QTCF measurements, default is QTCFBL
#' @param deduplicate `r lifecycle::badge("deprecated")` Logical, whether to
#'   remove duplicate baseline values before averaging. Only used when
#'   `options(cqtkit.override_preprocessing_error = TRUE)`.
#' @param qtcfblm_col An unquoted column name for the population mean
#'   baseline QTCF, default is QTCFBLM
#'
#' @return A data frame with deltaQTCFBL column added
#' @export
#' @importFrom rlang .data
#' @examples
#' compute_delta_qtcfblm(dplyr::select(cqtkit_data_verapamil, -deltaQTCFBL))
compute_delta_qtcfblm <- function(
  data,
  id_col = lifecycle::deprecated(),
  qtcfbl_col = QTCFBL,
  deduplicate = lifecycle::deprecated(),
  qtcfblm_col = QTCFBLM
) {
  qtcfbl <- rlang::enquo(qtcfbl_col)
  qtcfblm <- rlang::enquo(qtcfblm_col)

  legacy <- deprecated_blm_delta(
    data,
    id = rlang::enquo(id_col),
    bl = qtcfbl,
    deduplicate = deduplicate,
    blm = qtcfblm,
    delta_name = "deltaQTCFBL",
    fn = "compute_delta_qtcfblm",
    user_env = rlang::caller_env()
  )
  if (!is.null(legacy)) {
    return(legacy)
  }

  compute_delta_blm(
    data,
    bl = qtcfbl,
    blm = qtcfblm,
    delta_name = "deltaQTCFBL"
  )
}

#' Preprocess
#'
#' Computes deltaQTcB, deltaQTcF, deltaHR, deltaQTcB Baseline Mean,
#' deltaQTcF Baseline Mean, deltaHR Baseline Mean.
#'
#' @details
#' `QTCB`, `QTCF`, `QTCBBL` and `QTCFBL` must already be on `data`, and the
#' population means `HRBLM`, `QTCBBLM` and `QTCFBLM` must be too. Corrected
#' QT is nonlinear in RR, so deriving it from replicate-averaged `QT` and
#' `RR` does not give the mean of the replicate corrected values. Compute
#' the corrections per replicate with [compute_qtcb_qtcf()] before
#' averaging, and add the population means with [compute_blm()]. See
#' `vignette("data-assembly")`.
#'
#' Setting `options(cqtkit.override_preprocessing_error = TRUE)` restores
#' the pre-1.2.0 behaviour, where `preprocess()` derived those columns from
#' whatever `data` carried. It warns naming each value derived that way.
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param qt_col An unquoted column name for QT measurements
#' @param qtbl_col An unquoted column name for baseline QT measurements
#' @param rr_col An unquoted column name for RR measurements
#' @param rrbl_col An unquoted column name for baseline RR measurements
#' @param hr_col An unquoted column name for HR measurements, HR by default
#' @param hrbl_col An unquoted column name for baseline HR measurements, HRBL by default
#' @param qtcf_col An unquoted column name for QTCF measurements, QTCF by default
#' @param qtcfbl_col An unquoted column name for baseline QTCF measurements, QTCFBL by default
#' @param qtcb_col An unquoted column name for QTCB measurements, QTCB by default
#' @param qtcbbl_col An unquoted column name for baseline QTCB measurements, QTCBBL by default
#' @param id_col `r lifecycle::badge("deprecated")` An unquoted column name
#'   for subject ID. Only used when
#'   `options(cqtkit.override_preprocessing_error = TRUE)`.
#' @param deduplicate `r lifecycle::badge("deprecated")` Logical, whether to
#'   remove duplicate baseline values before averaging. Only used when
#'   `options(cqtkit.override_preprocessing_error = TRUE)`.
#' @param hrblm_col An unquoted column name for the population mean baseline
#'   HR, HRBLM by default. `NULL` skips the HR baseline delta.
#' @param qtcbblm_col An unquoted column name for the population mean baseline
#'   QTCB, QTCBBLM by default. `NULL` skips the QTCB baseline delta.
#' @param qtcfblm_col An unquoted column name for the population mean baseline
#'   QTCF, QTCFBLM by default. `NULL` skips the QTCF baseline delta.
#'
#' @return A data frame with all delta columns computed from baseline
#' @export
#'
#' @examples
#' preprocess(
#'   dplyr::select(cqtkit_data_verapamil, -dplyr::starts_with("delta"))
#' )
preprocess <- function(
  data,
  qt_col = QT,
  qtbl_col = QTBL,
  rr_col = RR,
  rrbl_col = RRBL,
  hr_col = HR,
  hrbl_col = HRBL,
  qtcf_col = QTCF,
  qtcfbl_col = QTCFBL,
  qtcb_col = QTCB,
  qtcbbl_col = QTCBBL,
  id_col = lifecycle::deprecated(),
  deduplicate = lifecycle::deprecated(),
  hrblm_col = HRBLM,
  qtcbblm_col = QTCBBLM,
  qtcfblm_col = QTCFBLM
) {
  checkmate::assertDataFrame(data)

  qt <- rlang::enquo(qt_col)
  qtbl <- rlang::enquo(qtbl_col)
  rr <- rlang::enquo(rr_col)
  rrbl <- rlang::enquo(rrbl_col)
  hr <- rlang::enquo(hr_col)
  hrbl <- rlang::enquo(hrbl_col)
  qtcf <- rlang::enquo(qtcf_col)
  qtcfbl <- rlang::enquo(qtcfbl_col)
  qtcb <- rlang::enquo(qtcb_col)
  qtcbbl <- rlang::enquo(qtcbbl_col)
  hrblm <- rlang::enquo(hrblm_col)
  qtcbblm <- rlang::enquo(qtcbblm_col)
  qtcfblm <- rlang::enquo(qtcfblm_col)
  deprecated_args <- warn_deprecated_blm_args(
    rlang::enquo(id_col),
    deduplicate,
    "preprocess",
    user_env = rlang::caller_env()
  )

  qtc_missing <- setdiff(
    unlist(lapply(c(qtcb, qtcbbl, qtcf, qtcfbl), name_quo_if_not_null)),
    names(data)
  )
  if (length(qtc_missing) > 0 && !preprocessing_override()) {
    stop(
      paste0("`", qtc_missing, "`", collapse = ", "),
      " not found in `data`. Corrected QT is nonlinear in RR, so deriving it ",
      "from replicate-averaged QT and RR does not give the mean of the ",
      "replicate corrected values. Compute the corrections per replicate with ",
      "`compute_qtcb_qtcf()` before averaging, see `vignette(\"data-assembly\")`. ",
      "To derive them here anyway, set ",
      "`options(cqtkit.override_preprocessing_error = TRUE)`.",
      call. = FALSE
    )
  }

  if (preprocessing_override()) {
    blm_missing <- setdiff(
      unlist(lapply(c(hrblm, qtcbblm, qtcfblm), name_quo_if_not_null)),
      names(data)
    )
    if (length(c(qtc_missing, blm_missing)) > 0) {
      warn_preprocessing_override(c(qtc_missing, blm_missing))
    }

    data <- compute_qtcb_qtcf(
      data,
      qt_col = !!qt,
      qtbl_col = !!qtbl,
      rr_col = !!rr,
      rrbl_col = !!rrbl
    )
  }

  blm_specs <- list(
    list(bl = hrbl, blm = hrblm, delta_name = "deltaHRBL"),
    list(bl = qtcbbl, blm = qtcbblm, delta_name = "deltaQTCBBL"),
    list(bl = qtcfbl, blm = qtcfblm, delta_name = "deltaQTCFBL")
  )

  for (spec in blm_specs) {
    data <- add_blm_delta(
      data,
      bl = spec$bl,
      blm = spec$blm,
      delta_name = spec$delta_name,
      id = deprecated_args$id,
      deduplicate = deprecated_args$deduplicate
    )
  }

  data |>
    compute_deltas(
      !!qt,
      !!qtbl,
      !!rr,
      !!rrbl,
      !!hr,
      !!hrbl,
      !!qtcf,
      !!qtcfbl,
      !!qtcb,
      !!qtcbbl
    )
}

#' Compute a population mean baseline value (BLM)
#'
#' Assembly-time helper for building a C-QT analysis dataset. Averages
#' `ecg_param_col` within each `group_col` group, averages those group means, and
#' attaches the result to `data` as a constant column. Not intended for use on
#' already-assembled datasets, use [compute_delta_hrblm()],
#' [compute_delta_qtcbblm()] or [compute_delta_qtcfblm()] for that.
#'
#' `ecg_param_col` must already be on `bl_data`, computed per replicate.
#' Corrected QT is nonlinear in RR, so a correction applied after averaging does not give
#' the mean of the replicate corrected values. Add the columns with
#' [compute_hr()] and [compute_qtcb_qtcf()] first. Groups with any `NA` in
#' `ecg_param_col` are dropped before averaging, and a warning is emitted.
#'
#' @param data A data frame to attach the population mean baseline column to
#'   (e.g., the assembled analysis dataset).
#' @param bl_data A data frame of baseline ECG measurements to compute the
#'   population mean baseline from (typically raw data filtered to baseline
#'   rows), one row per replicate.
#' @param group_col Grouping columns, as bare symbols or strings, supplied
#'   as `c(col1, col2, ...)` or a single value.
#' @param ecg_param_col Unquoted name of the column in `bl_data` to average.
#' @param blm_col_name Name of the column added to `data`.
#'
#' @return `data` with the population mean baseline column added (a constant
#'   scalar repeated across all rows). If that column already exists on `data`,
#'   returns `data` unchanged.
#' @export
#'
#' @examples
#' bl_data <- compute_qtcb_qtcf(
#'   cqtkit_data_bl_verapamil,
#'   qtbl_col = NULL,
#'   rrbl_col = NULL
#' )
#'
#' compute_blm(
#'   dplyr::select(cqtkit_data_verapamil, -QTCFBLM),
#'   bl_data,
#'   group_col = c(ID, TRTG),
#'   ecg_param_col = QTCF,
#'   blm_col_name = "QTCFBLM"
#' )
compute_blm <- function(data, bl_data, group_col, ecg_param_col, blm_col_name) {
  checkmate::assertDataFrame(data)
  checkmate::assertDataFrame(bl_data)
  checkmate::assertString(blm_col_name)

  ecg_param <- rlang::as_name(rlang::enquo(ecg_param_col))
  group_cols <- names_from_quo(rlang::enquo(group_col))
  checkmate::assertNames(
    names(bl_data),
    must.include = c(ecg_param, group_cols)
  )

  if (blm_col_name %in% names(data)) {
    return(data)
  }

  group_syms <- rlang::syms(group_cols)

  bl_flagged <- bl_data |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::mutate(.any_na = any(is.na(.data[[ecg_param]]))) |>
    dplyr::ungroup()

  dropped <- bl_flagged |>
    dplyr::filter(.data$.any_na) |>
    dplyr::distinct(!!!group_syms)

  if (nrow(dropped) > 0) {
    warning(
      blm_col_name,
      ": dropped ",
      nrow(dropped),
      " group(s) with NA values in ",
      ecg_param,
      call. = FALSE
    )
  }

  per_group <- bl_flagged |>
    dplyr::filter(!.data$.any_na) |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::summarise(
      .grp_mean = mean(.data[[ecg_param]]),
      .groups = "drop"
    )

  add_column_if_absent(data, blm_col_name, mean(per_group$.grp_mean))
}
