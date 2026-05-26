#' Compute QTcB QTcF
#'
#' @param data A data frame containing QT, RR, QTBL, RRBL
#' @param qt_col An unquoted column name for QT measurements
#' @param qtbl_col An unquoted column name for baseline QT measurements
#' @param rr_col An unquoted column name for RR measurements
#' @param rrbl_col An unquoted column name for baseline RR measurements
#'

#'
#' @return A data frame with QTCF, QTCB, QTCFBL, and QTCBBL columns added
#' @export
#'
#' @examples compute_qtcb_qtcf(cqtkit_data_verapamil)
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
#' @examples compute_hr(cqtkit_data_verapamil)
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
#' compute_deltas(compute_qtcb_qtcf(cqtkit_data_verapamil))
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

#' Compute deltaHRBL from HRBL and HRBLM
#'
#' Adds a `deltaHRBL` column to `data` computed as `HRBL - HRBLM`. Both
#' input columns must already exist on `data`; use [compute_hrblm()] to
#' add the `HRBLM` scalar first.
#'
#' @param data A data frame with `hrbl_col` and `hrblm_col` columns.
#' @param hrbl_col Unquoted column name for the per-subject baseline HR.
#'   Default `HRBL`.
#' @param hrblm_col Unquoted column name for the population baseline HR
#'   mean. Default `HRBLM`.
#'
#' @return `data` with a `deltaHRBL` column added. If `deltaHRBL` already
#'   exists, returns `data` unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_bl <- raw |> dplyr::filter(BASELINE == "Y")
#' analysis_data |>
#'   compute_hrblm(raw_bl, by = c(RANDID, EXTRT)) |>
#'   compute_delta_hrblm()
#' }
compute_delta_hrblm <- function(
  data,
  hrbl_col = HRBL,
  hrblm_col = HRBLM
) {
  checkmate::assertDataFrame(data)

  hrbl  <- rlang::enquo(hrbl_col)
  hrblm <- rlang::enquo(hrblm_col)

  required_cols <- unlist(lapply(c(hrbl, hrblm), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  add_column_if_absent(data, "deltaHRBL", !!hrbl - !!hrblm)
}

#' Compute deltaQTCBBL from QTCBBL and QTCBBLM
#'
#' Adds a `deltaQTCBBL` column to `data` computed as `QTCBBL - QTCBBLM`.
#' Both input columns must already exist on `data`; use
#' [compute_qtcbblm()] to add the `QTCBBLM` scalar first.
#'
#' @param data A data frame with `qtcbbl_col` and `qtcbblm_col` columns.
#' @param qtcbbl_col Unquoted column name for the per-subject baseline
#'   Bazett-corrected QT. Default `QTCBBL`.
#' @param qtcbblm_col Unquoted column name for the population baseline
#'   Bazett-corrected QT mean. Default `QTCBBLM`.
#'
#' @return `data` with a `deltaQTCBBL` column added. If `deltaQTCBBL`
#'   already exists, returns `data` unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_bl <- raw |> dplyr::filter(BASELINE == "Y")
#' analysis_data |>
#'   compute_qtcbblm(raw_bl, by = c(RANDID, EXTRT)) |>
#'   compute_delta_qtcbblm()
#' }
compute_delta_qtcbblm <- function(
  data,
  qtcbbl_col = QTCBBL,
  qtcbblm_col = QTCBBLM
) {
  checkmate::assertDataFrame(data)

  qtcbbl  <- rlang::enquo(qtcbbl_col)
  qtcbblm <- rlang::enquo(qtcbblm_col)

  required_cols <- unlist(lapply(c(qtcbbl, qtcbblm), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  add_column_if_absent(data, "deltaQTCBBL", !!qtcbbl - !!qtcbblm)
}

#' Compute deltaQTCFBL from QTCFBL and QTCFBLM
#'
#' Adds a `deltaQTCFBL` column to `data` computed as `QTCFBL - QTCFBLM`.
#' Both input columns must already exist on `data`; use
#' [compute_qtcfblm()] to add the `QTCFBLM` scalar first.
#'
#' @param data A data frame with `qtcfbl_col` and `qtcfblm_col` columns.
#' @param qtcfbl_col Unquoted column name for the per-subject baseline
#'   Fridericia-corrected QT. Default `QTCFBL`.
#' @param qtcfblm_col Unquoted column name for the population baseline
#'   Fridericia-corrected QT mean. Default `QTCFBLM`.
#'
#' @return `data` with a `deltaQTCFBL` column added. If `deltaQTCFBL`
#'   already exists, returns `data` unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_bl <- raw |> dplyr::filter(BASELINE == "Y")
#' analysis_data |>
#'   compute_qtcfblm(raw_bl, by = c(RANDID, EXTRT)) |>
#'   compute_delta_qtcfblm()
#' }
compute_delta_qtcfblm <- function(
  data,
  qtcfbl_col = QTCFBL,
  qtcfblm_col = QTCFBLM
) {
  checkmate::assertDataFrame(data)

  qtcfbl  <- rlang::enquo(qtcfbl_col)
  qtcfblm <- rlang::enquo(qtcfblm_col)

  required_cols <- unlist(lapply(c(qtcfbl, qtcfblm), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  add_column_if_absent(data, "deltaQTCFBL", !!qtcfbl - !!qtcfblm)
}


#' Preprocess a C-QT analysis dataset
#'
#' Adds the population-baseline and delta columns to an analysis-grain
#' dataset: the population baseline means (`HRBLM`/`QTCBBLM`/`QTCFBLM`), the
#' baseline-from-mean deltas (`deltaHRBL`/`deltaQTCBBL`/`deltaQTCFBL`), and
#' the row-level deltas (`deltaQTCB`/`deltaQTCF`/`deltaHR`/`deltaQT`/`deltaRR`).
#'
#' `data` must already carry the heart-rate-corrected columns
#' (`HR`/`HRBL`, `QTCB`/`QTCBBL`, `QTCF`/`QTCFBL`). Those corrections are
#' nonlinear in RR and should be computed per raw ECG replicate and then
#' averaged (see [compute_qtcb_qtcf()] and [compute_hr()] at assembly time),
#' not recomputed from already-aggregated values here.
#'
#' @param data A data frame at analysis grain carrying the corrected QT/HR
#'   columns and their baselines.
#' @param bl_data A data frame of baseline ECG measurements, used to compute
#'   the population baseline means. See [compute_hrblm()].
#' @param by Grouping columns for the population baseline means, as bare
#'   symbols supplied via `c(col1, col2, ...)`, e.g. `c(ID, TRTG)`.
#'
#' @return `data` with the population-baseline-mean and delta columns added.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_bl <- raw |> dplyr::filter(BASELINE == "Y")
#' analysis_data |> preprocess(raw_bl, by = c(ID, TRTG))
#' }
preprocess <- function(data, bl_data, by) {
  data |>
    compute_hrblm(bl_data, by = {{ by }}) |>
    compute_qtcbblm(bl_data, by = {{ by }}) |>
    compute_qtcfblm(bl_data, by = {{ by }}) |>
    compute_delta_hrblm() |>
    compute_delta_qtcbblm() |>
    compute_delta_qtcfblm() |>
    compute_deltas()
}

#' Compute population mean baseline HR (HRBLM)
#'
#' Assembly-time helper for building a C-QT analysis dataset. Computes the
#' population mean baseline heart rate from raw baseline ECG data and
#' attaches it as a constant column (`HRBLM`) on `data`. Not intended for
#' use on already-assembled datasets — use [compute_delta_hrblm()] for
#' that.
#'
#' When `by` is supplied, the scalar follows the white-paper recipe: per
#' group, average `60000 / rr_col`; then take the mean of those per-group
#' means. Groups with any `NA` in `rr_col` are dropped before averaging,
#' and a warning is emitted. When `by = NULL`, the scalar is a single row-
#' level mean of `60000 / rr_col`.
#'
#' @param data A data frame to attach the `HRBLM` column to (e.g., the
#'   assembled analysis dataset).
#' @param bl_data A data frame of baseline ECG measurements to compute
#'   `HRBLM` from (typically raw data filtered to baseline rows).
#' @param by Optional grouping columns as bare symbols, supplied as
#'   `c(col1, col2, ...)` or a single symbol. Default `NULL`.
#' @param rr_col Unquoted name of the RR column in `bl_data` (default `RR`).
#'
#' @return `data` with an `HRBLM` column added (a constant scalar repeated
#'   across all rows). If `HRBLM` already exists on `data`, returns `data`
#'   unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_bl <- raw |> dplyr::filter(BASELINE == "Y")
#' analysis_data |> compute_hrblm(raw_bl, by = c(RANDID, EXTRT))
#' }
compute_hrblm <- function(data, bl_data, by = NULL, rr_col = RR) {
  checkmate::assertDataFrame(data)
  checkmate::assertDataFrame(bl_data)

  rr <- rlang::enquo(rr_col)
  rr_name <- rlang::as_name(rr)
  by_cols <- parse_blm_by(rlang::enquo(by))

  checkmate::assertNames(names(bl_data), must.include = c(rr_name, by_cols))

  hrblm <- blm_scalar(
    bl_data   = bl_data,
    required  = rr_name,
    metric    = rlang::expr(60000 / !!rr),
    by_cols   = by_cols,
    blm_label = "HRBLM"
  )

  add_column_if_absent(data, "HRBLM", hrblm)
}


#' Compute population mean baseline Bazett-corrected QT (QTCBBLM)
#'
#' Assembly-time helper for building a C-QT analysis dataset. Computes the
#' population mean baseline Bazett-corrected QT (`QT / sqrt(RR/1000)`)
#' from raw baseline ECG data and attaches it as a constant column
#' (`QTCBBLM`) on `data`. Not intended for use on already-assembled
#' datasets — use [compute_delta_qtcbblm()] for that.
#'
#' When `by` is supplied, the scalar follows the white-paper recipe: per
#' group, average the Bazett-corrected QT; then take the mean of those
#' per-group means. Groups with any `NA` in `qt_col` or `rr_col` are
#' dropped before averaging, and a warning is emitted. When `by = NULL`,
#' the scalar is a single row-level mean.
#'
#' @param data A data frame to attach the `QTCBBLM` column to.
#' @param bl_data A data frame of baseline ECG measurements.
#' @param by Optional grouping columns as bare symbols, supplied as
#'   `c(col1, col2, ...)` or a single symbol. Default `NULL`.
#' @param qt_col Unquoted name of the QT column in `bl_data` (default `QT`).
#' @param rr_col Unquoted name of the RR column in `bl_data` (default `RR`).
#'
#' @return `data` with a `QTCBBLM` column added (a constant scalar). If
#'   `QTCBBLM` already exists on `data`, returns `data` unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_bl <- raw |> dplyr::filter(BASELINE == "Y")
#' analysis_data |> compute_qtcbblm(raw_bl, by = c(RANDID, EXTRT))
#' }
compute_qtcbblm <- function(data, bl_data, by = NULL, qt_col = QT, rr_col = RR) {
  checkmate::assertDataFrame(data)
  checkmate::assertDataFrame(bl_data)

  qt <- rlang::enquo(qt_col)
  rr <- rlang::enquo(rr_col)
  qt_name <- rlang::as_name(qt)
  rr_name <- rlang::as_name(rr)
  by_cols <- parse_blm_by(rlang::enquo(by))

  checkmate::assertNames(names(bl_data), must.include = c(qt_name, rr_name, by_cols))

  qtcbblm <- blm_scalar(
    bl_data   = bl_data,
    required  = c(qt_name, rr_name),
    metric    = rlang::expr(!!qt / sqrt(!!rr / 1000)),
    by_cols   = by_cols,
    blm_label = "QTCBBLM"
  )

  add_column_if_absent(data, "QTCBBLM", qtcbblm)
}


#' Compute population mean baseline Fridericia-corrected QT (QTCFBLM)
#'
#' Assembly-time helper for building a C-QT analysis dataset. Computes the
#' population mean baseline Fridericia-corrected QT
#' (`QT / (RR/1000)^(1/3)`) from raw baseline ECG data and attaches it as
#' a constant column (`QTCFBLM`) on `data`. Not intended for use on
#' already-assembled datasets — use [compute_delta_qtcfblm()] for that.
#'
#' When `by` is supplied, the scalar follows the white-paper recipe: per
#' group, average the Fridericia-corrected QT; then take the mean of those
#' per-group means. Groups with any `NA` in `qt_col` or `rr_col` are
#' dropped before averaging, and a warning is emitted. When `by = NULL`,
#' the scalar is a single row-level mean.
#'
#' @param data A data frame to attach the `QTCFBLM` column to.
#' @param bl_data A data frame of baseline ECG measurements.
#' @param by Optional grouping columns as bare symbols, supplied as
#'   `c(col1, col2, ...)` or a single symbol. Default `NULL`.
#' @param qt_col Unquoted name of the QT column in `bl_data` (default `QT`).
#' @param rr_col Unquoted name of the RR column in `bl_data` (default `RR`).
#'
#' @return `data` with a `QTCFBLM` column added (a constant scalar). If
#'   `QTCFBLM` already exists on `data`, returns `data` unchanged.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_bl <- raw |> dplyr::filter(BASELINE == "Y")
#' analysis_data |> compute_qtcfblm(raw_bl, by = c(RANDID, EXTRT))
#' }
compute_qtcfblm <- function(data, bl_data, by = NULL, qt_col = QT, rr_col = RR) {
  checkmate::assertDataFrame(data)
  checkmate::assertDataFrame(bl_data)

  qt <- rlang::enquo(qt_col)
  rr <- rlang::enquo(rr_col)
  qt_name <- rlang::as_name(qt)
  rr_name <- rlang::as_name(rr)
  by_cols <- parse_blm_by(rlang::enquo(by))

  checkmate::assertNames(names(bl_data), must.include = c(qt_name, rr_name, by_cols))

  qtcfblm <- blm_scalar(
    bl_data   = bl_data,
    required  = c(qt_name, rr_name),
    metric    = rlang::expr(!!qt / (!!rr / 1000)^(1 / 3)),
    by_cols   = by_cols,
    blm_label = "QTCFBLM"
  )

  add_column_if_absent(data, "QTCFBLM", qtcfblm)
}


# Internal: parse the `by` arg of compute_*blm() into a character vector of
# column names. Accepts NULL, a bare symbol, or c(sym1, sym2, ...).
parse_blm_by <- function(by_quo) {
  if (rlang::quo_is_null(by_quo)) return(character(0))
  expr <- rlang::quo_get_expr(by_quo)
  if (rlang::is_call(expr, "c")) {
    vapply(rlang::call_args(expr), rlang::as_string, character(1))
  } else if (rlang::is_symbol(expr)) {
    rlang::as_string(expr)
  } else {
    stop("`by` must be NULL, a bare symbol, or c(...) of bare symbols",
         call. = FALSE)
  }
}


# Internal: compute a population BLM scalar from bl_data.
# - metric: a quoted expression like `60000 / RR` or `QT / sqrt(RR/1000)`
# - required: character vector of column names whose NA presence determines
#   group exclusion (or row exclusion when by_cols is empty)
# - by_cols: character vector of grouping column names; empty = no grouping
# - blm_label: string for warning messages
blm_scalar <- function(bl_data, required, metric, by_cols, blm_label) {
  bl_with_metric <- dplyr::mutate(bl_data, .metric = !!metric)

  if (length(by_cols) == 0) {
    n_na <- sum(is.na(bl_with_metric$.metric))
    if (n_na > 0) {
      warning(blm_label, ": ", n_na, " NA value(s) dropped from average",
              call. = FALSE)
    }
    return(mean(bl_with_metric$.metric, na.rm = TRUE))
  }

  # Per-group then cross-group. Flag arms with any NA in required cols.
  bl_flagged <- bl_with_metric |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) |>
    dplyr::mutate(
      .any_na = any(!stats::complete.cases(
        dplyr::across(dplyr::all_of(required))
      ))
    ) |>
    dplyr::ungroup()

  dropped <- bl_flagged |>
    dplyr::filter(.data$.any_na) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(by_cols)))

  if (nrow(dropped) > 0) {
    warning(blm_label, ": dropped ", nrow(dropped),
            " group(s) with NA values in ", paste(required, collapse = ", "),
            call. = FALSE)
  }

  per_group <- bl_flagged |>
    dplyr::filter(!.data$.any_na) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) |>
    dplyr::summarise(.grp_mean = mean(.data$.metric), .groups = "drop")

  mean(per_group$.grp_mean)
}


# Helper function to add column without overwriting
add_column_if_absent <- function(data, colname, expr) {
  if (!(colname %in% names(data))) {
    data <- dplyr::mutate(data, !!colname := !!rlang::enquo(expr))
  }
  data
}
