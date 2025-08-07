#' Computes QTCF and QTCB from qt_col and rr_col and QTCFBL and QTCBBL from qtbl_col and rrbl_col
#'
#' @param data a dataframe containing QT, RR, QTBL, RRBL
#' @param qt_col an unquoted column name of QT measurements, QT by default
#' @param qtbl_col an unquoted column name of baseline QT measurements, QTBL by default
#' @param rr_col an unquoted column name of RR measurements, RR by default
#' @param rrbl_col an unquoted column name of baseline RR measurements, RRBL by default
#'
#' @importFrom rlang .data
#'
#' @return data with QTCF, QTCB, QTCFBL, and QTCBBL columns
#' @export
#'
#' @examples compute_qtcb_qtcf(data)
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

#' Computes delta variables RR, QTCF, HR, etc
#'
#' @param data input dataset for QT analysis
#' @param qt_col an unquoted column name of QT measurements, QT by default
#' @param qtbl_col an unquoted column name of baseline QT measurements, QTBL by default
#' @param rr_col an unquoted column name of RR measurements, RR by default
#' @param rrbl_col an unquoted column name of baseline RR measurements, RRBL by default
#' @param hr_col an unquoted column name of HR measurements, HR by default
#' @param hrbl_col an unquoted column name of baseline HR measurements, HRBL by default
#' @param qtcf_col an unquoted column name of QTCF measurements, QTCF by default
#' @param qtcfbl_col an unquoted column name of baseline QTCF measurements, QTCFBL by default
#' @param qtcb_col an unquoted column name of QTCB measurements, QTCB by default
#' @param qtcbbl_col an unquoted column name of baseline QTCB measurements, QTCBBL by default
#'
#' @return dataframe with deltaPARAM columns included
#' @export
#'
#' @examples
#' compute_deltas(compute_qtcb_qtcf(data))
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

#' computes delta HR BL Mean
#'
#' @param data input dataset for qtc analysis
#' @param id_col an unquoted column name of ID data
#' @param hrbl_col an unquoted column name of baseline HR measurements, default is HRBL
#' @param deduplicate boolean, whether baseline values are duplicated over rows. If true duplicates will be removed from average
#'
#' @return data frame with deltaHRBL
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' compute_delta_hrblm(data)
compute_delta_hrblm <- function(
  data,
  id_col = ID,
  hrbl_col = HRBL,
  deduplicate = TRUE
) {
  checkmate::assertDataFrame(data)

  id <- rlang::enquo(id_col)
  hrbl <- rlang::enquo(hrbl_col)

  required_cols <- unlist(lapply(c(id, hrbl), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  baseline_data <- data %>%
    dplyr::select(!!id, !!hrbl)

  if (deduplicate) {
    baseline_data <- baseline_data %>%
      dplyr::distinct()
  }

  hrbl_mean <- mean(baseline_data[[rlang::as_name(hrbl)]], na.rm = TRUE)

  data <- add_column_if_absent(data, "HRBLM", hrbl_mean)
  data <- add_column_if_absent(data, "deltaHRBL", !!hrbl - hrbl_mean)

  return(data)
}

#' Computes Baseline Mean QTCB
#'
#' @param data dataframe of input data
#' @param id_col an unquoted column name of ID data
#' @param qtcbbl_col an unquoted column name of baseline QTCB measurements, default is QTCBBL
#' @param deduplicate boolean, whether baseline values are duplicated over rows. If true duplicates will be removed from average
#'
#' @return a dataframe with deltaQTCBBL column
#' @export
#' @importFrom rlang .data
#' @examples
#' compute_delta_qtcbblm(compute_qtcb_qtcf(data))
compute_delta_qtcbblm <- function(
  data,
  id_col = ID,
  qtcbbl_col = QTCBBL,
  deduplicate = TRUE
) {
  checkmate::assertDataFrame(data)

  id <- rlang::enquo(id_col)
  qtcbbl <- rlang::enquo(qtcbbl_col)

  required_cols <- unlist(lapply(c(id, qtcbbl), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  baseline_data <- data %>%
    dplyr::select(!!id, !!qtcbbl)

  if (deduplicate) {
    baseline_data <- baseline_data %>%
      dplyr::distinct()
  }

  qtcbbl_mean <- mean(baseline_data[[rlang::as_name(qtcbbl)]], na.rm = TRUE)

  data <- add_column_if_absent(data, "QTCBBLM", qtcbbl_mean)
  data <- add_column_if_absent(data, "deltaQTCBBL", !!qtcbbl - qtcbbl_mean)

  return(data)
}

#' Computes Baseline Mean QTCF
#'
#' @param data dataframe of input data
#' @param id_col an unquoted column name of ID data
#' @param qtcfbl_col an unquoted column name of baseline QTCB measurements, default is QTCBBL
#' @param deduplicate boolean, whether baseline values are duplicated over rows. If true duplicates will be removed from average
#'
#' @return a dataframe with deltaQTCFBL column
#' @export
#' @importFrom rlang .data
#' @examples
#' compute_delta_qtcfblm(compute_qtcb_qtcf(data))
compute_delta_qtcfblm <- function(
  data,
  id_col = ID,
  qtcfbl_col = QTCFBL,
  deduplicate = TRUE
) {
  checkmate::assertDataFrame(data)

  id <- rlang::enquo(id_col)
  qtcfbl <- rlang::enquo(qtcfbl_col)

  required_cols <- unlist(lapply(c(id, qtcfbl), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  baseline_data <- data %>%
    dplyr::select(!!id, !!qtcfbl)

  if (deduplicate) {
    baseline_data <- baseline_data %>%
      dplyr::distinct()
  }

  qtcfbl_mean <- mean(baseline_data[[rlang::as_name(qtcfbl)]], na.rm = TRUE)

  data <- add_column_if_absent(data, "QTCFBLM", qtcfbl_mean)
  data <- add_column_if_absent(data, "deltaQTCFBL", !!qtcfbl - qtcfbl_mean)
  return(data)
}


#' Pre-processes data
#' Computes QTcB, QTcF, deltaQTcF, deltaQTcB, deltaHR, deltaQTcB Baseline Mean,
#' deltaQTcF Baseline Mean, deltaHR Baseline Mean
#'
#' @param data Data frame containing QTc data
#' @param qt_col an unquoted column name of QT measurements, QT by default
#' @param qtbl_col an unquoted column name of baseline QT measurements, QTBL by default
#' @param rr_col an unquoted column name of RR measurements, RR by default
#' @param rrbl_col an unquoted column name of baseline RR measurements, RRBL by default
#' @param hr_col an unquoted column name of HR measurements, HR by default
#' @param hrbl_col an unquoted column name of baseline HR measurements, HRBL by default
#' @param qtcf_col an unquoted column name of QTCF measurements, QTCF by default
#' @param qtcfbl_col an unquoted column name of baseline QTCF measurements, QTCFBL by default
#' @param qtcb_col an unquoted column name of QTCB measurements, QTCB by default
#' @param qtcbbl_col an unquoted column name of baseline QTCB measurements, QTCBBL by default
#' @param id_col an unquoted column name of ID data
#' @param deduplicate boolean, whether baseline values are duplicated over rows. If true duplicates will be removed from average
#'
#' @return dataframe with deltas computed from BL
#' @export
#'
#' @examples preprocess(data)
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
  id_col = ID,
  deduplicate = TRUE
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
  id <- rlang::enquo(id_col)

  data %>%
    compute_qtcb_qtcf(!!qt, !!qtbl, !!rr, !!rrbl) %>%
    compute_delta_hrblm(!!id, !!hrbl, deduplicate) %>%
    compute_delta_qtcbblm(!!id, !!qtcbbl, deduplicate) %>%
    compute_delta_qtcfblm(!!id, !!qtcfbl, deduplicate) %>%
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

# Helper function to add column without overwriting
add_column_if_absent <- function(data, colname, expr) {
  if (!(colname %in% names(data))) {
    data <- dplyr::mutate(data, !!colname := !!rlang::enquo(expr))
  }
  data
}
