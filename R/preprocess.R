#' Computes QTCF and QTCB from qt_col and rr_col and QTCFBL and QTCBBL from qtbl_col and rrbl_col
#'
#' @param data A data frame containing QT, RR, QTBL, RRBL
#' @param qt_col An unquoted column name for QT measurements
#' @param qtbl_col An unquoted column name for baseline QT measurements
#' @param rr_col An unquoted column name for RR measurements
#' @param rrbl_col An unquoted column name for baseline RR measurements
#'
#' @importFrom rlang .data
#'
#' @return data with QTCF, QTCB, QTCFBL, and QTCBBL columns
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

#' Computes delta variables RR, QTCF, HR, etc
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
#' @return dataframe with deltaPARAM columns included
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

#' computes delta HR BL Mean
#'
#' @param data A data frame containing C-QT analysis dataset
#' @param id_col An unquoted column name for subject ID
#' @param hrbl_col An unquoted column name for baseline HR measurements, default is HRBL
#' @param deduplicate Boolean, whether baseline values are duplicated over rows. If true duplicates will be removed from average
#'
#' @return data frame with deltaHRBL
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' compute_delta_hrblm(cqtkit_data_verapamil)
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
#' @param data A data frame containing C-QT analysis dataset
#' @param id_col An unquoted column name for subject ID
#' @param qtcbbl_col An unquoted column name for baseline QTCB measurements, default is QTCBBL
#' @param deduplicate Boolean, whether baseline values are duplicated over rows. If true duplicates will be removed from average
#'
#' @return a dataframe with deltaQTCBBL column
#' @export
#' @importFrom rlang .data
#' @examples
#' compute_delta_qtcbblm(compute_qtcb_qtcf(cqtkit_data_verapamil))
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
#' @param data A data frame containing C-QT analysis dataset
#' @param id_col An unquoted column name for subject ID
#' @param qtcfbl_col An unquoted column name for baseline QTCB measurements, default is QTCBBL
#' @param deduplicate Boolean, whether baseline values are duplicated over rows. If true duplicates will be removed from average
#'
#' @return a dataframe with deltaQTCFBL column
#' @export
#' @importFrom rlang .data
#' @examples
#' compute_delta_qtcfblm(compute_qtcb_qtcf(cqtkit_data_verapamil))
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
#' @param id_col An unquoted column name for subject ID
#' @param deduplicate Boolean, whether baseline values are duplicated over rows. If true duplicates will be removed from average
#'
#' @return dataframe with deltas computed from BL
#' @export
#'
#' @examples preprocess(cqtkit_data_verapamil)
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
