#' Describe where a population baseline mean now comes from
#'
#' Shared wording for the deprecation warnings and errors that point callers at
#' [compute_blm()].
#'
#' @return A one-sentence string.
#' @noRd
blm_deprecate_details <- function() {
  paste0(
    "The population baseline mean is now computed by `compute_blm()` from ",
    "baseline ECG data."
  )
}

#' Is the pre-1.2.0 preprocessing behaviour requested?
#'
#' @return `TRUE` when `options(cqtkit.override_preprocessing_error = TRUE)` is
#'   set, otherwise `FALSE`.
#' @noRd
preprocessing_override <- function() {
  isTRUE(getOption("cqtkit.override_preprocessing_error", FALSE))
}

#' Pre-1.2.0 population baseline mean and delta
#'
#' Averages the per-subject baseline values already on `data` and subtracts
#' that mean. The baseline values have themselves been averaged across
#' replicates, so the result is not the mean of the replicate values. Reached
#' only under `preprocessing_override()`. Removed in 2.0.0.
#'
#' @param data A data frame containing a C-QT analysis dataset.
#' @param id Quosure for the subject identifier column.
#' @param bl Quosure for the per-subject baseline column.
#' @param deduplicate Whether to reduce `data` to one row per subject before
#'   averaging.
#' @param blm_name Name of the population baseline mean column to add.
#' @param delta_name Name of the delta column to add.
#' @return `data` with both columns added.
#' @noRd
legacy_delta_blm <- function(data, id, bl, deduplicate, blm_name, delta_name) {
  required_cols <- unlist(lapply(c(id, bl), name_quo_if_not_null))
  checkmate::assertNames(names(data), must.include = required_cols)

  baseline_data <- dplyr::select(data, !!id, !!bl)
  if (deduplicate) {
    baseline_data <- dplyr::distinct(baseline_data)
  }

  bl_mean <- mean(baseline_data[[rlang::as_name(bl)]], na.rm = TRUE)

  data <- add_column_if_absent(data, blm_name, bl_mean)
  add_column_if_absent(data, delta_name, !!bl - bl_mean)
}

#' Warn about the deprecated `id_col` and `deduplicate` arguments
#'
#' Warns for each argument that was supplied, since the population baseline
#' mean is no longer computed from `data`, and supplies the pre-1.2.0 default
#' for each that was not. Delete alongside those arguments in 2.0.0.
#'
#' @param id Quosure for `id_col`, possibly `lifecycle::deprecated()`.
#' @param deduplicate Value of `deduplicate`, possibly
#'   `lifecycle::deprecated()`.
#' @param fn Name of the calling function, for the warning.
#' @param user_env Environment of the caller the warning is attributed to.
#'
#' @return A list with the resolved `id` quosure and `deduplicate` flag.
#' @noRd
warn_deprecated_blm_args <- function(
  id,
  deduplicate,
  fn,
  user_env
) {
  if (deprecated_quo_is_present(id)) {
    lifecycle::deprecate_warn(
      when = "1.2.0",
      what = paste0(fn, "(id_col)"),
      details = blm_deprecate_details(),
      user_env = user_env,
      always = TRUE
    )
  } else {
    id <- rlang::quo(ID)
  }

  if (lifecycle::is_present(deduplicate)) {
    lifecycle::deprecate_warn(
      when = "1.2.0",
      what = paste0(fn, "(deduplicate)"),
      details = blm_deprecate_details(),
      user_env = user_env,
      always = TRUE
    )
  } else {
    deduplicate <- TRUE
  }

  list(id = id, deduplicate = deduplicate)
}

#' Handle the deprecated `id_col` and `deduplicate` arguments
#'
#' Warns when either was supplied, since the population baseline mean is no
#' longer computed from `data`. Delete alongside those arguments in 2.0.0.
#'
#' @param data A data frame containing a C-QT analysis dataset.
#' @param id Quosure for `id_col`, possibly `lifecycle::deprecated()`.
#' @param bl Quosure for the per-subject baseline column.
#' @param deduplicate Value of `deduplicate`, possibly
#'   `lifecycle::deprecated()`.
#' @param blm Quosure for the population baseline mean column.
#' @param delta_name Name of the delta column to add.
#' @param fn Name of the calling function, for the warning.
#'
#' @return The pre-1.2.0 result when `cqtkit.override_preprocessing_error` is
#'   set and `blm` is absent from `data`, otherwise `NULL`.
#' @noRd
deprecated_blm_delta <- function(
  data,
  id,
  bl,
  deduplicate,
  blm,
  delta_name,
  fn,
  user_env
) {
  args <- warn_deprecated_blm_args(id, deduplicate, fn, user_env)
  id <- args$id
  deduplicate <- args$deduplicate

  blm_name <- rlang::as_name(blm)
  if (!preprocessing_override() || blm_name %in% names(data)) {
    return(NULL)
  }

  legacy_delta_blm(
    data = data,
    id = id,
    bl = bl,
    deduplicate = deduplicate,
    blm_name = blm_name,
    delta_name = delta_name
  )
}
