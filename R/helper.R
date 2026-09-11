#' Name Quo If Not Null
#'
#' Returns the quo_name if quo is not NULL.
#'
#' @param quo An rlang::enquo variable
#'
#' @return The quo_name if quo is not NULL, otherwise NULL
#'
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' dv <- rlang::enquo(DV)
#' name_quo_if_not_null(dv)
#' }
name_quo_if_not_null <- function(quo) {
  if (!rlang::quo_is_null(quo)) {
    return(rlang::quo_name(quo))
  }
}


#' Assert Column Names Are Syntactic
#'
#' Errors if any of the supplied names are non-syntactic (e.g. contain spaces),
#' since nlme::lme cannot parse them even when backtick-quoted.
#'
#' @param col_names Character vector of column names to validate
#'
#' @return Invisibly TRUE if all names are syntactic; otherwise stops with an
#'   actionable message.
#'
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' assert_syntactic_names(c("CONC", "Dosing Regimen"))
#' }
assert_syntactic_names <- function(col_names) {
  non_syntactic <- col_names[make.names(col_names) != col_names]
  if (length(non_syntactic) > 0) {
    stop(
      "Model column name(s) must be syntactic (no spaces or special characters): ",
      paste(sprintf('"%s"', non_syntactic), collapse = ", "),
      ".\nRename the column(s) before fitting, e.g. `",
      non_syntactic[1],
      "` -> `",
      gsub(" ", "_", non_syntactic[1]),
      "`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Assert Factors Retain Multiple Levels After NA Removal
#'
#' Errors if any categorical column collapses to fewer than 2 levels once rows
#' with missing values across `model_cols` are dropped (as nlme::lme does with
#' na.action = "na.exclude"). A single-level factor triggers the cryptic
#' "contrasts can be applied only to factors with 2 or more levels" error.
#'
#' @param data A data frame of model data
#' @param model_cols Character vector of all columns used in the model, used to
#'   determine complete cases
#' @param factor_cols Character vector of categorical columns to check
#'
#' @return Invisibly TRUE. Stops if any checked factor collapses to < 2 levels;
#'   warns if a factor loses level(s) but retains >= 2.
#'
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' assert_multilevel_factors(data, c("deltaQTCF", "CONC", "TRTG"), "TRTG")
#' }
assert_multilevel_factors <- function(data, model_cols, factor_cols) {
  complete_rows <- stats::complete.cases(data[, model_cols, drop = FALSE])
  complete_data <- data[complete_rows, , drop = FALSE]

  # For a dropped level of `col`, name the model column(s) entirely NA for its
  # rows (the actual reason those rows were removed).
  culprit_line <- function(lv, col) {
    lv_rows <- !is.na(data[[col]]) & as.character(data[[col]]) == lv
    always_na <- setdiff(
      model_cols[vapply(
        model_cols,
        function(mc) all(is.na(data[lv_rows, mc])),
        logical(1)
      )],
      col
    )
    if (length(always_na) > 0) {
      paste0(
        "  - level \"",
        lv,
        "\": always NA in column(s): ",
        paste(always_na, collapse = ", ")
      )
    } else {
      paste0(
        "  - level \"",
        lv,
        "\": rows dropped due to missing values across model columns"
      )
    }
  }

  for (col in factor_cols) {
    is_categorical <- is.factor(data[[col]]) || is.character(data[[col]])
    if (!is_categorical) {
      next
    }

    kept_levels    <- unique(as.character(complete_data[[col]]))
    present_levels <- unique(as.character(data[[col]][!is.na(data[[col]])]))
    dropped_levels <- setdiff(present_levels, kept_levels)

    if (length(dropped_levels) == 0) {
      next
    }

    culprit_lines <- vapply(dropped_levels, culprit_line, character(1), col = col)

    if (length(kept_levels) < 2) {
      kept_desc <- if (length(kept_levels) == 1) {
        paste0("a single level (\"", kept_levels, "\")")
      } else {
        "no levels"
      }
      stop(
        "Column \"",
        col,
        "\" collapses to ",
        kept_desc,
        " once rows with missing model values are dropped, so the model cannot be fit.\n",
        "Level(s) removed: ",
        paste(sprintf("\"%s\"", dropped_levels), collapse = ", "),
        "\n",
        paste(culprit_lines, collapse = "\n"),
        "\nFix the missing values in those column(s), or remove this term from the model.",
        call. = FALSE
      )
    }

    warning(
      "Column \"",
      col,
      "\" lost level(s) once rows with missing model values are dropped: ",
      paste(sprintf("\"%s\"", dropped_levels), collapse = ", "),
      "\n",
      paste(culprit_lines, collapse = "\n"),
      "\nThe model will be fit on the remaining ",
      length(kept_levels),
      " level(s).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Simple quadratic formula solver
#'
#' @param a X^2 coefficient
#' @param b X coefficient
#' @param c X^0 coefficient
#'
#' @return A list with lower_conc and upper_conc solutions
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{
#' quad_form(1, 4, 2)
#' }
quad_form <- function(a, b, c) {
  x0 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
  x1 <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)

  return(list(
    lower_conc = x0,
    upper_conc = x1
  ))
}

#' Normalise the `show_model_results` argument
#'
#' @param x TRUE, FALSE, NULL, or a `model_results_spec()`
#' @return A `cqtkit_model_results_spec`, or NULL when results are not shown
#' @keywords internal
#' @noRd
as_model_results_spec <- function(x) {
  if (is.null(x) || isFALSE(x)) {
    return(NULL)
  }
  if (isTRUE(x)) {
    return(model_results_spec())
  }
  if (inherits(x, "cqtkit_model_results_spec")) {
    return(x)
  }
  stop(
    "`show_model_results` must be TRUE, FALSE, or a `model_results_spec()`",
    call. = FALSE
  )
}

#' Format a number to a fixed number of decimal places, zero-padded
#'
#' @param x Numeric
#' @param digits Integer decimal places
#' @return Character, e.g. `fmt_fixed(0.1, 3)` is "0.100"
#' @keywords internal
#' @noRd
fmt_fixed <- function(x, digits) {
  formatC(x, format = "f", digits = digits)
}

#' Format a slope estimate, CI, and p-value for a plot caption
#'
#' @param label Model label prefix, e.g. "Linear Regression"
#' @param estimate Numeric slope estimate
#' @param lower,upper Numeric confidence bounds
#' @param pvalue Numeric slope p-value
#' @param spec A `model_results_spec()`
#' @return A caption string, or NULL if the spec shows nothing
#' @keywords internal
#' @noRd
format_model_results <- function(label, estimate, lower, upper, pvalue, spec) {
  lines <- character()
  if (spec$slope) {
    lines <- c(
      lines,
      paste0(
        label,
        " Slope [",
        round(spec$ci * 100),
        "% CI]: ",
        fmt_fixed(estimate, spec$digits),
        " [",
        fmt_fixed(lower, spec$digits),
        ", ",
        fmt_fixed(upper, spec$digits),
        "]"
      )
    )
  }
  if (spec$pvalue) {
    p_str <- if (is.na(pvalue)) {
      "NA"
    } else if (pvalue < spec$eps) {
      paste0("< ", format(spec$eps, scientific = FALSE))
    } else {
      fmt_fixed(pvalue, spec$digits)
    }
    lines <- c(lines, paste0("Slope p-value: ", p_str))
  }
  if (length(lines) == 0) {
    return(NULL)
  }
  paste(lines, collapse = "\n")
}
