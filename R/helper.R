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

#' Paste grouping values together while keeping factor level order
#'
#' `paste()` returns character, which then sorts lexically in legends and in
#' `summarise()` output. This returns a factor whose levels follow the level
#' order of `x` (and, when `y` is a vector, of `y` within each level of `x`).
#'
#' @param x Factor or character grouping values
#' @param y Either a single string appended to every value, or a second
#'   grouping vector the same length as `x`
#' @param sep Separator passed to `paste()`
#' @return A factor
#' @keywords internal
#' @noRd
paste_grouping <- function(x, y, sep = " ") {
  x <- as.factor(x)
  if (length(y) == 1L) {
    return(factor(
      paste(x, y, sep = sep),
      levels = paste(levels(x), y, sep = sep)
    ))
  }
  y <- as.factor(y)
  lvls <- as.vector(t(outer(levels(x), levels(y), paste, sep = sep)))
  droplevels(factor(paste(x, y, sep = sep), levels = lvls))
}

# Pull an optional grouping column without silently merging missing groups.
complete_group_values <- function(data, group) {
  values <- dplyr::pull(data, !!group)
  if (anyNA(values)) {
    stop(
      "`group_col` column `", rlang::as_name(group),
      "` contains missing values. Fill or filter these rows, or omit ",
      "`group_col` to use the default grouping.",
      call. = FALSE
    )
  }
  values
}

#' Error if any model column name is non-syntactic
#'
#' `nlme::lme()` builds its formula from pasted column names and cannot parse
#' non-syntactic ones (e.g. containing spaces), failing downstream in
#' `str2lang()` with a message that names neither the column nor the cause.
#'
#' @param col_names Character vector of column names used in the model
#' @return `TRUE`, invisibly
#' @keywords internal
#' @noRd
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

#' Error or warn when a categorical predictor loses levels to missing values
#'
#' Rows with missing model values are dropped when the model is fit
#' (`na.action = "na.exclude"`). A categorical predictor can lose levels as a
#' result, and if it collapses below two levels `nlme::lme()` fails inside
#' `contrasts<-` without naming the column. Detect it first, and report which
#' model column is responsible for each dropped level.
#'
#' @param data The modelling data frame
#' @param model_cols Character vector of all column names used in the model
#' @param factor_cols Character vector of the categorical predictors to check
#' @return `TRUE`, invisibly
#' @keywords internal
#' @noRd
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

    kept_levels <- unique(as.character(complete_data[[col]]))
    present_levels <- unique(as.character(data[[col]][!is.na(data[[col]])]))
    dropped_levels <- setdiff(present_levels, kept_levels)

    if (length(dropped_levels) == 0) {
      next
    }

    culprit_lines <- vapply(
      dropped_levels,
      culprit_line,
      character(1),
      col = col
    )

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

#' Insert a zero-valued row for each term's reference level
#'
#' `nlme::lme()` estimates each factor level against a reference level, which it
#' therefore does not report. Add it back so a reader can see the comparator
#' rather than infer it from which level is absent.
#'
#' @param sum Tibble of fixed effect estimates, one row per parameter
#' @param fit The fitted model, carrying the contrasts used in fitting
#' @param col_names Character vector of the factor columns to check
#' @return `sum` with a reference row inserted above each term's first level
#' @keywords internal
#' @noRd
add_reference_level_rows <- function(sum, fit, col_names) {
  reference_row <- function(level_name, term) {
    tibble::tibble(
      Parameters = paste0(level_name, " (Reference)"),
      Value = 0,
      Std.Error = NA_real_,
      DF = NA_integer_,
      `t-value` = NA_real_,
      `p-value` = NA_real_,
      CIl = NA_real_,
      CIu = NA_real_,
      .term = term
    )
  }

  for (col in col_names) {
    if (!col %in% sum$.term) {
      next
    }

    contrast <- fit$contrasts[[col]]
    if (!is.matrix(contrast)) {
      next
    }
    ref_level <- rownames(contrast)[rowSums(abs(contrast)) == 0]
    if (length(ref_level) != 1) {
      next
    }

    first_idx <- which(sum$.term == col)[1]
    if (is.na(first_idx)) {
      next
    }

    sum <- dplyr::bind_rows(
      sum[seq_len(first_idx - 1), ],
      reference_row(ref_level, col),
      sum[first_idx:nrow(sum), ]
    )
  }

  sum
}

#' Classify model parameters into sections and order the rows by them
#'
#' @param parameters Tibble of model parameters
#' @param trt_col_name,tafd_col_name,conc_col_name,baseline_col_name Column
#'   names used in model fitting, used to recognise each parameter
#' @return `parameters` with a `Section` factor column, ordered by it
#' @keywords internal
#' @noRd
add_parameter_sections <- function(
  parameters,
  trt_col_name,
  tafd_col_name,
  conc_col_name,
  baseline_col_name
) {
  params <- parameters$Parameters
  term <- parameters$.term

  parameters$Section <- dplyr::case_when(
    term %in% conc_col_name ~ "Slope",
    term %in% trt_col_name ~ "Treatment",
    term %in% c("(Intercept)", baseline_col_name) ~ "Intercept",
    term %in% tafd_col_name ~ "Time",
    grepl("^IIV", params) | params == "Residual Error" ~ "Random Effects",
    TRUE ~ "Other"
  )
  parameters$Section <- factor(
    parameters$Section,
    levels = c(
      "Slope",
      "Treatment",
      "Intercept",
      "Time",
      "Random Effects",
      "Other"
    )
  )

  parameters[order(parameters$Section), ]
}

#' Build the summarise expressions counting values above each threshold
#'
#' @param qtc_thresholds,dqtc_thresholds Numeric threshold vectors
#' @param count Either "subjects" (distinct ids) or "observations" (rows)
#' @return A named list of quosures for `dplyr::summarise()`
#' @keywords internal
#' @noRd
high_qtc_count_exprs <- function(qtc_thresholds, dqtc_thresholds, count) {
  counter <- function(col) {
    if (count == "subjects") {
      function(thresh) {
        rlang::expr(dplyr::n_distinct(
          .data$id[.data[[!!col]] > !!thresh],
          na.rm = TRUE
        ))
      }
    } else {
      function(thresh) {
        rlang::expr(sum(.data[[!!col]] > !!thresh, na.rm = TRUE))
      }
    }
  }

  qtc_exprs <- lapply(qtc_thresholds, counter("qtc"))
  names(qtc_exprs) <- paste0("n_QTc_gt_", qtc_thresholds)

  dqtc_exprs <- lapply(dqtc_thresholds, counter("deltaqtc"))
  names(dqtc_exprs) <- paste0("n_dQTc_gt_", dqtc_thresholds)

  c(qtc_exprs, dqtc_exprs)
}

#' Summarise threshold counts, grouped or as a single total row
#'
#' @param qtdf Tibble with the qtc/deltaqtc (and optionally id) columns
#' @param data The original data, used to pull the grouping column
#' @param group A quosure for the grouping column, possibly NULL
#' @param exprs Named list of summarise expressions
#' @return A tibble with a `group` column followed by one column per threshold
#' @keywords internal
#' @noRd
summarise_high_qtc <- function(qtdf, data, group, exprs) {
  if (!rlang::quo_is_null(group)) {
    qtdf |>
      dplyr::mutate(group = data |> dplyr::pull(!!group)) |>
      dplyr::group_by(.data$group) |>
      dplyr::summarise(!!!exprs)
  } else {
    qtdf |>
      dplyr::summarise(!!!exprs) |>
      dplyr::mutate(group = "Total", .before = 1)
  }
}

#' Render a high QTc count tibble as a gt table
#'
#' @param n_gt Tibble of counts from a `compute_high_qtc_*()` function
#' @param group A quosure for the grouping column, possibly NULL
#' @param group_label Optional label for the group column
#' @param qtc_label String label for the QTc parameter
#' @param unit String for the unit of measurement
#' @param qtc_thresholds,dqtc_thresholds Numeric threshold vectors
#' @param title Optional table title, wrapped in `gt::md()`
#' @param dots Additional arguments for `gt::tab_options()`
#' @return A gt table
#' @keywords internal
#' @noRd
render_high_qtc_table <- function(
  n_gt,
  group,
  group_label,
  qtc_label,
  unit,
  qtc_thresholds,
  dqtc_thresholds,
  title,
  dots
) {
  qtc_labels <- lapply(qtc_thresholds, function(thresh) {
    gt::md(paste0(qtc_label, " > ", thresh, " ", unit))
  })
  names(qtc_labels) <- paste0("n_QTc_gt_", qtc_thresholds)

  dqtc_labels <- lapply(dqtc_thresholds, function(thresh) {
    gt::md(paste0("&Delta; ", qtc_label, " > ", thresh, " ", unit))
  })
  names(dqtc_labels) <- paste0("n_dQTc_gt_", dqtc_thresholds)

  t <- n_gt |>
    gt::gt() |>
    gt::cols_label(!!!c(qtc_labels, dqtc_labels))

  if (!is.null(title)) {
    t <- t |>
      gt::tab_header(title = gt::md(title))
  }

  if (is.null(group_label)) {
    group_label <- if (!rlang::quo_is_null(group)) {
      name_quo_if_not_null(group)
    } else {
      ""
    }
  }
  t <- t |>
    gt::cols_label(group = group_label)

  tab_option_args <- dots[names(dots) %in% names(formals(gt::tab_options))]
  tab_option_args$data <- t

  do.call(gt::tab_options, tab_option_args)
}

#' Format a caption number
#'
#' `decimals = NULL` rounds to three places, which is what the plot captions
#' did before `decimals` existed. An integer pads to a fixed width, so 0.1
#' prints as "0.100" under `decimals = 3`.
#'
#' @param x Numeric
#' @param decimals Integer decimal places, or NULL to round
#' @return Numeric when `decimals` is NULL, character otherwise
#' @keywords internal
#' @noRd
fmt_caption_number <- function(x, decimals) {
  if (is.null(decimals)) {
    return(round(x, 3))
  }
  formatC(x, format = "f", digits = decimals)
}

#' Format a slope estimate, its confidence interval and its p-value
#'
#' @param label Model label prefix, e.g. "Linear Regression"
#' @param estimate Numeric slope estimate
#' @param lower,upper Numeric confidence bounds
#' @param pvalue Numeric slope p-value
#' @param conf_int Numeric confidence interval level
#' @param include_pvalue Logical, add the p-value on a second line
#' @param pvalue_eps Numeric, p-values below this print as "< eps", or NULL for
#'   no cutoff
#' @param decimals Integer decimal places, or NULL to round
#' @param scientific Logical, show the p-value in scientific notation
#' @return A caption string
#' @keywords internal
#' @noRd
format_model_results <- function(
  label,
  estimate,
  lower,
  upper,
  pvalue,
  conf_int,
  include_pvalue = FALSE,
  pvalue_eps = NULL,
  decimals = NULL,
  scientific = TRUE
) {
  caption <- paste0(
    label,
    " Slope [",
    round(conf_int * 100),
    "% CI]: ",
    fmt_caption_number(estimate, decimals),
    " [",
    fmt_caption_number(lower, decimals),
    ", ",
    fmt_caption_number(upper, decimals),
    "]"
  )

  if (!include_pvalue) {
    return(caption)
  }

  p_str <- if (is.na(pvalue)) {
    "NA"
  } else if (scientific) {
    formatC(pvalue, format = "e", digits = decimals %||% 3)
  } else if (!is.null(pvalue_eps) && pvalue < pvalue_eps) {
    paste0("< ", format(pvalue_eps, scientific = FALSE))
  } else {
    rounded <- fmt_caption_number(pvalue, decimals)
    if (as.numeric(rounded) == 0) {
      warning(
        "The slope p-value printed as 0: it is smaller than `decimals` (",
        decimals %||% 3,
        ") can show. Use `scientific = TRUE`, or set `pvalue_eps` to print ",
        "it as `< eps`.",
        call. = FALSE
      )
    }
    rounded
  }

  paste0(caption, "\nSlope p-value: ", p_str)
}

#' Warn when `include_pvalue` is set but the annotation is switched off
#'
#' @param show_model_results Logical
#' @param include_pvalue Logical
#' @return NULL, invisibly
#' @keywords internal
#' @noRd
warn_unused_include_pvalue <- function(show_model_results, include_pvalue) {
  if (include_pvalue && !show_model_results) {
    warning(
      "`include_pvalue` is ignored because `show_model_results` is FALSE. ",
      "The p-value is part of the model results annotation, so set ",
      "`show_model_results = TRUE` to see it.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# lifecycle::is_present() forces the promise, so it cannot be used on an
# argument whose value is a bare column name.
deprecated_quo_is_present <- function(arg_quo) {
  !identical(rlang::quo_get_expr(arg_quo), quote(lifecycle::deprecated()))
}

#' Column names from a captured quosure
#'
#' @param quo A quosure holding a column name, or a `c()` of them, as bare
#'   symbols or strings.
#' @param arg Name of the argument to report in the error.
#' @return A character vector of column names.
#' @noRd
names_from_quo <- function(quo, arg = rlang::caller_arg(quo)) {
  expr <- rlang::quo_get_expr(quo)
  parts <- if (rlang::is_call(expr, "c")) rlang::call_args(expr) else list(expr)

  is_symbol_or_string <- function(x) {
    rlang::is_symbol(x) || rlang::is_string(x)
  }

  if (!all(vapply(parts, is_symbol_or_string, logical(1)))) {
    stop(
      "`",
      arg,
      "` must be column names, as bare symbols or strings.",
      call. = FALSE
    )
  }

  vapply(parts, rlang::as_string, character(1))
}
