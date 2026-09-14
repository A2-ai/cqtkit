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

#' Null coalescing operator
#'
#' Returns the left-hand side if it is not NULL, otherwise returns the right-hand side.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
