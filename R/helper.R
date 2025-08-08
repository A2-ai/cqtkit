#' Internal function for helping dealing with enquos. returns the quo_name if quo is not null
#'
#' @param quo An rlang::enquo variable
#'
#' @return an rlang::quo_name if quo is not null.
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
#' @returns list of lower, upper x solutions
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
