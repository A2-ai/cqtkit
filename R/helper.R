#' Internal function for helping dealing with enquos. returns the quo_name if quo is not null
#'
#' @param quo an rlang::enquo variable.
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
#' @param a x^2 coefficient
#' @param b x coefficient
#' @param c x^0 coefficient
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

#' Generates a suffix depending on the last number in n.
#'
#' @param n an integer
#'
#' @return a string of a number and the appropriate suffix
#' @keywords internal
#' @noRd
#'
#' @examples \dontrun{suffix(42)}
suffix <- function(n) {
  #Pretty sure this doesn't work as intended with decimals. but good enough for now.
  suffixes <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")

  if (n %% 1 != 0) {
    int <- as.numeric(strsplit(as.character(n), "\\.")[[1]][1])
    num_str <- strsplit(as.character(int), "")[[1]]
  } else {
    num_str <- strsplit(as.character(n), "")[[1]]
  }
  rev_num_str <- rev(num_str)

  last <- as.numeric(rev_num_str[[1]])
  if (!('.' %in% rev_num_str)) {
    if (n > 10) {
      last_two <- as.numeric(paste0(rev_num_str[1:2], collapse = ''))
      if (last_two %in% c(11, 21, 31)) {
        #reversed
        suffix <- 'th'
      }
    }
    suffix <- suffixes[last + 1]
    s <- paste0(n, suffix)
  } else {
    i = 1
    while (last == 0) {
      i <- i + 1
      last <- as.numeric(rev_num_str[[i]])
    }
    suffix <- 'th'
    s <- paste0(n, suffix)
  }

  s
}


#' Null coalescing operator
#'
#' Returns the left-hand side if it is not NULL, otherwise returns the right-hand side.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
