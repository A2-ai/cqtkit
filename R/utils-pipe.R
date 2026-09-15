#' Pipe operator
#'
#' `r lifecycle::badge("deprecated")`
#'
#' The re-export of magrittr's pipe from cqtkit is deprecated and will be
#' removed in cqtkit 2.0.0. Attach the operator yourself with `library(dplyr)`
#' or `library(magrittr)`, or use the base pipe `|>` (R >= 4.1.0).
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
