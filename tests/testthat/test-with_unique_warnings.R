collect_conditions <- function(expr) {
  messages <- character()
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        messages <<- c(messages, paste("warning:", conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      messages <<- c(messages, paste("error:", conditionMessage(e)))
    }
  )
  messages
}

test_that("with_unique_warnings keeps warnings raised before an error", {
  out <- collect_conditions(with_unique_warnings({
    warning("a")
    stop("b")
  }))

  expect_identical(out, c("warning: a", "error: b"))
})

test_that("with_unique_warnings gives each warning once", {
  out <- collect_conditions(with_unique_warnings({
    warning("a")
    warning("b")
    warning("a")
    1
  }))

  expect_identical(out, c("warning: a", "warning: b"))
  expect_identical(with_unique_warnings(1), 1)
})

test_that("nested with_unique_warnings gives each warning once", {
  out <- collect_conditions(with_unique_warnings({
    warning("a")
    with_unique_warnings({
      warning("a")
      warning("b")
      warning("b")
    })
    warning("b")
  }))

  expect_identical(out, c("warning: a", "warning: b"))
})
