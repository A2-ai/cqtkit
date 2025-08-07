
test_that("can set individual elements correctly", {
  style <- set_style(
    title = "My Title",
    xlabel = "X Label",
    xlims = c(0.1, 10),
    logx = TRUE
  )
  expect_equal(style$title, "My Title")
  expect_equal(style$xlabel, "X Label")
  expect_equal(style$xlims, c(0.1, 10))
  expect_true(style$logx)
})

test_that("can accept plotmath expressions and bquote objects", {
  lbl <- expression(Delta ~ QTcF)
  style1 <- set_style(ylabel = lbl)
  expect_equal(style1$ylabel, lbl)

  lbl2 <- bquote(Delta ~ Delta ~ "QTcF (ms)")
  style2 <- set_style(ylabel = lbl2)
  expect_equal(style2$ylabel, lbl2)
})

test_that("rejects invalid xlims and ylims", {
  expect_error(
    set_style(xlims = c(10, 5)),
    "xlims must be in increasing order"
  )
  expect_error(
    set_style(ylims = c(1)),
    "ylims must be a numeric vector of length 2"
  )
  expect_error(set_style(xlims = "bad"), "xlims must be a numeric vector")
})

test_that("rejects logx/logy when limits are non-positive", {
  expect_warning(
    set_style(logx = TRUE, xlims = c(0, 10)),
    "xlims should be > 0"
  )
  expect_warning(
    set_style(logy = TRUE, ylims = c(-5, 10)),
    "ylims should be > 0"
  )
})

test_that("rejects unnamed or malformed color/label vectors", {
  expect_error(
    set_style(colors = c("red", "blue")),
    "colors must be a named character vector"
  )
  expect_error(set_style(labels = c(1, 2)), "labels must be a named vector")
})

test_that("rejects invalid legend.position", {
  expect_error(
    set_style(legend.position = "middle"),
    "legend.position must be one of"
  )
})

test_that("updates existing style with new values", {
  base <- set_style(title = "Initial", logx = FALSE)
  updated <- set_style(style = base, title = "Updated", logx = TRUE)
  expect_equal(updated$title, "Updated")
  expect_true(updated$logx)
})

test_that("unknown elements in existing style list emit a warning", {
  bad_style <- list(title = "Bad", badkey = 42)
  expect_warning(
    style <- set_style(style = bad_style),
    "unknown keys"
  )
  expect_equal(style$title, "Bad")
})
