test_that("legend order follows factor levels, not row order", {
  set.seed(1)
  lv <- c("2.4 mg/kg", "4.8 mg/kg", "7.2 mg/kg", "10 mg/kg", "12 mg/kg")
  d <- data.frame(
    CONC = runif(500, 0, 500),
    dHR = rnorm(500, 0, 10),
    DOSEC = factor(sample(lv, 500, TRUE), levels = lv)
  )
  d <- d[order(runif(nrow(d))), ]

  p <- eda_scatter_with_regressions(
    d, dHR, CONC, trt_col = DOSEC, loess_line = FALSE
  )

  expect_equal(
    as.character(ggplot2::get_guide_data(p, "colour")$.label),
    lv
  )
})
