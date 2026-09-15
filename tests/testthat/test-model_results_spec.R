data_proc <- preprocess(cqtkit_data_verapamil)

test_that("spec defaults leave the captions unchanged", {
  lm_plot <- eda_qt_rr_plot(data_proc, RR, QT, model_type = "lm")
  expect_equal(
    lm_plot$labels$caption,
    "Linear Regression Slope [90% CI]: 0.107 [0.099, 0.115]"
  )

  lme_plot <- eda_qt_rr_plot(data_proc, RR, QT, ID, model_type = "lme")
  expect_equal(
    lme_plot$labels$caption,
    "Linear Mixed Effects Slope [90% CI]: 0.132 [0.119, 0.144]"
  )
})

test_that("show_model_results still accepts TRUE and FALSE", {
  with_results <- eda_qt_rr_plot(
    data_proc,
    RR,
    QT,
    model_type = "lm",
    show_model_results = TRUE
  )
  without <- eda_qt_rr_plot(
    data_proc,
    RR,
    QT,
    model_type = "lm",
    show_model_results = FALSE
  )

  expect_equal(
    with_results$labels$caption,
    "Linear Regression Slope [90% CI]: 0.107 [0.099, 0.115]"
  )
  expect_null(without$labels$caption)
})

test_that("a spec drives the confidence level, p-value and digits", {
  p <- eda_qt_rr_plot(
    data_proc,
    RR,
    QT,
    model_type = "lm",
    show_model_results = model_results_spec(
      ci = 0.95,
      pvalue = TRUE,
      digits = 3
    )
  )

  expect_equal(
    p$labels$caption,
    "Linear Regression Slope [95% CI]: 0.107 [0.097, 0.117]\nSlope p-value: < 0.001"
  )
})

test_that("conf_int warns and still sets the confidence level", {
  expect_warning(
    p <- eda_qt_rr_plot(data_proc, RR, QT, model_type = "lm", conf_int = 0.95),
    "deprecated"
  )

  expect_equal(
    p$labels$caption,
    "Linear Regression Slope [95% CI]: 0.107 [0.097, 0.117]"
  )
})

test_that("eda_qtc_comparison_plot passes the spec down without warning", {
  expect_no_warning(
    eda_qtc_comparison_plot(
      data_proc,
      RR,
      QT,
      QTCB,
      QTCF,
      trt_col = TRTG,
      model_type = "lm",
      show_model_results = model_results_spec(pvalue = TRUE)
    )
  )
})
