data_proc <- preprocess(cqtkit_data_verapamil)

test_that("the new arguments leave the default captions unchanged", {
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

  off <- eda_qt_rr_plot(
    data_proc,
    RR,
    QT,
    model_type = "lm",
    show_model_results = FALSE
  )
  expect_null(off$labels$caption)
})

test_that("include_pvalue warns and does nothing without show_model_results", {
  expect_warning(
    p <- eda_qt_rr_plot(
      data_proc,
      RR,
      QT,
      ID,
      model_type = "lme",
      show_model_results = FALSE,
      include_pvalue = TRUE
    ),
    "ignored because `show_model_results` is FALSE"
  )

  expect_null(p$labels$caption)
})

test_that("include_pvalue adds a scientific p-value by default", {
  p <- eda_qt_rr_plot(
    data_proc,
    RR,
    QT,
    model_type = "lm",
    include_pvalue = TRUE
  )

  expect_match(p$labels$caption, "Slope p-value: [0-9.]+e-[0-9]+$")
})

test_that("pvalue_eps defaults to the floor decimals can display", {
  three <- eda_qt_rr_plot(
    data_proc,
    RR,
    QT,
    model_type = "lm",
    include_pvalue = TRUE,
    scientific = FALSE
  )
  six <- eda_qt_rr_plot(
    data_proc,
    RR,
    QT,
    model_type = "lm",
    include_pvalue = TRUE,
    scientific = FALSE,
    decimals = 6
  )
  coarse <- eda_qt_rr_plot(
    data_proc,
    RR,
    QT,
    model_type = "lm",
    include_pvalue = TRUE,
    scientific = FALSE,
    pvalue_eps = 0.05
  )

  expect_match(three$labels$caption, "Slope p-value: < 0.001$")
  expect_match(six$labels$caption, "Slope p-value: < 0.000001$")
  expect_match(coarse$labels$caption, "Slope p-value: < 0.05$")
})

test_that("an eps finer than decimals can show warns instead of printing 0", {
  expect_warning(
    p <- eda_qt_rr_plot(
      data_proc,
      RR,
      QT,
      model_type = "lm",
      include_pvalue = TRUE,
      scientific = FALSE,
      pvalue_eps = 1e-100
    ),
    "printed as 0"
  )

  expect_match(p$labels$caption, "Slope p-value: 0$")
})

test_that("decimals pads to a fixed width and NULL rounds", {
  padded <- eda_qt_rr_plot(data_proc, RR, QT, model_type = "lm", decimals = 4)

  expect_equal(
    padded$labels$caption,
    "Linear Regression Slope [90% CI]: 0.1070 [0.0989, 0.1150]"
  )
  expect_equal(fmt_caption_number(0.1, NULL), 0.1)
  expect_equal(fmt_caption_number(0.1, 3), "0.100")
})

test_that("eda_qtc_comparison_plot forwards the caption arguments", {
  expect_no_error(
    eda_qtc_comparison_plot(
      data_proc,
      RR,
      QT,
      QTCB,
      QTCF,
      trt_col = TRTG,
      model_type = "lm",
      include_pvalue = TRUE,
      scientific = TRUE
    )
  )
})
