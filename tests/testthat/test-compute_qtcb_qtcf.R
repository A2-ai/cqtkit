test_that('compute_qtcb_qtcf messages and errors if QT, RR, QTBL, RRBL not supplied', {
  .test_data <- data %>% dplyr::select(-RR, -QT, -RRBL, -QTBL)
  expect_error(compute_qtcb_qtcf(.test_data))
})

test_that('compute_qtcb_qtcf messages and errors if RR is not supplied', {
  .test_data <- data %>% dplyr::select(-RR)
  expect_error(compute_qtcb_qtcf(.test_data))
})

test_that('compute_qtcb_qtcf messages and errors if QT is not supplied', {
  .test_data <- data %>% dplyr::select(-QT)
  expect_error(compute_qtcb_qtcf(.test_data))
})

test_that('compute_qtcb_qtcf messages and errors if RRBL is not supplied', {
  .test_data <- data %>% dplyr::select(-RRBL)
  expect_error(compute_qtcb_qtcf(.test_data))
})

test_that('compute_qtcb_qtcf messages and errors if QTBL is not supplied', {
  .test_data <- data %>% dplyr::select(-QTBL)
  expect_error(compute_qtcb_qtcf(.test_data))
})


test_that("compute_qtcb_qtcf will not compute QTCFBL and QTCBBL if either/both bl cols are null", {
  df1 <- compute_qtcb_qtcf(data, qtbl_col = NULL)
  expect_true(!("QTCBBL" %in% names(df1)))
  expect_true(!("QTCFBL" %in% names(df1)))

  df2 <- compute_qtcb_qtcf(data, rrbl_col = NULL)
  expect_true(!("QTCBBL" %in% names(df2)))
  expect_true(!("QTCFBL" %in% names(df2)))

  df3 <- compute_qtcb_qtcf(data, rrbl_col = NULL, qtbl_col = NULL)
  expect_true(!("QTCBBL" %in% names(df3)))
  expect_true(!("QTCFBL" %in% names(df3)))
})

test_that("compute_qtcb_qtcf will compute QTCF, QTCB, QTCFBL, QTCBBL using default QT, QTBL, RR, and RRBL", {
  expect_no_condition(compute_qtcb_qtcf(data))
})


test_that("compute_qtcb_qtcf will compute QTCF, QTCB, QTCFBL, QTCBBL using supplied col names", {
  test_data <- data %>%
    dplyr::rename(
      qt_data = QT,
      rr_data = RR,
      qtbl_data = QTBL,
      rrbl_data = RRBL
    )

  expect_false(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(df)))

  expect_no_condition(compute_qtcb_qtcf(
    test_data,
    qt_col = qt_data,
    rr_col = rr_data,
    qtbl_col = qtbl_data,
    rrbl_col = rrbl_data
  ))

  df <- compute_qtcb_qtcf(
    test_data,
    qt_col = qt_data,
    rr_col = rr_data,
    qtbl_col = qtbl_data,
    rrbl_col = rrbl_data
  )
  expect_true(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(df)))
})

test_that("compute_qtcb_qtcf will not overwrite existing QTCF, QTCB, QTCFBL, QTCBBL", {
  data_proc <- data %>% preprocess()
  expect_true(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(data_proc)))

  expect_false(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(data)))
  df <- data %>%
    compute_qtcb_qtcf()
  expect_true(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(df)))

  df <- data %>%
    dplyr::mutate(
      QTCB = 1,
      QTCF = 1,
    )
  expect_true(all(df$QTCB == 1))

  df2 <- df %>%
    preprocess()
  expect_true(all(df2$QTCB == 1))
})
