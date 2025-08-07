test_that('compute_qtcb_qtcf messages and errors if QT, RR, QTBL, RRBL not supplied', {
  .test_data <- cqtkit_data_verapamil %>% dplyr::select(-RR, -QT, -RRBL, -QTBL)
  expect_error(compute_qtcb_qtcf(.test_data))
})

test_that('compute_qtcb_qtcf messages and errors if RR is not supplied', {
  .test_data <- cqtkit_data_verapamil %>% dplyr::select(-RR)
  expect_error(compute_qtcb_qtcf(.test_data))
})

test_that('compute_qtcb_qtcf messages and errors if QT is not supplied', {
  .test_data <- cqtkit_data_verapamil %>% dplyr::select(-QT)
  expect_error(compute_qtcb_qtcf(.test_data))
})

test_that('compute_qtcb_qtcf messages and errors if RRBL is not supplied', {
  .test_data <- cqtkit_data_verapamil %>% dplyr::select(-RRBL)
  expect_error(compute_qtcb_qtcf(.test_data))
})

test_that('compute_qtcb_qtcf messages and errors if QTBL is not supplied', {
  .test_data <- cqtkit_data_verapamil %>% dplyr::select(-QTBL)
  expect_error(compute_qtcb_qtcf(.test_data))
})


test_that("compute_qtcb_qtcf will not compute QTCFBL and QTCBBL if either/both bl cols are null", {
  .test_data <- cqtkit_data_verapamil %>% dplyr::select(-QTCBBL, -QTCFBL)
  expect_true(!("QTCBBL" %in% names(.test_data)))
  expect_true(!("QTCFBL" %in% names(.test_data)))

  df1 <- compute_qtcb_qtcf(.test_data, qtbl_col = NULL)
  expect_true(!("QTCBBL" %in% names(df1)))
  expect_true(!("QTCFBL" %in% names(df1)))

  df2 <- compute_qtcb_qtcf(.test_data, rrbl_col = NULL)
  expect_true(!("QTCBBL" %in% names(df2)))
  expect_true(!("QTCFBL" %in% names(df2)))

  df3 <- compute_qtcb_qtcf(.test_data, rrbl_col = NULL, qtbl_col = NULL)
  expect_true(!("QTCBBL" %in% names(df3)))
  expect_true(!("QTCFBL" %in% names(df3)))
})

test_that("compute_qtcb_qtcf will compute QTCF, QTCB, QTCFBL, QTCBBL using default QT, QTBL, RR, and RRBL", {
  expect_no_condition(compute_qtcb_qtcf(cqtkit_data_verapamil))
})


test_that("compute_qtcb_qtcf will compute QTCF, QTCB, QTCFBL, QTCBBL using supplied col names", {
  test_data <- cqtkit_data_verapamil %>%
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
  .test_data <- cqtkit_data_verapamil %>% dplyr::select(-QTCB, -QTCF, -QTCFBL, -QTCBBL)

  data_proc <- .test_data %>% preprocess()
  expect_true(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(data_proc)))

  expect_false(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(.test_data)))
  df <- .test_data %>%
    compute_qtcb_qtcf()
  expect_true(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(df)))

  df <- cqtkit_data_verapamil %>%
    dplyr::mutate(
      QTCB = 1,
      QTCF = 1,
    )
  expect_true(all(df$QTCB == 1))

  df2 <- df %>%
    preprocess()
  expect_true(all(df2$QTCB == 1))
})
