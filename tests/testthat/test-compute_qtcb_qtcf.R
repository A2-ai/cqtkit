test_that("compute_qtcb_qtcf errors if QT, RR, QTBL or RRBL is missing", {
  for (col in c("QT", "RR", "QTBL", "RRBL")) {
    expect_error(
      compute_qtcb_qtcf(dplyr::select(cqtkit_data_verapamil, -dplyr::all_of(col))),
      paste0("missing elements \\{'", col, "'\\}")
    )
  }
})

test_that("compute_qtcb_qtcf will not compute QTCFBL and QTCBBL if either/both bl cols are null", {
  .test_data <- cqtkit_data_verapamil %>% dplyr::select(-QTCBBL, -QTCFBL)

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

test_that("compute_qtcb_qtcf will compute QTCF, QTCB, QTCFBL, QTCBBL using supplied col names", {
  test_data <- cqtkit_data_verapamil %>%
    dplyr::select(-QTCB, -QTCBBL, -QTCF, -QTCFBL) %>%
    dplyr::rename(
      qt_data = QT,
      rr_data = RR,
      qtbl_data = QTBL,
      rrbl_data = RRBL
    )

  df <- compute_qtcb_qtcf(
    test_data,
    qt_col = qt_data,
    rr_col = rr_data,
    qtbl_col = qtbl_data,
    rrbl_col = rrbl_data
  )

  expect_equal(df$QTCB, test_data$qt_data / sqrt(test_data$rr_data / 1000))
  expect_equal(df$QTCF, test_data$qt_data / (test_data$rr_data / 1000)^(1 / 3))
  expect_equal(df$QTCBBL, test_data$qtbl_data / sqrt(test_data$rrbl_data / 1000))
  expect_equal(df$QTCFBL, test_data$qtbl_data / (test_data$rrbl_data / 1000)^(1 / 3))
})
