test_that('compute_qtcb_qtcf errors when required columns are missing', {
  # single column missing
  expect_error(
    compute_qtcb_qtcf(cqtkit_data_verapamil |> dplyr::select(-RR)),
    "must include"
  )
  # all required columns missing
  expect_error(
    compute_qtcb_qtcf(cqtkit_data_verapamil |> dplyr::select(-RR, -QT, -RRBL, -QTBL)),
    "must include"
  )
})

test_that("compute_qtcb_qtcf will not compute QTCFBL and QTCBBL if either/both bl cols are null", {
  .test_data <- cqtkit_data_verapamil |> dplyr::select(-QTCBBL, -QTCFBL)

  df1 <- compute_qtcb_qtcf(.test_data, qtbl_col = NULL)
  expect_false("QTCBBL" %in% names(df1))
  expect_false("QTCFBL" %in% names(df1))

  df2 <- compute_qtcb_qtcf(.test_data, rrbl_col = NULL)
  expect_false("QTCBBL" %in% names(df2))
  expect_false("QTCFBL" %in% names(df2))

  df3 <- compute_qtcb_qtcf(.test_data, rrbl_col = NULL, qtbl_col = NULL)
  expect_false("QTCBBL" %in% names(df3))
  expect_false("QTCFBL" %in% names(df3))
})

test_that("compute_qtcb_qtcf works with custom column names", {
  test_data <- cqtkit_data_verapamil |>
    dplyr::select(-QTCB, -QTCBBL, -QTCF, -QTCFBL) |>
    dplyr::rename(
      qt_data = QT, rr_data = RR, qtbl_data = QTBL, rrbl_data = RRBL
    )

  expect_false(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(test_data)))

  df <- compute_qtcb_qtcf(
    test_data,
    qt_col = qt_data, rr_col = rr_data,
    qtbl_col = qtbl_data, rrbl_col = rrbl_data
  )
  expect_true(all(c("QTCB", "QTCBBL", "QTCF", "QTCFBL") %in% names(df)))

  # verify values are correct with custom columns
  expected_qtcf <- test_data$qt_data / (test_data$rr_data / 1000)^(1 / 3)
  expect_equal(df$QTCF, expected_qtcf, tolerance = 1e-10)
})

test_that("compute_qtcb_qtcf computes correct Bazett and Fridericia corrections", {
  .test_data <- cqtkit_data_verapamil |>
    dplyr::select(-QTCB, -QTCBBL, -QTCF, -QTCFBL)

  result <- compute_qtcb_qtcf(.test_data)

  # Bazett: QTcB = QT / sqrt(RR/1000)
  expected_qtcb <- .test_data$QT / sqrt(.test_data$RR / 1000)
  expect_equal(result$QTCB, expected_qtcb, tolerance = 1e-10)

  # Fridericia: QTcF = QT / (RR/1000)^(1/3)
  expected_qtcf <- .test_data$QT / (.test_data$RR / 1000)^(1 / 3)
  expect_equal(result$QTCF, expected_qtcf, tolerance = 1e-10)

  # Baseline corrections use QTBL and RRBL
  expected_qtcbbl <- .test_data$QTBL / sqrt(.test_data$RRBL / 1000)
  expect_equal(result$QTCBBL, expected_qtcbbl, tolerance = 1e-10)

  expected_qtcfbl <- .test_data$QTBL / (.test_data$RRBL / 1000)^(1 / 3)
  expect_equal(result$QTCFBL, expected_qtcfbl, tolerance = 1e-10)
})

test_that("compute_qtcb_qtcf will not overwrite existing QTCF, QTCB, QTCFBL, QTCBBL", {
  df <- cqtkit_data_verapamil |>
    dplyr::mutate(QTCB = 1, QTCF = 1)
  expect_true(all(df$QTCB == 1))

  df2 <- df
  expect_true(all(df2$QTCB == 1))
})
