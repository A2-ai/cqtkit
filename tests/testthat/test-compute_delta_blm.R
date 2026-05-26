test_that("compute_hrblm reproduces the population mean baseline HR", {
  result <- cqtkit_data_verapamil |>
    dplyr::select(-HRBLM) |>
    compute_hrblm(cqtkit_data_bl_verapamil, by = c(ID, TRTG))

  expected <- cqtkit_data_bl_verapamil |>
    dplyr::group_by(ID, TRTG) |>
    dplyr::summarise(m = mean(60000 / RR), .groups = "drop") |>
    dplyr::pull(m) |>
    mean()

  expect_equal(unique(result$HRBLM), expected, tolerance = 1e-10)
  expect_equal(unique(result$HRBLM), unique(cqtkit_data_verapamil$HRBLM), tolerance = 1e-8)
})

test_that("compute_qtcbblm reproduces the population mean baseline QTcB", {
  result <- cqtkit_data_verapamil |>
    dplyr::select(-QTCBBLM) |>
    compute_qtcbblm(cqtkit_data_bl_verapamil, by = c(ID, TRTG))

  expected <- cqtkit_data_bl_verapamil |>
    dplyr::group_by(ID, TRTG) |>
    dplyr::summarise(m = mean(QT / sqrt(RR / 1000)), .groups = "drop") |>
    dplyr::pull(m) |>
    mean()

  expect_equal(unique(result$QTCBBLM), expected, tolerance = 1e-10)
  expect_equal(unique(result$QTCBBLM), unique(cqtkit_data_verapamil$QTCBBLM), tolerance = 1e-8)
})

test_that("compute_qtcfblm reproduces the population mean baseline QTcF", {
  result <- cqtkit_data_verapamil |>
    dplyr::select(-QTCFBLM) |>
    compute_qtcfblm(cqtkit_data_bl_verapamil, by = c(ID, TRTG))

  expected <- cqtkit_data_bl_verapamil |>
    dplyr::group_by(ID, TRTG) |>
    dplyr::summarise(m = mean(QT / (RR / 1000)^(1 / 3)), .groups = "drop") |>
    dplyr::pull(m) |>
    mean()

  expect_equal(unique(result$QTCFBLM), expected, tolerance = 1e-10)
  expect_equal(unique(result$QTCFBLM), unique(cqtkit_data_verapamil$QTCFBLM), tolerance = 1e-8)
})

test_that("compute_delta_hrblm computes deltaHRBL from HRBL and HRBLM", {
  data <- cqtkit_data_verapamil |> dplyr::select(-deltaHRBL)
  result <- compute_delta_hrblm(data)
  expect_equal(result$deltaHRBL, data$HRBL - data$HRBLM, tolerance = 1e-10)
  expect_equal(result$deltaHRBL, cqtkit_data_verapamil$deltaHRBL, tolerance = 1e-10)
})

test_that("compute_delta_qtcbblm computes deltaQTCBBL from QTCBBL and QTCBBLM", {
  data <- cqtkit_data_verapamil |> dplyr::select(-deltaQTCBBL)
  result <- compute_delta_qtcbblm(data)
  expect_equal(result$deltaQTCBBL, data$QTCBBL - data$QTCBBLM, tolerance = 1e-10)
  expect_equal(result$deltaQTCBBL, cqtkit_data_verapamil$deltaQTCBBL, tolerance = 1e-10)
})

test_that("compute_delta_qtcfblm computes deltaQTCFBL from QTCFBL and QTCFBLM", {
  data <- cqtkit_data_verapamil |> dplyr::select(-deltaQTCFBL)
  result <- compute_delta_qtcfblm(data)
  expect_equal(result$deltaQTCFBL, data$QTCFBL - data$QTCFBLM, tolerance = 1e-10)
  expect_equal(result$deltaQTCFBL, cqtkit_data_verapamil$deltaQTCFBL, tolerance = 1e-10)
})
