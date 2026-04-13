test_that("compute_delta_hrblm computes correct baseline mean and delta", {
  data <- cqtkit_data_verapamil |>
    dplyr::select(-HRBLM, -deltaHRBL)

  result <- compute_delta_hrblm(data)

  bl_vals <- data |>
    dplyr::select(ID, HRBL) |>
    dplyr::distinct() |>
    dplyr::pull(HRBL)
  expected_mean <- mean(bl_vals, na.rm = TRUE)

  expect_equal(result$HRBLM[1], expected_mean, tolerance = 1e-10)
  expect_equal(result$deltaHRBL, data$HRBL - expected_mean, tolerance = 1e-10)
})

test_that("compute_delta_qtcbblm computes correct baseline mean and delta", {
  data <- cqtkit_data_verapamil |>
    compute_qtcb_qtcf() |>
    dplyr::select(-QTCBBLM, -deltaQTCBBL)

  result <- compute_delta_qtcbblm(data)

  bl_vals <- data |>
    dplyr::select(ID, QTCBBL) |>
    dplyr::distinct() |>
    dplyr::pull(QTCBBL)
  expected_mean <- mean(bl_vals, na.rm = TRUE)

  expect_equal(result$QTCBBLM[1], expected_mean, tolerance = 1e-10)
  expect_equal(result$deltaQTCBBL, data$QTCBBL - expected_mean, tolerance = 1e-10)
})

test_that("compute_delta_qtcfblm computes correct baseline mean and delta", {
  data <- cqtkit_data_verapamil |>
    compute_qtcb_qtcf() |>
    dplyr::select(-QTCFBLM, -deltaQTCFBL)

  result <- compute_delta_qtcfblm(data)

  bl_vals <- data |>
    dplyr::select(ID, QTCFBL) |>
    dplyr::distinct() |>
    dplyr::pull(QTCFBL)
  expected_mean <- mean(bl_vals, na.rm = TRUE)

  expect_equal(result$QTCFBLM[1], expected_mean, tolerance = 1e-10)
  expect_equal(result$deltaQTCFBL, data$QTCFBL - expected_mean, tolerance = 1e-10)
})
