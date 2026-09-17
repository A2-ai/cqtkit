.data <- cqtkit_data_verapamil

test_that("preprocess errors when a corrected QT column is missing", {
  expect_error(
    preprocess(dplyr::select(.data, -QTCF)),
    "Corrected QT is nonlinear in RR"
  )
  expect_error(
    preprocess(dplyr::select(.data, -QTCBBL)),
    "`QTCBBL` not found"
  )
})

test_that("preprocess names every missing corrected QT column", {
  expect_error(
    preprocess(dplyr::select(.data, -QTCB, -QTCF)),
    "`QTCB`, `QTCF` not found"
  )
})

test_that("the override restores the pre-1.2.0 derivation", {
  stripped <- dplyr::select(
    .data,
    -QTCB,
    -QTCF,
    -QTCBBL,
    -QTCFBL,
    -dplyr::starts_with("delta")
  )

  out <- withr::with_options(
    list(cqtkit.override_preprocessing_error = TRUE),
    preprocess(stripped)
  )

  expect_equal(out$QTCF, stripped$QT / ((stripped$RR / 1000)^(1 / 3)))
  expect_true("deltaQTCF" %in% names(out))
})

test_that("preprocess leaves stored corrected QT alone", {
  out <- preprocess(dplyr::select(.data, -dplyr::starts_with("delta")))

  expect_equal(out$QTCF, .data$QTCF)
  expect_equal(out$deltaQTCF, .data$deltaQTCF)
})

test_that("compute_delta_*blm error when the population mean is missing", {
  expect_error(
    compute_delta_hrblm(dplyr::select(.data, -HRBLM)),
    "The population baseline mean is now computed by"
  )
  expect_error(
    compute_delta_qtcbblm(dplyr::select(.data, -QTCBBLM)),
    "`QTCBBLM` not found"
  )
  expect_error(
    compute_delta_qtcfblm(dplyr::select(.data, -QTCFBLM)),
    "`QTCFBLM` not found"
  )
})

test_that("compute_delta_*blm subtract the population mean already on data", {
  out <- compute_delta_hrblm(dplyr::select(.data, -deltaHRBL))

  expect_equal(out$deltaHRBL, .data$HRBL - .data$HRBLM)
})

test_that("the deprecated arguments warn only when supplied", {
  withr::local_options(
    cqtkit.override_preprocessing_error = TRUE,
    lifecycle_verbosity = "warning"
  )
  stripped <- dplyr::select(.data, -HRBLM, -deltaHRBL)

  expect_no_warning(compute_delta_hrblm(stripped))
  expect_warning(
    compute_delta_hrblm(stripped, ID),
    "`id_col` argument of `compute_delta_hrblm\\(\\)` is deprecated"
  )
  expect_warning(
    compute_delta_hrblm(stripped, deduplicate = FALSE),
    "`deduplicate` argument of `compute_delta_hrblm\\(\\)` is deprecated"
  )
})

test_that("the deprecated arguments still take effect under the override", {
  withr::local_options(cqtkit.override_preprocessing_error = TRUE)
  stripped <- dplyr::select(.data, -HRBLM, -deltaHRBL)

  deduped <- suppressWarnings(
    compute_delta_hrblm(stripped, deduplicate = TRUE)
  )
  all_rows <- suppressWarnings(
    compute_delta_hrblm(stripped, deduplicate = FALSE)
  )

  expect_equal(
    unique(deduped$HRBLM),
    mean(dplyr::distinct(dplyr::select(.data, ID, HRBL))$HRBL)
  )
  expect_equal(unique(all_rows$HRBLM), mean(.data$HRBL))
})

test_that("preprocess warns under its own name, not the delta functions", {
  withr::local_options(
    cqtkit.override_preprocessing_error = TRUE,
    lifecycle_verbosity = "warning"
  )

  expect_warning(
    preprocess(.data, deduplicate = TRUE),
    "`deduplicate` argument of `preprocess\\(\\)` is deprecated"
  )
})

test_that("NULL skips a parameter the data does not carry", {
  fridericia <- dplyr::select(
    cqtkit_data_verapamil,
    -dplyr::starts_with("delta"),
    -QTCB,
    -QTCBBL,
    -QTCBBLM
  )

  out <- preprocess(
    fridericia,
    qtcb_col = NULL,
    qtcbbl_col = NULL,
    qtcbblm_col = NULL
  )

  expect_true(all(c("deltaQTCF", "deltaQTCFBL") %in% names(out)))
  expect_false(any(c("deltaQTCB", "deltaQTCBBL") %in% names(out)))
})

test_that("preprocess reads population means from the named columns", {
  renamed <- cqtkit_data_verapamil %>%
    dplyr::select(-dplyr::starts_with("delta")) %>%
    dplyr::rename(HR_MEAN_BL = HRBLM)

  out <- preprocess(renamed, hrblm_col = HR_MEAN_BL)

  expect_equal(out$deltaHRBL, renamed$HRBL - renamed$HR_MEAN_BL)
})
