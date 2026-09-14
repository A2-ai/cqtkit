raw_verapamil <- function() {
  dplyr::select(
    cqtkit_data_verapamil,
    -dplyr::any_of(c(
      "HRBLM", "QTCBBLM", "QTCFBLM",
      "deltaHRBL", "deltaQTCBBL", "deltaQTCFBL",
      "deltaQTCF", "deltaQTCB", "deltaHR", "deltaQT", "deltaRR"
    ))
  )
}

test_that("preprocess() adds the baseline-mean and delta columns", {
  raw <- raw_verapamil()
  out <- preprocess(raw, cqtkit_data_bl_verapamil, ID)
  expect_true(all(
    c("HRBLM", "QTCBBLM", "QTCFBLM", "deltaQTCFBL", "deltaQTCF") %in% names(out)
  ))
})

test_that("preprocess() explains the new signature on pre-2.0.0 calls", {
  raw <- raw_verapamil()
  expect_error(preprocess(raw), "now takes `(data, bl_data, by)`", fixed = TRUE)
  expect_error(preprocess(raw, ID), "now takes `(data, bl_data, by)`", fixed = TRUE)
  expect_error(preprocess(raw, ID), "object 'ID' not found", fixed = TRUE)
  expect_error(preprocess(raw, id_col = ID), "unused argument")
})

test_that("preprocess() requires by", {
  expect_error(
    preprocess(raw_verapamil(), cqtkit_data_bl_verapamil),
    "`by` is required", fixed = TRUE
  )
})
