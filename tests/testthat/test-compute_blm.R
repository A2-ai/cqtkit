.bl <- tibble::tibble(
  ID = c(1, 1, 2, 2, 3),
  TRTG = factor(c("A", "A", "A", "A", "A")),
  RR = c(1000, 1000, 800, 1200, 1000),
  QT = c(400, 410, 380, 420, 390)
)

.bl_derived <- compute_qtcb_qtcf(
  compute_hr(.bl, rrbl_col = NULL),
  qtbl_col = NULL,
  rrbl_col = NULL
)

hrblm <- function(data, bl_data = .bl_derived, ...) {
  compute_blm(data, bl_data, ecg_param_col = HR, blm_name = "HRBLM", ...)
}

test_that("compute_hr derives HR and HRBL from RR", {
  d <- tibble::tibble(RR = c(1000, 500), RRBL = c(1000, 2000))

  expect_equal(compute_hr(d)$HR, c(60, 120))
  expect_equal(compute_hr(d)$HRBL, c(60, 30))
  expect_false("HRBL" %in% names(compute_hr(d, rrbl_col = NULL)))
  expect_false("HR" %in% names(compute_hr(d, rr_col = NULL)))
})

test_that("compute_hr does not overwrite an existing HR", {
  d <- tibble::tibble(RR = 1000, RRBL = 1000, HR = 99)

  expect_equal(compute_hr(d)$HR, 99)
})

test_that("compute_blm averages within group before averaging across groups", {
  hr <- 60000 / .bl$RR
  expected <- mean(c(mean(hr[1:2]), mean(hr[3:4]), hr[5]))

  out <- hrblm(tibble::tibble(x = 1), by = ID)

  expect_equal(unique(out$HRBLM), expected)
})

test_that("compute_blm averages corrections computed per replicate", {
  qtcb <- .bl$QT / sqrt(.bl$RR / 1000)
  qtcf <- .bl$QT / (.bl$RR / 1000)^(1 / 3)

  b <- compute_blm(
    tibble::tibble(x = 1),
    .bl_derived,
    by = ID,
    ecg_param_col = QTCB,
    blm_name = "QTCBBLM"
  )
  f <- compute_blm(
    tibble::tibble(x = 1),
    .bl_derived,
    by = ID,
    ecg_param_col = QTCF,
    blm_name = "QTCFBLM"
  )

  expect_equal(
    unique(b$QTCBBLM),
    mean(c(mean(qtcb[1:2]), mean(qtcb[3:4]), qtcb[5]))
  )
  expect_equal(
    unique(f$QTCFBLM),
    mean(c(mean(qtcf[1:2]), mean(qtcf[3:4]), qtcf[5]))
  )
})

test_that("the blm scalar differs from a flat mean over baseline rows", {
  flat <- mean(60000 / .bl$RR)

  out <- hrblm(tibble::tibble(x = 1), by = ID)

  expect_false(isTRUE(all.equal(unique(out$HRBLM), flat)))
})

test_that("by accepts several columns", {
  out <- hrblm(tibble::tibble(x = 1), by = c(ID, TRTG))

  expect_equal(
    unique(out$HRBLM),
    unique(hrblm(tibble::tibble(x = 1), by = ID)$HRBLM)
  )
})

test_that("by is required", {
  expect_error(hrblm(tibble::tibble(x = 1)))
  expect_error(
    hrblm(tibble::tibble(x = 1), by = 1),
    "bare symbols or strings"
  )
})

test_that("by accepts bare symbols and strings", {
  expected <- unique(hrblm(tibble::tibble(x = 1), by = c(ID, TRTG))$HRBLM)

  for (by in list(quote(c("ID", "TRTG")), quote(c(ID, "TRTG")))) {
    out <- rlang::eval_tidy(rlang::expr(
      hrblm(tibble::tibble(x = 1), by = !!by)
    ))
    expect_equal(unique(out$HRBLM), expected)
  }

  expect_equal(
    unique(hrblm(tibble::tibble(x = 1), by = "ID")$HRBLM),
    unique(hrblm(tibble::tibble(x = 1), by = ID)$HRBLM)
  )
})

test_that("compute_blm errors when value_col is absent from bl_data", {
  expect_error(
    hrblm(tibble::tibble(x = 1), bl_data = .bl, by = ID),
    "HR"
  )
})

test_that("compute_blm drops groups with NA and warns", {
  bl_na <- .bl_derived
  bl_na$HR[1] <- NA

  expect_warning(
    out <- hrblm(tibble::tibble(x = 1), bl_data = bl_na, by = ID),
    "dropped 1 group"
  )

  hr <- 60000 / .bl$RR
  expect_equal(unique(out$HRBLM), mean(c(mean(hr[3:4]), hr[5])))
})

test_that("compute_blm does not overwrite an existing column", {
  out <- hrblm(tibble::tibble(HRBLM = 1), by = ID)

  expect_equal(out$HRBLM, 1)
})
