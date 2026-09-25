data_proc <- preprocess(cqtkit_data_verapamil)

test_that("compute_high_qtc_subjects counts distinct subjects", {
  res <- compute_high_qtc_subjects(
    data_proc,
    QTCF,
    deltaQTCF,
    ID,
    qtc_thresholds = c(400, 420),
    dqtc_thresholds = c(5)
  )

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("group", "n_QTc_gt_400", "n_QTc_gt_420", "n_dQTc_gt_5"))
  expect_equal(res$group, "Total")

  expected <- length(unique(data_proc$ID[data_proc$QTCF > 400]))
  expect_equal(res$n_QTc_gt_400, expected)
})

test_that("compute_high_qtc_observations counts observations", {
  res <- compute_high_qtc_observations(
    data_proc,
    QTCF,
    deltaQTCF,
    qtc_thresholds = c(400, 420),
    dqtc_thresholds = c(5)
  )

  expect_equal(res$n_QTc_gt_400, sum(data_proc$QTCF > 400, na.rm = TRUE))
})

test_that("subject counts are lower than observation counts when subjects repeat", {
  subs <- compute_high_qtc_subjects(
    data_proc,
    QTCF,
    deltaQTCF,
    ID,
    TRTG,
    qtc_thresholds = c(400),
    dqtc_thresholds = c(5)
  )
  obs <- compute_high_qtc_observations(
    data_proc,
    QTCF,
    deltaQTCF,
    TRTG,
    qtc_thresholds = c(400),
    dqtc_thresholds = c(5)
  )

  expect_equal(subs$group, obs$group)
  expect_true(all(subs$n_QTc_gt_400 < obs$n_QTc_gt_400))
})

test_that("grouping produces one row per group", {
  res <- compute_high_qtc_subjects(data_proc, QTCF, deltaQTCF, ID, TRTG)

  expect_equal(nrow(res), dplyr::n_distinct(data_proc$TRTG))
})

test_that("custom thresholds drive the column names", {
  res <- compute_high_qtc_subjects(
    data_proc,
    QTCF,
    deltaQTCF,
    ID,
    qtc_thresholds = c(410, 415, 420),
    dqtc_thresholds = c(5, 10, 15)
  )

  expect_named(
    res,
    c(
      "group",
      "n_QTc_gt_410",
      "n_QTc_gt_415",
      "n_QTc_gt_420",
      "n_dQTc_gt_5",
      "n_dQTc_gt_10",
      "n_dQTc_gt_15"
    )
  )
})

test_that("missing columns are caught", {
  expect_error(
    compute_high_qtc_subjects(data_proc, NOPE, deltaQTCF, ID),
    "missing elements \\{'NOPE'\\}"
  )
})

test_that("compute_high_qtc_sub always warns and keeps observation counts", {
  expect_warning(
    old <- compute_high_qtc_sub(data_proc, QTCF, deltaQTCF),
    "counts observations, not subjects"
  )
  expect_warning(
    old <- compute_high_qtc_sub(data_proc, QTCF, deltaQTCF),
    "deprecated"
  )

  new <- compute_high_qtc_observations(data_proc, QTCF, deltaQTCF)
  expect_equal(old, new)
})

test_that("tabulate_high_qtc_sub always warns", {
  expect_warning(
    tbl <- tabulate_high_qtc_sub(data_proc, QTCF, deltaQTCF, group_col = DOSEF),
    "counts observations, not subjects"
  )
  expect_warning(
    tbl <- tabulate_high_qtc_sub(data_proc, QTCF, deltaQTCF, group_col = DOSEF),
    "deprecated"
  )
  expect_s3_class(tbl, "gt_tbl")
})
