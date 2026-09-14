test_that('compute_high_qtc_obs counts observations correctly', {

  result <- compute_high_qtc_obs(cqtkit_data_verapamil, QTCF, deltaQTCF, qtc_thresholds = c(450), dqtc_thresholds = c(30))

  expected_qtc_gt_450 <- sum(cqtkit_data_verapamil$QTCF > 450, na.rm = TRUE)
  expected_dqtc_gt_30 <- sum(cqtkit_data_verapamil$deltaQTCF > 30, na.rm = TRUE)

  expect_equal(result$n_QTc_gt_450, expected_qtc_gt_450)
  expect_equal(result$n_dQTc_gt_30, expected_dqtc_gt_30)
})

test_that('compute_high_qtc_obs respects grouping column', {

  result <- compute_high_qtc_obs(cqtkit_data_verapamil, QTCF, deltaQTCF, TRTG, qtc_thresholds = c(450), dqtc_thresholds = c(30))

  # verify each group's count matches manual filtering
  for (grp in unique(cqtkit_data_verapamil$TRTG)) {
    grp_data <- cqtkit_data_verapamil |> dplyr::filter(TRTG == grp)
    grp_row <- result |> dplyr::filter(group == grp)

    expect_equal(grp_row$n_QTc_gt_450, sum(grp_data$QTCF > 450, na.rm = TRUE))
    expect_equal(grp_row$n_dQTc_gt_30, sum(grp_data$deltaQTCF > 30, na.rm = TRUE))
  }
})

test_that('compute_high_qtc_sub counts subjects correctly', {

  result <- compute_high_qtc_sub(cqtkit_data_verapamil, QTCF, deltaQTCF, ID, NULL, qtc_thresholds = c(450), dqtc_thresholds = c(30))

  expected_sub_qtc <- dplyr::n_distinct(cqtkit_data_verapamil$ID[cqtkit_data_verapamil$QTCF > 450])
  expected_sub_dqtc <- dplyr::n_distinct(cqtkit_data_verapamil$ID[cqtkit_data_verapamil$deltaQTCF > 30])

  expect_equal(result$n_QTc_gt_450, expected_sub_qtc)
  expect_equal(result$n_dQTc_gt_30, expected_sub_dqtc)
})

test_that("compute_high_qtc_sub() errors clearly when id_col is missing", {
  expect_error(
    compute_high_qtc_sub(cqtkit_data_verapamil, QTCF, deltaQTCF),
    "compute_high_qtc_obs()", fixed = TRUE
  )
})

test_that("compute_high_qtc_sub() requires an explicit group_col", {
  # a pre-2.0.0 positional call binds its group column to `id_col` and stops
  # here rather than silently counting distinct treatment groups
  expect_error(
    compute_high_qtc_sub(cqtkit_data_verapamil, QTCF, deltaQTCF, TRTG),
    "`group_col` is required", fixed = TRUE
  )
  expect_error(
    compute_high_qtc_sub(cqtkit_data_verapamil, QTCF, deltaQTCF, id_col = ID),
    "`group_col` is required", fixed = TRUE
  )
})
