test_that("compute_study_summary keeps factor level order in grouping", {
  tl <- c("Placebo", "Low", "High")
  lvls <- c("2.4 mg", "7.2 mg", "10 mg")
  dat <- cqtkit_data_verapamil %>%
    dplyr::mutate(
      TRTG = factor(tl[(as.integer(factor(ID)) %% 3) + 1], levels = tl),
      DOSEF = factor(lvls[(as.integer(factor(ID)) %% 3) + 1], levels = lvls)
    )

  expect_equal(compute_study_summary(dat, TRTG, ID)$grouping, c("Total", tl))
  expect_equal(
    compute_study_summary(dat, TRTG, ID, DOSEF)$grouping,
    c("Total", paste(tl, lvls))
  )
})
