test_that("compute_study_summary keeps factor level order in grouping", {
  tl <- c("Placebo", "Low", "High")
  lvls <- c("2.4 mg", "7.2 mg", "10 mg")
  dat <- cqtkit_data_verapamil %>%
    dplyr::mutate(
      TRTG = factor(tl[(as.integer(factor(ID)) %% 3) + 1], levels = tl),
      DOSEF = factor(lvls[(as.integer(factor(ID)) %% 3) + 1], levels = lvls)
    )

  by_treatment <- compute_study_summary(dat, TRTG, ID)
  by_dose <- compute_study_summary(dat, TRTG, ID, DOSEF)

  expect_s3_class(by_treatment$grouping, "factor")
  expect_equal(levels(by_treatment$grouping), c("Total", tl))
  expect_equal(as.character(by_treatment$grouping), c("Total", tl))
  expect_s3_class(by_dose$grouping, "factor")
  expect_equal(levels(by_dose$grouping), c("Total", paste(tl, lvls)))
  expect_equal(
    as.character(by_dose$grouping),
    c("Total", paste(tl, lvls))
  )
})

test_that("compute_study_summary orders numeric groups numerically", {
  dat <- tibble::tibble(
    id = 1:3,
    treatment = c("Drug", "Drug", "Placebo"),
    dose = c(120, 60, 0)
  )

  result <- compute_study_summary(dat, treatment, id, dose)

  expected <- c("Total", "Drug 60", "Drug 120", "Placebo 0")
  expect_s3_class(result$grouping, "factor")
  expect_equal(levels(result$grouping), expected)
  expect_equal(as.character(result$grouping), expected)
})
