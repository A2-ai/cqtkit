test_that('compute_study_summary returns correct subject counts', {

  result <- compute_study_summary(cqtkit_data_verapamil, TRTG, ID)

  # total row should match overall distinct IDs
  total_row <- result |> dplyr::filter(grouping == "Total")
  expect_equal(total_row$n_sub, dplyr::n_distinct(cqtkit_data_verapamil$ID))

  # each treatment group count should match manual count
  non_total <- result |> dplyr::filter(grouping != "Total")
  for (grp in unique(cqtkit_data_verapamil$TRTG)) {
    grp_row <- non_total |> dplyr::filter(grouping == as.character(grp))
    expected_n <- dplyr::n_distinct(cqtkit_data_verapamil$ID[cqtkit_data_verapamil$TRTG == grp])
    expect_equal(grp_row$n_sub, expected_n)
  }
})

test_that("compute_study_summary keeps factor level order in grouping", {
  tl <- c("Placebo", "Low", "High")
  lvls <- c("2.4 mg", "7.2 mg", "10 mg")
  dat <- cqtkit_data_verapamil |>
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
