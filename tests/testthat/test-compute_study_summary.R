test_that('compute_study_summary returns correct subject counts', {
  data <- cqtkit_data_verapamil |> preprocess()

  result <- compute_study_summary(data, TRTG, ID)

  # total row should match overall distinct IDs
  total_row <- result |> dplyr::filter(grouping == "Total")
  expect_equal(total_row$n_sub, dplyr::n_distinct(data$ID))

  # each treatment group count should match manual count
  non_total <- result |> dplyr::filter(grouping != "Total")
  for (grp in unique(data$TRTG)) {
    grp_row <- non_total |> dplyr::filter(grouping == as.character(grp))
    expected_n <- dplyr::n_distinct(data$ID[data$TRTG == grp])
    expect_equal(grp_row$n_sub, expected_n)
  }
})
