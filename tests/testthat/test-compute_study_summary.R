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
