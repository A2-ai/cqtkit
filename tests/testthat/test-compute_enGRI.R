test_that('compute_enGRI computes correct trapezoidal area', {
  # Simple case: 3 points, compute by hand
  # conc = [10, 5, 0], ddqtc = [2, 4, 0]
  # Padded: CP = [10, 5, 0, 0], ddQ = [2, 4, 0, 0]
  # Trapezoids:
  #   (10-5) * (2+4)/2 = 5 * 3 = 15
  #   (5-0)  * (4+0)/2 = 5 * 2 = 10
  #   (0-0)  * (0+0)/2 = 0
  # Sum = 25, normalized by max(conc)=10 -> 2.5
  .test_data <- tibble::tibble(
    conc = c(10, 5, 0),
    ddqtc = c(2, 4, 0)
  )

  result <- compute_enGRI(.test_data, conc, ddqtc)
  expect_equal(result, 2.5, tolerance = 1e-10)
})