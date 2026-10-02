test_that("compute_enGRI is the loop area divided by the maximum concentration", {
  triangle <- tibble::tibble(CONC = c(2, 2), ddQTCF = c(0, 2))

  expect_equal(compute_enGRI(triangle, CONC, ddQTCF), 1)
})
