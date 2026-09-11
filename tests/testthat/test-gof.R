fit <- fit_prespecified_model(
  cqtkit_data_verapamil, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
  method = "REML", remove_conc_iiv = TRUE
)

test_that("gof_plots snapshot", {
  p <- gof_plots(cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG)

  snapshot_plot(p, "gof-plots")
})

test_that("gof_concordance_plots snapshot", {
  p <- gof_concordance_plots(
    cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG
  )

  snapshot_plot(p, "gof-concordance")
})

test_that("gof_residuals_plots snapshot", {
  p <- gof_residuals_plots(
    cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG
  )

  snapshot_plot(p, "gof-residuals")
})

test_that("gof_qq_plots snapshot", {
  p <- gof_qq_plots(
    cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG
  )

  snapshot_plot(p, "gof-qq")
})

test_that("gof_residuals_time_boxplots snapshot", {
  p <- gof_residuals_time_boxplots(
    cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG
  )

  snapshot_plot(p, "gof-residuals-time-box")
})

test_that("gof_residuals_trt_boxplots snapshot", {
  p <- gof_residuals_trt_boxplots(
    cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG
  )

  snapshot_plot(p, "gof-residuals-trt-box")
})

test_that("gof_vpc_plot snapshot", {
  set.seed(804831)
  p <- gof_vpc_plot(cqtkit_data_verapamil, fit, CONC, deltaQTCF, nruns = 10)

  snapshot_plot(p, "gof-vpc")
})

test_that("gof_plots with style snapshot", {
  p <- gof_plots(
    cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG,
    style = ggstylekit::style_spec(
      colors = c("Placebo" = "grey", "Verapamil HCL" = "steelblue"),
      legends = ggstylekit::legend_spec(channel = "color", title = "Treatment"),
      legend.position = "bottom"
    )
  )

  snapshot_plot(p, "gof-plots-styled")
})

test_that("gof_vpc_plot seed makes simulations reproducible", {
  sims <- function(seed) {
    suppressWarnings(compute_summary_statistics_of_simulations(
      cqtkit_data_verapamil, fit, CONC, 0.9, nruns = 5, nbins = 10, seed = seed
    ))
  }
  expect_identical(sims(42), sims(42))
  expect_false(identical(sims(42), sims(43)))

  vpc <- function(seed) {
    suppressWarnings(gof_vpc_plot(
      cqtkit_data_verapamil, fit, CONC, deltaQTCF, nruns = 5, seed = seed
    ))
  }
  expect_identical(vpc(42)$data, vpc(42)$data)
})

test_that("gof_vpc_plot seed does not disturb the caller's RNG stream", {
  set.seed(1)
  before <- .Random.seed
  invisible(suppressWarnings(gof_vpc_plot(
    cqtkit_data_verapamil, fit, CONC, deltaQTCF, nruns = 2, seed = 99
  )))
  expect_identical(.Random.seed, before)
})
