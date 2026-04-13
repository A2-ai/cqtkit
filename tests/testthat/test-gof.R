data_proc <- preprocess(cqtkit_data_verapamil)
fit <- fit_prespecified_model(
  data_proc, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
  method = "REML", remove_conc_iiv = TRUE
)

test_that("gof_plots snapshot", {
  p <- gof_plots(data_proc, fit, deltaQTCF, CONC, NTLD, TRTG)

  snapshot_plot(p, "gof-plots", width = 10, height = 10)
})

test_that("gof_concordance_plots snapshot", {
  p <- gof_concordance_plots(
    data_proc, fit, deltaQTCF, CONC, NTLD, TRTG, legend_location = "top"
  )

  snapshot_plot(p, "gof-concordance")
})

test_that("gof_residuals_plots snapshot", {
  p <- gof_residuals_plots(
    data_proc, fit, deltaQTCF, CONC, NTLD, TRTG, legend_location = "top"
  )

  snapshot_plot(p, "gof-residuals", width = 10, height = 10)
})

test_that("gof_qq_plots snapshot", {
  p <- gof_qq_plots(
    data_proc, fit, deltaQTCF, CONC, NTLD, TRTG, legend_location = "top"
  )

  snapshot_plot(p, "gof-qq")
})

test_that("gof_residuals_time_boxplots snapshot", {
  p <- gof_residuals_time_boxplots(
    data_proc, fit, deltaQTCF, CONC, NTLD, TRTG, legend_location = "top"
  )

  snapshot_plot(p, "gof-residuals-time-box", width = 10, height = 8)
})

test_that("gof_residuals_trt_boxplots snapshot", {
  p <- gof_residuals_trt_boxplots(
    data_proc, fit, deltaQTCF, CONC, NTLD, TRTG
  )

  snapshot_plot(p, "gof-residuals-trt-box")
})

test_that("gof_vpc_plot snapshot", {
  set.seed(804831)
  p <- gof_vpc_plot(data_proc, fit, CONC, deltaQTCF, nruns = 10)

  snapshot_plot(p, "gof-vpc")
})

test_that("gof_plots with style snapshot", {
  p <- gof_plots(
    data_proc, fit, deltaQTCF, CONC, NTLD, TRTG,
    style = set_style(
      colors = c("Placebo" = "grey", "Verapamil HCL" = "steelblue"),
      legend = "Treatment",
      legend.position = "bottom"
    )
  )

  snapshot_plot(p, "gof-plots-styled", width = 10, height = 10)
})
