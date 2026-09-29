fit <- fit_prespecified_model(
  cqtkit_data_verapamil,
  deltaQTCF,
  ID,
  CONC,
  deltaQTCFBL,
  TRTG,
  TAFD,
  method = "REML",
  remove_conc_iiv = TRUE
)

test_that("gof_plots snapshot", {
  p <- gof_plots(cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG)

  snapshot_plot(p, "gof-plots")
})

test_that("gof_plots with style snapshot", {
  p <- gof_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = set_style(
      colors = c("Placebo" = "grey", "Verapamil HCL" = "steelblue"),
      legend = "Treatment"
    ),
    legend_location = "bottom"
  )

  snapshot_plot(p, "gof-plots-styled")
})

test_that("gof_concordance_plots snapshot", {
  p <- gof_concordance_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG
  )

  snapshot_plot(p, "gof-concordance")
})

test_that("gof_residuals_plots snapshot", {
  p <- gof_residuals_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG
  )

  snapshot_plot(p, "gof-residuals")
})

test_that("gof_qq_plots snapshot", {
  p <- gof_qq_plots(cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG)

  snapshot_plot(p, "gof-qq")
})

test_that("gof_residuals_time_boxplots snapshot", {
  p <- gof_residuals_time_boxplots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG
  )

  snapshot_plot(p, "gof-residuals-time-box")
})

test_that("gof_residuals_trt_boxplots snapshot", {
  p <- gof_residuals_trt_boxplots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG
  )

  snapshot_plot(p, "gof-residuals-trt-box")
})

test_that("gof_vpc_plot snapshot", {
  p <- suppressWarnings(gof_vpc_plot(
    cqtkit_data_verapamil,
    fit,
    CONC,
    deltaQTCF,
    nruns = 10,
    seed = 804831
  ))

  snapshot_plot(p, "gof-vpc")
})

test_that("gof_residuals_plots loess_line adds a LOESS line to each panel", {
  has_smooth <- function(p) {
    vapply(seq_len(length(p)), function(i) {
      any(vapply(
        p[[i]]$layers,
        function(l) inherits(l$geom, "GeomSmooth"),
        logical(1)
      ))
    }, logical(1))
  }
  p <- gof_residuals_plots(
    cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG,
    style = style_spec(), loess_line = TRUE
  )
  expect_true(all(has_smooth(p)))

  p <- gof_residuals_plots(
    cqtkit_data_verapamil, fit, deltaQTCF, CONC, NTLD, TRTG,
    style = style_spec()
  )
  expect_false(any(has_smooth(p)))
})
