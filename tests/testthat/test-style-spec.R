test_that("combined spec plots remain restylable", {
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

  p <- gof_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = style_spec()
  )

  expect_s3_class(p, "patchwork")
  expect_no_error(restyle_plot(p, legend.position = "bottom"))
  expect_no_error(reveal(p, time, as = "facet"))
})

test_that("explicit shape legend settings survive automatic unification", {
  p <- eda_scatter_with_regressions(
    cqtkit_data_verapamil,
    deltaQTCF,
    CONC,
    TRTG,
    style = style_spec(
      legends = list(
        legend_spec(channel = "color", title = "Treatment", order = 1),
        legend_spec(channel = "shape", title = "Groups", order = 3)
      )
    )
  )

  expect_equal(p$labels$colour, "Treatment")
  expect_equal(p$labels$shape, "Groups")
  expect_equal(p$guides$guides$colour$params$order, 1)
  expect_equal(p$guides$guides$shape$params$order, 3)
})

test_that("color palette functions also provide mapped fill defaults", {
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

  p <- predict_with_exposure_plot(
    cqtkit_data_verapamil,
    fit,
    CONC,
    treatment_predictors = list(
      CONC = 0,
      deltaQTCFBL = 0,
      TRTG = "Verapamil HCL",
      TAFD = "0.5 HR"
    ),
    style = style_spec(colors = scales::hue_pal())
  )

  expect_no_error(ggplot2::ggplot_build(p))
})
