errorbar_layer_data <- function(p) {
  indices <- which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomErrorbar"),
    logical(1)
  ))
  expect_length(indices, 1L)
  ggplot2::ggplot_build(p)$data[[indices[[1]]]]
}

test_that("error bar styling preserves CI, SE and SD calculations", {
  for (interval in c("CI", "SE", "SD")) {
    for (reference in list(NULL, "0 mg")) {
      make_plot <- function(style) {
        eda_mean_dv_over_time(
          cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
          reference_dose = reference,
          error_bars = interval,
          style = style
        )
      }
      baseline <- errorbar_layer_data(make_plot(style_spec()))
      p <- make_plot(style_spec(
        errorbar_color = "grey60",
        errorbar_width = 0.12,
        errorbar_linewidth = 1.2,
        errorbar_alpha = 0.4,
        errorbar_linetype = "dotted",
        line_linewidth = 0.8
      ))
      bars <- errorbar_layer_data(p)
      expect_equal(bars[c("x", "group", "ymin", "ymax")], baseline[c("x", "group", "ymin", "ymax")])
      expect_true(all(bars$colour == "grey60"))
      expect_equal(bars$xmax - bars$xmin, rep(0.12, nrow(bars)))
      expect_true(all(bars$linewidth == 1.2))
      expect_true(all(bars$alpha == 0.4))
      expect_true(all(bars$linetype == "dotted"))

      lines <- which(vapply(
        p$layers, function(layer) inherits(layer$geom, "GeomLine"), logical(1)
      ))
      expect_true(all(ggplot2::ggplot_build(p)$data[[lines[[1]]]]$linewidth == 0.8))
      line_only <- errorbar_layer_data(make_plot(style_spec(line_linewidth = 2)))
      expect_equal(line_only$linewidth, baseline$linewidth)

      updated <- errorbar_layer_data(restyle_plot(
        p, errorbar_color = "purple", errorbar_width = 0.25, errorbar_linewidth = 0.7
      ))
      expect_equal(updated$xmax - updated$xmin, rep(0.25, nrow(updated)))
      expect_true(all(updated$linewidth == 0.7))
      expect_true(all(updated$colour == "purple"))
      expect_equal(updated[c("ymin", "ymax")], baseline[c("ymin", "ymax")])
    }
  }
})

test_that("quantile plots pass error bar settings to ggstylekit", {
  fit <- fit_prespecified_model(
    cqtkit_data_verapamil, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    remove_conc_iiv = TRUE
  )
  spec <- style_spec(
    errorbar_color = "purple",
    errorbar_width = 5,
    errorbar_linewidth = 1.1
  )
  eda <- eda_quantiles_plot(
    dplyr::filter(cqtkit_data_verapamil, DOSE > 0),
    CONC, deltaQTCF, trt_col = TRTG, style = spec
  )
  expect_warning(
    prediction <- predict_with_quantiles_plot(
      cqtkit_data_verapamil, fit, CONC, deltaQTCF,
      treatment_predictors = list(
        CONC = 0, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "0.5 HR"
      ),
      style = spec
    ),
    "quantiles had duplicates"
  )
  for (p in list(eda, prediction)) {
    bars <- errorbar_layer_data(p)
    points <- which(vapply(
      p$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)
    ))
    point_colors <- ggplot2::ggplot_build(p)$data[[points[[1]]]]$colour
    expect_true(all(bars$colour == "purple"))
    expect_false(any(point_colors == "purple"))
    expect_true(all(bars$linewidth == 1.1))
    expect_equal(bars$xmax - bars$xmin, rep(5, nrow(bars)))
    changed <- errorbar_layer_data(restyle_plot(p, errorbar_width = 2))
    expect_equal(changed$xmax - changed$xmin, rep(2, nrow(changed)))
  }
})
