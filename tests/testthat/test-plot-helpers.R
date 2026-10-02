test_that("legend order follows factor levels, not row order", {
  set.seed(1)
  lv <- c("2.4 mg/kg", "4.8 mg/kg", "7.2 mg/kg", "10 mg/kg", "12 mg/kg")
  d <- data.frame(
    CONC = runif(500, 0, 500),
    dHR = rnorm(500, 0, 10),
    DOSEC = factor(sample(lv, 500, TRUE), levels = lv)
  )
  d <- d[order(runif(nrow(d))), ]

  p <- eda_scatter_with_regressions(
    d,
    dHR,
    CONC,
    trt_col = DOSEC,
    loess_line = FALSE
  )

  expect_equal(
    as.character(ggplot2::get_guide_data(p, "colour")$.label),
    lv
  )
})

test_that("add_horizontal_references is deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  p <- eda_scatter_with_regressions(cqtkit_data_verapamil, deltaQTCF, CONC, TRTG)

  expect_warning(
    add_horizontal_references(p, 10),
    "`add_horizontal_references\\(\\)` was deprecated in cqtkit 1.2.1"
  )
})

test_that("add_horizontal_references lines are black after style_plot()", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p <- suppressWarnings(add_horizontal_references(p, 10))
  reference_colour <- function(p) {
    unique(ggplot2::ggplot_build(p)$data[[2]]$colour)
  }

  styled <- suppressWarnings(style_plot(p))
  expect_identical(reference_colour(styled), "black")

  styled <- suppressWarnings(style_plot(p, colors = c("Reference 10" = "red")))
  expect_identical(reference_colour(styled), "red")
})

test_that("style_plot() recolours a named line without changing the plot passed in", {
  p <- eda_scatter_with_regressions(
    cqtkit_data_verapamil,
    deltaQTCF,
    CONC,
    TRTG,
    reference_threshold = 10
  )
  reference_colour <- function(p) {
    unique(ggplot2::ggplot_build(p)$data[[length(p$layers)]]$colour)
  }
  before <- reference_colour(p)

  styled <- suppressWarnings(style_plot(p, colors = c("Reference 10" = "red")))

  expect_identical(reference_colour(styled), "red")
  expect_identical(reference_colour(p), before)
})
