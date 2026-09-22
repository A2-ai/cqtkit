model_data <- cqtkit_data_verapamil %>% preprocess()
fit <- fit_prespecified_model(
  model_data,
  deltaQTCF,
  ID,
  CONC,
  deltaQTCFBL,
  TRTG,
  TAFD,
  remove_conc_iiv = TRUE
)

test_that("defaults leave the parameter table unchanged", {
  params <- compute_model_fit_parameters(fit, conf_int = 0.9)

  expect_false("Section" %in% names(params))
  expect_equal(params$Parameters[1:3], c("Intercept", "CONC", "deltaQTCFBL"))
  expect_false(any(grepl("Reference", params$Parameters)))
})

test_that("section adds a Section column and orders the rows by it", {
  params <- compute_model_fit_parameters(fit, conf_int = 0.9, section = TRUE)

  expect_true("Section" %in% names(params))
  expect_equal(as.character(params$Section[1]), "Slope")
  expect_equal(params$Parameters[1], "CONC")
  expect_false(is.unsorted(as.integer(params$Section)))
  expect_equal(
    as.character(unique(params$Section)),
    c("Slope", "Treatment", "Intercept", "Time", "Random Effects")
  )
})

test_that("section does not add or drop rows", {
  plain <- compute_model_fit_parameters(fit, conf_int = 0.9)
  sectioned <- compute_model_fit_parameters(fit, conf_int = 0.9, section = TRUE)

  expect_setequal(plain$Parameters, sectioned$Parameters)
})

test_that("include_reference_levels adds the comparator rows", {
  params <- compute_model_fit_parameters(
    fit,
    conf_int = 0.9,
    include_reference_levels = TRUE
  )

  expect_true("Placebo (Reference)" %in% params$Parameters)
  expect_equal(
    params$Value[params$Parameters == "Placebo (Reference)"],
    0
  )
  expect_true(is.na(params$CIl[params$Parameters == "Placebo (Reference)"]))
  expect_false("Section" %in% names(params))
})

test_that("reference rows are classified into their term's section", {
  params <- compute_model_fit_parameters(
    fit,
    conf_int = 0.9,
    section = TRUE,
    include_reference_levels = TRUE
  )

  expect_equal(
    as.character(params$Section[params$Parameters == "Placebo (Reference)"]),
    "Treatment"
  )
})

test_that("tabulate_model_fit_parameters groups rows only when asked", {
  plain <- tabulate_model_fit_parameters(fit, conf_int = 0.9)
  sectioned <- tabulate_model_fit_parameters(
    fit,
    conf_int = 0.9,
    section = TRUE
  )

  expect_length(plain[["_row_groups"]], 0)
  expect_equal(
    sectioned[["_row_groups"]],
    c("Slope", "Treatment", "Intercept", "Time", "Random Effects")
  )
})

test_that("overlapping treatment and time labels retain their model terms", {
  dat <- model_data
  dat$TRTG <- factor(dat$TRTG, labels = c("0", "1"))
  dat$TAFD <- factor(
    dat$TAFD, levels = unique(dat$TAFD),
    labels = seq_along(unique(dat$TAFD)) - 1
  )
  mod <- fit_prespecified_model(
    dat, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    remove_conc_iiv = TRUE
  )
  params <- compute_model_fit_parameters(
    mod, section = TRUE, include_reference_levels = TRUE
  )
  refs <- params[params$Parameters == "0 (Reference)", ]
  expect_equal(as.character(refs$Section), c("Treatment", "Time"))
  expect_equal(refs$Value, c(0, 0))
  expect_equal(sum(params$Section == "Treatment"), 2)
  expect_equal(sum(params$Section == "Time"), nlevels(dat$TAFD))
  expect_false(any(params$Section == "Other"))
  expect_false(".term" %in% names(params))
  estimates <- summary(mod)$tTable[, "Value"]
  expect_equal(params$Value[params$Section == "Treatment" & params$Parameters == "1"],
               unname(estimates["TRTG1"]))
  expect_equal(params$Value[params$Section == "Time" & params$Parameters == "1"],
               unname(estimates["TAFD1"]))
  plain <- compute_model_fit_parameters(mod, include_reference_levels = TRUE)
  expect_equal(sum(plain$Parameters == "0 (Reference)"), 2)
  expect_false(any(c(".term", "Section") %in% names(plain)))
})

test_that("reference rows follow fitted contrasts rather than missing labels", {
  dat <- model_data
  contrasts(dat$TRTG) <- stats::contr.treatment(2, base = 2)
  mod <- fit_prespecified_model(
    dat, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    remove_conc_iiv = TRUE
  )
  params <- compute_model_fit_parameters(mod, include_reference_levels = TRUE)
  expect_true("Verapamil HCL (Reference)" %in% params$Parameters)
  expect_false("Placebo (Reference)" %in% params$Parameters)

  contrasts(dat$TRTG) <- stats::contr.sum(2)
  mod <- fit_prespecified_model(
    dat, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    remove_conc_iiv = TRUE
  )
  params <- compute_model_fit_parameters(mod, section = TRUE, include_reference_levels = TRUE)
  expect_equal(sum(params$Section == "Treatment"), 1)
  expect_false(any(grepl("Reference", params$Parameters[params$Section == "Treatment"])))
})
