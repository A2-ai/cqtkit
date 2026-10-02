test_that("compute_grouped_mean_sd errors when time grouping does not reduce size", {
  .test_data <- cqtkit_data_verapamil %>%
    dplyr::mutate('TIME_UNIQUE' = dplyr::row_number())
  expect_error(
    compute_grouped_mean_sd(
      .test_data,
      QT,
      TIME_UNIQUE,
      DOSE
    ),
    "Grouping by ntime_col does not reduce size"
  )
})

test_that("compute_grouped_mean_sd errors when dose grouping does not reduce size", {
  .test_data <- cqtkit_data_verapamil %>%
    dplyr::mutate('DOSE_UNIQUE' = dplyr::row_number())
  expect_error(
    compute_grouped_mean_sd(
      .test_data,
      QT,
      NTLD,
      DOSE_UNIQUE
    ),
    "Grouping by dosef_col does not reduce size"
  )
})

test_that("compute_grouped_mean_sd means by time and dose, and deltas against the reference", {
  plain <- compute_grouped_mean_sd(cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF)
  ref <- compute_grouped_mean_sd(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    reference_dose = "0 mg"
  )
  expected <- cqtkit_data_verapamil |>
    dplyr::group_by(NTLD, DOSEF) |>
    dplyr::summarise(mean_dv = mean(deltaQTCF), .groups = "drop")
  placebo <- expected$mean_dv[expected$DOSEF == "0 mg"]

  expect_equal(plain$mean_dv, expected$mean_dv)
  expect_equal(
    ref$mean_delta_dv,
    expected$mean_dv - rep(placebo, each = 2)
  )
})

test_that("grouped summaries return ordered factor groups", {
  data <- cqtkit_data_verapamil |> preprocess()

  plain <- compute_grouped_mean_sd(data, deltaQTCF, NTLD, DOSEF)
  grouped <- compute_grouped_mean_sd(data, deltaQTCF, NTLD, DOSEF, TRTG)
  plain_ref <- compute_grouped_mean_sd(
    data,
    deltaQTCF,
    NTLD,
    DOSEF,
    reference_dose = "0 mg"
  )
  grouped_ref <- compute_grouped_mean_sd(
    data,
    deltaQTCF,
    NTLD,
    DOSEF,
    TRTG,
    reference_dose = "0 mg"
  )

  expect_s3_class(plain$group, "factor")
  expect_equal(levels(plain$group), c("0 mg", "120 mg"))
  expect_equal(dplyr::group_vars(plain), c("time", "dose"))

  expected_group_levels <- c("0 mg Placebo", "120 mg Verapamil HCL")
  expect_s3_class(grouped$group, "factor")
  expect_equal(levels(grouped$group), expected_group_levels)
  expect_equal(dplyr::group_vars(grouped), c("time", "dose", "group"))

  expect_s3_class(plain_ref$group, "factor")
  expect_s3_class(grouped_ref$group, "factor")
  expect_equal(dplyr::group_vars(plain_ref), "time")
  expect_equal(dplyr::group_vars(grouped_ref), "time")

  plain_keys <- as.data.frame(plain[c("time", "dose", "group")])
  plain_ref_keys <- as.data.frame(plain_ref[c("time", "dose", "group")])
  grouped_keys <- as.data.frame(grouped[c("time", "dose", "group")])
  grouped_ref_keys <- as.data.frame(grouped_ref[c("time", "dose", "group")])
  expect_identical(plain_keys, plain_ref_keys)
  expect_identical(grouped_keys, grouped_ref_keys)
})

test_that("ECG summaries retain factor group levels", {
  result <- compute_ecg_param_summary(
    cqtkit_data_verapamil |> preprocess(),
    NTLD,
    DOSEF,
    QTCF,
    deltaQTCF
  )

  expect_s3_class(result$group, "factor")
  expect_equal(levels(result$group), c("0 mg", "120 mg"))
})

collect_warnings <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = messages)
}

test_that("compute_grouped_mean_sd gives NA differences where the reference dose is absent", {
  full <- compute_grouped_mean_sd(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    reference_dose = "0 mg"
  )
  .test_data <- cqtkit_data_verapamil |>
    dplyr::filter(!(NTLD == 2 & DOSEF == "0 mg"))

  out <- collect_warnings(compute_grouped_mean_sd(
    .test_data,
    deltaQTCF,
    NTLD,
    DOSEF,
    reference_dose = "0 mg"
  ))

  expect_identical(
    out$warnings,
    "No `reference_dose` (0 mg) observations at: 2. Differences from `reference_dose` are NA there."
  )
  at_2 <- dplyr::filter(out$value, time == 2)
  expect_true(all(is.na(at_2$mean_delta_dv)))
  expect_true(all(is.na(at_2$ci_low_delta)))
  expect_true(all(is.na(at_2$ci_up_delta)))

  expect_equal(
    dplyr::filter(out$value, time != 2)$mean_delta_dv,
    dplyr::filter(full, time != 2)$mean_delta_dv
  )
})

test_that("compute_grouped_mean_sd gives NA CIs for groups with fewer than 2 observations", {
  single <- cqtkit_data_verapamil |>
    dplyr::filter(DOSEF == "120 mg") |>
    dplyr::slice(1) |>
    dplyr::mutate(NTLD = 99)
  .test_data <- dplyr::bind_rows(cqtkit_data_verapamil, single)

  out <- collect_warnings(
    compute_grouped_mean_sd(.test_data, deltaQTCF, NTLD, DOSEF)
  )

  expect_identical(
    out$warnings,
    "Fewer than 2 observations at: 99 / 120 mg. Their CIs are NA."
  )
  at_99 <- dplyr::filter(out$value, time == 99)
  expect_equal(at_99$n, 1L)
  expect_equal(at_99$mean_dv, single$deltaQTCF)
  expect_true(is.na(at_99$ci_low))
  expect_true(is.na(at_99$ci_high))
})

test_that("compute_ecg_param_summary gives each warning once", {
  single <- cqtkit_data_verapamil |>
    dplyr::filter(DOSEF == "120 mg") |>
    dplyr::slice(1) |>
    dplyr::mutate(NTLD = 99)
  .test_data <- dplyr::bind_rows(cqtkit_data_verapamil, single)

  out <- collect_warnings(compute_ecg_param_summary(
    .test_data,
    NTLD,
    DOSEF,
    QTCF,
    deltaQTCF,
    reference_dose = "0 mg"
  ))

  expect_identical(
    out$warnings,
    c(
      "Fewer than 2 observations at: 99 / 120 mg. Their CIs are NA.",
      "No `reference_dose` (0 mg) observations at: 99. Differences from `reference_dose` are NA there."
    )
  )
})

study_data <- function() {
  cqtkit_data_verapamil |>
    dplyr::mutate(
      STUDY = ifelse(dplyr::dense_rank(ID) %% 2 == 0, "A", "B")
    )
}

test_that("compute_grouped_mean_sd matches a study's reference dose by hand", {
  .test_data <- study_data()

  out <- dplyr::ungroup(compute_grouped_mean_sd(
    .test_data,
    deltaQTCF,
    NTLD,
    DOSEF,
    STUDY,
    reference_dose = "0 mg"
  ))
  expected <- .test_data |>
    dplyr::group_by(NTLD, STUDY, DOSEF) |>
    dplyr::summarise(mean_dv = mean(deltaQTCF), .groups = "drop_last") |>
    dplyr::mutate(mean_delta_dv = mean_dv - mean_dv[DOSEF == "0 mg"]) |>
    dplyr::ungroup() |>
    dplyr::mutate(group = paste(DOSEF, STUDY))
  matched <- dplyr::left_join(
    dplyr::mutate(out, group = as.character(group)),
    expected,
    by = c(time = "NTLD", group = "group"),
    suffix = c("", ".expected")
  )

  expect_equal(matched$mean_delta_dv, matched$mean_delta_dv.expected)
  expect_false(any(c(".group", ".reference_n", ".reference_by_group") %in% names(out)))
})

test_that("compute_grouped_mean_sd gives NA where a study has no reference dose at a time point", {
  .test_data <- study_data()
  full <- compute_grouped_mean_sd(
    .test_data,
    deltaQTCF,
    NTLD,
    DOSEF,
    STUDY,
    reference_dose = "0 mg"
  )
  .test_data <- dplyr::filter(
    .test_data,
    !(NTLD == 2 & DOSEF == "0 mg" & STUDY == "B")
  )

  out <- collect_warnings(compute_grouped_mean_sd(
    .test_data,
    deltaQTCF,
    NTLD,
    DOSEF,
    STUDY,
    reference_dose = "0 mg"
  ))

  expect_identical(
    out$warnings,
    "No `reference_dose` (0 mg) observations at time / group: 2 / B. Differences from `reference_dose` are NA there."
  )
  b_at_2 <- dplyr::filter(out$value, time == 2, group == "120 mg B")
  expect_true(is.na(b_at_2$mean_delta_dv))
  expect_true(is.na(b_at_2$ci_low_delta))
  expect_true(is.na(b_at_2$ci_up_delta))

  a_at_2 <- dplyr::filter(out$value, time == 2, group == "120 mg A")
  expect_equal(
    a_at_2$mean_delta_dv,
    dplyr::filter(full, time == 2, group == "120 mg A")$mean_delta_dv
  )
})

test_that("compute_grouped_mean_sd gives NA where a study has no reference dose", {
  .test_data <- study_data() |>
    dplyr::filter(!(DOSEF == "0 mg" & STUDY == "B"))
  times <- sort(unique(.test_data$NTLD))

  out <- collect_warnings(compute_grouped_mean_sd(
    .test_data,
    deltaQTCF,
    NTLD,
    DOSEF,
    STUDY,
    reference_dose = "0 mg"
  ))

  expect_identical(
    out$warnings,
    paste0(
      "No `reference_dose` (0 mg) observations at time / group: ",
      paste(times, "B", sep = " / ", collapse = ", "),
      ". Differences from `reference_dose` are NA there."
    )
  )
  b <- dplyr::filter(out$value, group == "120 mg B")
  expect_equal(nrow(b), length(times))
  expect_true(all(is.na(b$mean_delta_dv)))
  expect_true(all(is.na(b$ci_low_delta)))
  a <- dplyr::filter(out$value, group == "120 mg A")
  expect_false(anyNA(a$mean_delta_dv))
})

test_that("compute_grouped_mean_sd shares the reference dose when each dose has one group", {
  plain <- compute_grouped_mean_sd(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    reference_dose = "0 mg"
  )
  grouped <- compute_grouped_mean_sd(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    TRTG,
    reference_dose = "0 mg"
  )

  delta_cols <- c(
    "mean_delta_dv",
    "geomean_delta_dv",
    "delta_sd",
    "delta_se",
    "ci_low_delta",
    "ci_up_delta"
  )
  expect_equal(
    as.data.frame(dplyr::ungroup(grouped)[delta_cols]),
    as.data.frame(dplyr::ungroup(plain)[delta_cols])
  )
})

test_that("compute_ecg_param_summary keeps groups apart where the dose is missing", {
  .test_data <- cqtkit_data_verapamil |>
    dplyr::mutate(
      GRP = ifelse(as.integer(factor(ID)) %% 2 == 0, "F", "M"),
      DOSEF = dplyr::if_else(NTLD == 2 & DOSEF == "120 mg", NA, DOSEF)
    )

  out <- suppressWarnings(compute_ecg_param_summary(
    .test_data,
    NTLD,
    DOSEF,
    QTCF,
    deltaQTCF,
    GRP
  ))
  at_2 <- dplyr::filter(out, time == 2)
  expected <- .test_data |>
    dplyr::filter(NTLD == 2) |>
    dplyr::group_by(DOSEF, GRP) |>
    dplyr::summarise(
      mean_ecg = mean(QTCF),
      mean_decg = mean(deltaQTCF),
      .groups = "drop"
    )

  expect_identical(
    as.character(at_2$group),
    c("0 mg F", "0 mg M", "NA F", "NA M")
  )
  expect_equal(at_2$mean_ecg, expected$mean_ecg)
  expect_equal(at_2$mean_decg, expected$mean_decg)
})
