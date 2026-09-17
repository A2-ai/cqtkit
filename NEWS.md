# cqtkit 1.2.0

### Enhanced
* `gof_vpc_plot()` and `compute_summary_statistics_of_simulations()` gain a `seed` argument for reproducible simulations. The caller's RNG state is restored on exit.
* `fit_prespecified_model()` warns when a treatment or time predictor loses a level because rows with missing model values are dropped, naming the level and the column that was missing.
* `compute_model_fit_parameters()` and `tabulate_model_fit_parameters()` gain `section`, which classifies each parameter as Slope, Treatment, Intercept, Time or Random Effects and orders the rows by it. `tabulate_model_fit_parameters()` renders those as row groups. Defaults to `FALSE`.
* `compute_model_fit_parameters()` and `tabulate_model_fit_parameters()` gain `include_reference_levels`, which adds a zero-valued row for the reference level of the treatment and time terms. Defaults to `FALSE`.
* `compute_model_fit_parameters()` and `tabulate_model_fit_parameters()` gain `conc_col_name` and `baseline_col_name`, used to recognise those terms when `section = TRUE`.
* New `compute_high_qtc_subjects()` and `tabulate_high_qtc_subjects()` count the number of distinct subjects with at least one value above each threshold.
* New `compute_high_qtc_observations()` and `tabulate_high_qtc_observations()` count observations above each threshold, matching the existing `*_sub()` behaviour.
* The high QTc functions gain `qtc_thresholds` and `dqtc_thresholds`, so the thresholds are no longer fixed at 450/480/500 ms and 30/60 ms.
* C-QT dataset assembly gains functions for the derivations that must happen before replicates are averaged.
  * New `compute_hr()` derives `HR` and `HRBL` from an RR column.
  * New `compute_blm()` computes a population mean baseline from baseline ECG data, averaging within each `by` group and then across groups.
  * New `vignette("data-assembly")` documents how the bundled datasets are assembled from the source trial data.
* `eda_qt_rr_plot()` and `eda_qtc_comparison_plot()` gain arguments for showing the slope p-value in the caption.
  * `include_pvalue` adds the p-value. Defaults to `FALSE`, and warns when `show_model_results = FALSE`.
  * `scientific` shows it in scientific notation. Defaults to `TRUE`, matching `tabulate_model_fit_parameters()`.
  * `pvalue_eps` is the cutoff below which it prints as `< eps` when `scientific = FALSE`. Defaults to `NULL`, no cutoff.
  * `decimals` sets the decimal places in the caption. Defaults to `NULL`, which rounds to three decimals as before.

### Deprecated
* The `id_col` and `deduplicate` arguments of `preprocess()`, `compute_delta_hrblm()`, `compute_delta_qtcbblm()` and `compute_delta_qtcfblm()` are deprecated. The population baseline mean is now computed by `compute_blm()`. They will be removed in 2.0.0.
* The re-export of magrittr's `%>%` is deprecated. `library(cqtkit)` will no longer attach it in 2.0.0. Attach it with `library(dplyr)` or `library(magrittr)`, or use the base pipe `|>`.
* `compute_high_qtc_sub()` and `tabulate_high_qtc_sub()` are deprecated. They count observations, not subjects. Their counts are unchanged. Use `compute_high_qtc_subjects()` / `tabulate_high_qtc_subjects()` for subject counts or `compute_high_qtc_observations()` / `tabulate_high_qtc_observations()` for observation counts. They will be removed in 2.0.0.

### Fixed
* Preprocessing no longer derives values from replicate-averaged data. The QT corrections are nonlinear in RR, so applying them to an averaged `QT` and `RR` gives an incorrect `QTCB` or `QTCF`: the correction must be applied to each replicate and the results averaged.
  * `preprocess()` errors when `QTCB`, `QTCF`, `QTCBBL` or `QTCFBL` is missing from `data` instead of deriving it.
  * `compute_delta_hrblm()`, `compute_delta_qtcbblm()` and `compute_delta_qtcfblm()` error when the matching `HRBLM`, `QTCBBLM` or `QTCFBLM` column is missing from `data` instead of averaging the baseline values already on `data`.
  * `options(cqtkit.override_preprocessing_error = TRUE)` restores the pre-1.2.0 behaviour of all four functions.
  * The bundled `cqtkit_data_*` and `cqtkit_data_bl_*` datasets are rebuilt, as documented in `vignette("data-assembly")`. Values and row counts change, and the datasets gain subject covariates.
* `fit_prespecified_model()` errors naming the offending column when a model column name is non-syntactic, instead of failing in `str2lang()`.
* `fit_prespecified_model()` errors naming the column, the levels removed, and the missing values responsible when a treatment or time predictor collapses below two levels once rows with missing model values are dropped, instead of failing in `contrasts<-`.
* `compute_pk_parameters()` now takes one Cmax per subject before summarizing, so subjects with more timepoints no longer contribute repeated Cmax values to `Cmax_gm` and `Cmax_cv`.
* Plot legends follow the level order of factor grouping columns instead of the order the rows happen to be in.
* `compute_study_summary()` and `compute_pk_parameters()` keep the level order of factor treatment and group columns in `grouping` instead of sorting them alphabetically.

# cqtkit 1.1.0

### Enhanced
* Added `qtc_label` and `unit` arguments to `tabulate_high_qtc_sub()` for customizable column headers.
* Added `time_label` argument to `tabulate_ecg_param_summary()` for customizable time column header.
* Updated `compute_conc_for_upper_pred()` to support models without treatment group (contrasts between conc = 0 and conc != 0). Now returns a single numeric concentration value instead of a list.

# cqtkit 1.0.2

### Fixed 
* Fixed issue where duplicate conc values were added in `compute_exposure_predictions()` causing `predict_with_exposure_plot` to fail.


# cqtkit 1.0.1

### Fixed
* Exposed the intended optional `legend_location` argument in `gof_residuals_trt_boxplots()`.
* Updated `predict_with_observations_plot` to use `trt_col` to group data allowing for styling.
* Updated `predict_with_observations_plot` and `predict_with_quantiles_plot` to keep default black color
for prediction line when other colors supplied.
* Updated `eda_quantiles_plot` with `plot_observations` to add individual data points to plot.

# cqtkit 1.0.0

* Initial public release.
