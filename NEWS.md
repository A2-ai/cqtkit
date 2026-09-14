# cqtkit 2.0.0

### Breaking Changes
* `preprocess()` now takes `(data, bl_data, by)`. The population baseline means (`HRBLM`, `QTCBBLM`, `QTCFBLM`) are computed from a separate baseline dataset as the mean of per-`by` means instead of from `id_col`, and `preprocess()` no longer derives `QTCB`/`QTCF` when they are absent; compute those at assembly time with `compute_qtcb_qtcf()`. On `cqtkit_data_verapamil` the baseline means change by less than 0.01 ms. See `vignette("data-assembly")`.
* `compute_delta_hrblm()`, `compute_delta_qtcbblm()`, and `compute_delta_qtcfblm()` no longer compute the population baseline mean. Add it first with `compute_hrblm()`, `compute_qtcbblm()`, or `compute_qtcfblm()`.
* `compute_high_qtc_sub()` and `tabulate_high_qtc_sub()` now count subjects rather than observations, and require both `id_col` and `group_col` (pass `group_col = NULL` for an ungrouped total). Use the new `compute_high_qtc_obs()` and `tabulate_high_qtc_obs()` for observation counts.
* Plots are styled with [ggstylekit](https://github.com/a2-ai/ggstylekit) through the `style` argument. `set_style()` and `style_plot()` are removed in favour of `style_spec()` and `restyle_plot()`, and passing a plain list to `style` is an error. `style_spec()`, `legend_spec()`, `reveal()`, and `restyle_plot()` are re-exported so ggstylekit need not be attached. See `vignette("styling")`.

### Deprecations
* `legend_location` is deprecated in favour of `style_spec(legend.position = )`.
* `conf_int` in `eda_qt_rr_plot()` and `eda_qtc_comparison_plot()` is deprecated in favour of `show_model_results = model_results_spec(ci = )`.

### Enhanced
* New `model_results_spec()` controls the slope, CI, p-value, and digits in QT vs RR plot captions.
* `gof_vpc_plot()` and `compute_summary_statistics_of_simulations()` gain a `seed` argument.
* New `compute_hr()`, `compute_hrblm()`, `compute_qtcbblm()`, and `compute_qtcfblm()` for dataset assembly.
* High QTc functions gain `qtc_thresholds` and `dqtc_thresholds` arguments.
* `compute_model_fit_parameters()` and `tabulate_model_fit_parameters()` gain `conc_col_name`, `baseline_col_name`, and `include_reference_levels`.
* `fit_prespecified_model()` errors on non-syntactic column names and on factors that collapse to one level after missing rows are dropped, and warns when a factor loses levels.
* Plot legends and groups follow factor level order.
* Example datasets include subject covariates and `VISIT`. `cqtkit_data_verapamil` gains subject 1005's verapamil period (14 rows), which was missing.
* New "Data Assembly" and "Styling" vignettes.

### Fixed
* `compute_pk_parameters()` computed Cmax summaries over repeated rows instead of one Cmax per subject.
* `eda_mean_dv_over_time()`, `compute_study_summary()`, and `compute_pk_parameters()` ordered groups alphabetically instead of by factor level.

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
