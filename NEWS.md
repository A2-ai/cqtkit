# cqtkit 2.0.0

### Breaking Changes
* Plot styling now uses **ggstylekit**. Pass a `ggstylekit::style_spec()` to any plotting function's `style` argument. `set_style()` and `style_plot()` have been removed.
* The `legend_location` argument has been removed from the `eda_*` and `gof_*` plotting functions. Set the legend position with `legend.position` in the `style` instead.
* `preprocess()` now takes `(data, bl_data, by)`, joining baseline values from a separate baseline dataset instead of computing them from an `id_col`.
* `compute_delta_hrblm()`, `compute_delta_qtcbblm()`, and `compute_delta_qtcfblm()` now expect the baseline-mean column to already exist on the data. Add it first with the new `compute_hrblm()`, `compute_qtcbblm()`, or `compute_qtcfblm()`.
* `compute_high_qtc_sub()` and `tabulate_high_qtc_sub()` have been renamed to `compute_high_qtc_obs()` and `tabulate_high_qtc_obs()` respectively, since these functions count observations (rows), not subjects.
* New `compute_high_qtc_sub()` and `tabulate_high_qtc_sub()` functions now count distinct subjects with at least one observation exceeding thresholds. These require an `id_col` argument.
* `eda_qt_rr_plot()` and `eda_qtc_comparison_plot()`: `conf_int` is removed and `show_model_results` now takes a `model_results_spec()`. `TRUE`/`FALSE` still work.

### Enhanced
* All high QTc functions now support configurable `qtc_thresholds` and `dqtc_thresholds` arguments (defaults: `c(450, 480, 500)` and `c(30, 60)`).
* New `compute_hr()` derives `HR`/`HRBL` from RR.
* `gof_vpc_plot()` and `compute_summary_statistics_of_simulations()` gain a `seed` argument.
* New `model_results_spec()` controls the slope, CI level, p-value, and digits in QT vs RR plot captions.
* New `compute_hrblm()`, `compute_qtcbblm()`, and `compute_qtcfblm()` add population baseline-mean columns from a baseline dataset.
* Example datasets now include subject covariates.
* `fit_prespecified_model()` now errors early with an actionable message when a model column has a non-syntactic name, naming the offending columns.
* `fit_prespecified_model()` now checks categorical model columns after rows with missing values are dropped: it errors if a factor collapses to fewer than 2 levels, and warns if a factor loses level(s) but retains 2 or more, naming the columns responsible.
* Legend and grouping order in plots now honors factor level order rather than order of appearance.
* New "Data Assembly" vignette covering how to build an analysis-ready dataset.
* New "Styling" vignette covering plot styling with **ggstylekit**.

### Fixed
* Fixed `compute_pk_parameters()` computing Cmax summary statistics over repeated per-observation rows instead of one Cmax per subject, which biased the geometric mean and other summaries.
* Fixed a `tidyselect` `.data` pronoun deprecation warning.
* Fixed `eda_mean_dv_over_time()`, `compute_study_summary()`, and `compute_pk_parameters()` ordering groups alphabetically instead of by factor level order.

### Migration Guide
* If you were using `compute_high_qtc_sub()` or `tabulate_high_qtc_sub()` for observation-level counts, rename to `compute_high_qtc_obs()` or `tabulate_high_qtc_obs()`.
* If you need subject-level counts (distinct individuals), use the new `compute_high_qtc_sub()` or `tabulate_high_qtc_sub()` with the required `id_col` argument.
* See the new "Data Assembly" vignette for the updated `preprocess()` workflow.

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
