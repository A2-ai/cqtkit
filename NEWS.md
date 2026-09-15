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

### Deprecated
* `compute_high_qtc_sub()` and `tabulate_high_qtc_sub()` are deprecated. Despite their names they count observations, not subjects, so a subject above a threshold at several timepoints is counted several times. They keep returning observation counts and now always warn. Use `compute_high_qtc_subjects()` / `tabulate_high_qtc_subjects()` for subject counts, or `compute_high_qtc_observations()` / `tabulate_high_qtc_observations()` for the existing counts. They will be removed in 2.0.0.

### Fixed
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
