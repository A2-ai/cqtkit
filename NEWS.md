# cqtkit 1.2.1

### Enhanced
* `gof_residuals_plots()` gains `loess_line`, which adds a "LOESS Regression" line to each panel. Defaults to `FALSE`.
* `tabulate_ecg_param_summary()` gains `footnote_missing`, which footnotes cells left empty by fewer than 2 observations or by a time point with no `reference_dose` observations. Defaults to `TRUE`.

### Deprecated
* `add_horizontal_references()` is deprecated and draws the same lines as in 1.1.0. Use the `reference_threshold` argument of the plotting functions.
* The `legend_location` argument of `eda_qtc_comparison_plot()`, `gof_plots()`, `gof_concordance_plots()`, `gof_residuals_plots()`, `gof_qq_plots()`, `gof_residuals_time_boxplots()` and `gof_residuals_trt_boxplots()` is deprecated. Set `legend.position` in `style_spec()` instead. It will be removed in 2.0.0.
* `compute_loess_linear_r_squared()` is deprecated.

### Fixed
* `compute_loess_linear_r_squared()` regresses `deltaqtc_col` on `conc_col`.
* `compute_potential_hysteresis()` errors name the group when `group_col` holds more than one group or a group has fewer than 4 time points.
  * `compute_hysteresis_labeller()` and `eda_hysteresis_loop_plot()` name every group with fewer than 4 time points in one error.
  * `compute_potential_hysteresis()` and `eda_hysteresis_loop_plot()` error, naming the time points, when a time point's mean `deltaqtc_col` is NA, such as a time point with no `reference_dose` observations.
* Reference lines, regression lines and `gof_vpc_plot()` percentile lines are named lines in the linetype legend, placed after the group legend. `reveal()` onto shapes gives a legend of the revealed column only, and `colors` and `linetypes` restyle each line by name, for example `"Reference 10"`.
  * `eda_scatter_with_regressions()` draws the linear regression solid and the LOESS regression short-dashed.
  * The linetype legend has no title. Legends placed side by side are bottom-aligned unless the theme sets `legend.box.just`.
  * The regression, density and identity lines of `eda_qt_rr_plot()`, `eda_quantiles_plot()` and the `gof_*()` plots are named lines.
* `predict_with_observations_plot()` and `predict_with_quantiles_plot()` show predictions, observations and reference lines in one legend. Their points are shape 16, and a style list's `shapes` sets them.
* On the `style_spec()` path, points map colour only and are shape 16; set it with `style_spec(point_shape = )`.
* `restyle_plot()` sets `title`, `xlabel`, `ylabel`, `xlims`, `ylims` and `fill_alpha` on `style_spec()` plots.
* A `style_spec()` title on `eda_qtc_comparison_plot()` is the figure title. The panels keep their column-name titles.
* `style_spec(legend.position = )` places the combined legend of `eda_qtc_comparison_plot()` and the multi-panel `gof_*()` functions.
* `eda_hysteresis_loop_plot()` works with `restyle_plot()` and `reveal()`, and keeps its facets when combined.
* `gof_residuals_trt_boxplots()` shows its reference-line legend when `trt_col` is not supplied.
* A style list that sets `color_order` without `shape_order` gives the shape legend the same order, so treatment keeps one legend.
* `compute_grouped_mean_sd()` and `compute_pk_parameters()` warn that TIME or DOSE data contains NA values.
* `compute_grouped_mean_sd()` gives NA differences at time points with no `reference_dose` observations, with a warning naming them, instead of an error. A group with fewer than 2 observations has NA confidence intervals, with a warning naming it.
  * `compute_ecg_param_summary()`, `tabulate_ecg_param_summary()` and `eda_mean_dv_over_time()` give each of these warnings once.
* `compute_contrast_observations()` with `contrast_method = "group"` warns that observed data contained NA whether the missing value is in the treatment or the control group.
* `predict_with_exposure_plot()` shows reference and Cmax lines in one "Exposure" legend. `colors` recolours a Cmax line by name, for example `"Cmax_113.59"`.
* Requires ggstylekit >= 0.4.1. `style_spec(caption = ...)` overrides generated plot captions, including exposure predictions; `caption = ""` hides them.
* `eda_scatter_with_regressions()`, the `gof_*()` residual plots and `predict_with_observations_plot()` keep the remaining columns of `data` in the plot data, so `reveal()` can map a covariate on them.
  * `compute_fit_results()` and `compute_contrast_observations()` return an ungrouped tibble with the remaining columns of `data` after their computed columns.
  * `compute_fit_results()` warns when its `TRTG` column overwrites a `TRTG` column in `data`, which happens when `trt_col` is not `TRTG`.

# cqtkit 1.2.0

* cqtkit requires R >= 4.1.0, up from 3.5.0. The package now uses the base pipe `|>` internally.

### Enhanced
* `gof_vpc_plot()` and `compute_summary_statistics_of_simulations()` gain a `seed` argument for reproducible simulations. The caller's RNG state is restored on exit.
* `fit_prespecified_model()` warns when a treatment or time predictor loses a level because rows with missing model values are dropped, naming the level and the column that was missing.
* `compute_model_fit_parameters()` and `tabulate_model_fit_parameters()` gain `section`, which classifies each parameter as Slope, Treatment, Intercept, Time or Random Effects and orders the rows by it. `tabulate_model_fit_parameters()` renders those as row groups. Defaults to `FALSE`.
* `compute_model_fit_parameters()` and `tabulate_model_fit_parameters()` gain `include_reference_levels`, which adds a zero-valued row for the reference level of the treatment and time terms. Defaults to `FALSE`.
* `compute_model_fit_parameters()` and `tabulate_model_fit_parameters()` gain `conc_col_name` and `baseline_col_name`, used to recognise those terms when `section = TRUE`.
* New `compute_high_qtc_subjects()` and `tabulate_high_qtc_subjects()` count the number of distinct subjects with at least one value above each threshold.
* New `compute_high_qtc_observations()` and `tabulate_high_qtc_observations()` count observations above each threshold, matching the existing `*_sub()` behaviour.
* The new `*_high_qtc_subjects()` and `*_high_qtc_observations()` functions take `qtc_thresholds` and `dqtc_thresholds`, so the thresholds are not fixed at 450/480/500 ms and 30/60 ms. `compute_high_qtc_sub()` and `tabulate_high_qtc_sub()` keep the fixed thresholds.
* C-QT dataset assembly gains functions for the derivations that must happen before replicates are averaged.
  * New `compute_hr()` derives `HR` and `HRBL` from an RR column.
  * New `compute_blm()` computes a population mean baseline from replicate-level baseline ECG data, averaging within each `group_col` group and then across groups.
  * New `vignette("data-assembly")` documents how the bundled datasets are assembled from the source trial data.
* All sixteen plotting functions accept a ggstylekit `style_spec()` as `style`, alongside the existing style list. The two are told apart by class, so an untouched call keeps the list engine and the figure it produced before.
  * cqtkit re-exports `style_spec()`, `legend_spec()`, `reveal()` and `restyle_plot()` from ggstylekit. Everything else in ggstylekit is reached as `ggstylekit::`.
  * Do not call `library(ggstylekit)` while both styling APIs exist. Both packages export `set_style` and `style_plot`, they are unrelated functions, and whichever package is attached second wins.
  * Plots styled with a `style_spec()` can be adjusted afterwards with `restyle_plot()` and inspected with `reveal()`. Multi-panel plots on this path are patchworks so their panels remain editable. Plots styled with a list cannot.
  * Palette functions supplied to `style_spec(colors = )` now also work for plots with mapped fills, and explicit shape legend settings are preserved.
  * New `vignette("styling")` maps every `set_style()` argument to its `style_spec()` or `legend_spec()` equivalent.
* `eda_qt_rr_plot()` and `eda_qtc_comparison_plot()` gain arguments for showing the slope p-value in the caption.
  * `include_pvalue` adds the p-value. Defaults to `FALSE`, and warns when `show_model_results = FALSE`.
  * `scientific` shows it in scientific notation. Defaults to `TRUE`, matching `tabulate_model_fit_parameters()`.
  * `pvalue_eps` is the cutoff below which it prints as `< eps` when `scientific = FALSE`. Defaults to `NULL`, no cutoff.
  * `decimals` sets the decimal places in the caption. Defaults to `NULL`, which rounds to three decimals as before.

### Deprecated
* `set_style()` and `style_plot()` are deprecated in favour of `style_spec()` and `restyle_plot()`. Both keep working. Passing `style` a list keeps working and does not warn. They will be removed in 2.0.0.
* The `id_col` and `deduplicate` arguments of `preprocess()`, `compute_delta_hrblm()`, `compute_delta_qtcbblm()` and `compute_delta_qtcfblm()` are deprecated. The population baseline mean is now computed by `compute_blm()`. They will be removed in 2.0.0.
* The re-export of magrittr's `%>%` is deprecated. `library(cqtkit)` will no longer attach it in 2.0.0. Attach it with `library(dplyr)` or `library(magrittr)`, or use the base pipe `|>`.
* `compute_high_qtc_sub()` and `tabulate_high_qtc_sub()` are deprecated. They count observations, not subjects. Their counts are unchanged. Use `compute_high_qtc_subjects()` / `tabulate_high_qtc_subjects()` for subject counts or `compute_high_qtc_observations()` / `tabulate_high_qtc_observations()` for observation counts. They will be removed in 2.0.0.

### Fixed
* Summary functions that combine `group_col` with dose or treatment now error if the supplied grouping column contains missing values. Fill or filter those values, or omit `group_col` to summarize by dose or treatment alone. This deliberately replaces the literal `NA` labels produced in 1.1.0 and prevents missing groups from silently merging distinct doses or treatments.
* Model-table sections and reference rows use the original model terms and fitted contrasts, so treatment and time levels with matching display labels remain distinct. These options do not change model estimates.
* Preprocessing no longer derives values from replicate-averaged data. The QT corrections are nonlinear in RR, so applying them to an averaged `QT` and `RR` gives an incorrect `QTCB` or `QTCF`: the correction must be applied to each replicate and the results averaged.
  * `preprocess()` errors when `QTCB`, `QTCF`, `QTCBBL` or `QTCFBL` is missing from `data` instead of deriving it.
  * `compute_delta_hrblm()`, `compute_delta_qtcbblm()` and `compute_delta_qtcfblm()` use the matching `HRBLM`, `QTCBBLM` or `QTCFBLM` column on `data`, and error when it is missing instead of averaging the baseline values already on `data`. To reproduce 1.1.0 deltas, drop those columns and set `options(cqtkit.override_preprocessing_error = TRUE)`.
  * `preprocess()` errors when `HRBLM`, `QTCBBLM` or `QTCFBLM` is missing, and gains `hrblm_col`, `qtcbblm_col` and `qtcfblm_col`.
  * `options(cqtkit.override_preprocessing_error = TRUE)` derives any of those values that are missing from `data` the pre-1.2.0 way, and warns naming each one.
  * The bundled `cqtkit_data_*` and `cqtkit_data_bl_*` datasets are rebuilt, as documented in `vignette("data-assembly")`. Values and row counts change, `ID` is an integer, and the datasets gain the subject covariates `SEX`, `AGE`, `HGHT`, `WGHT`, `RACE` and `ETHNIC`, and `VISIT`.
    * The `cqtkit_data_*` datasets carry `deltaRR`, `deltaHR`, `deltaQT`, `deltaQTCB` and `deltaQTCF`, so `preprocess()` returns them unchanged.
    * The `cqtkit_data_bl_*` datasets carry `QTCB` and `QTCF`.
* `fit_prespecified_model()` errors naming the offending column when a model column name is non-syntactic, instead of failing in `str2lang()`.
* `fit_prespecified_model()` errors naming the column, the levels removed, and the missing values responsible when a treatment or time predictor collapses below two levels once rows with missing model values are dropped, instead of failing in `contrasts<-`.
* `compute_pk_parameters()` now takes one Tmax and Cmax per subject before summarizing, so subjects with more timepoints no longer contribute repeated values to any Tmax or Cmax summary.
* `eda_qt_rr_plot()` reads the caller's `xlabel` from `style$xlabel` rather than the misspelled `style$xlabe`, which resolved only through partial matching.
* Plot legends and their default colours follow the level order of factor grouping columns instead of the order the rows happen to be in. A group's default colour can therefore change where row order and level order differed.
* `tabulate_pk_parameters()` prints numeric dose groups as labels, for example "120", left-aligned.
* `compute_hysteresis_labeller()` accepts a `dosef_col` factor with unused levels.
* Grouping columns returned by `compute_grouped_mean_sd()`, `compute_ecg_param_summary()`, `compute_pk_parameters()` and `compute_study_summary()` are factors whose levels carry their intended display order. This preserves factor treatment and group order, and sorts numeric groups numerically, instead of sorting their labels alphabetically.

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
