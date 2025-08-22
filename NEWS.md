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
