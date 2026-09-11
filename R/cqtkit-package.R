#' cqtkit: Comprehensive C-QT Analysis From Data to Deliverables
#'
#' An end-to-end toolkit for concentration-QT (C-QT) analysis in clinical
#' pharmacology. Provides functions for ECG data preprocessing, exploratory
#' data analysis, statistical modeling, goodness-of-fit evaluation, and
#' exposure-response predictions. Generates publication-ready tables and
#' figures for regulatory submissions.
#'
#' cqtkit has seven main classes of functions for analyzing C-QT data:
#'
#' @section Preprocessing:
#' These functions compute QT corrections and baseline means:
#' \itemize{
#'   \item \code{\link{preprocess}}: Main preprocessing function that computes
#'     QTc corrections and baseline-corrected parameters
#'   \item \code{\link{compute_hr}}: Derive HR and baseline HR from RR
#'     intervals
#'   \item \code{\link{compute_qtcb_qtcf}}: Compute Bazett's and Fridericia's
#'     QT corrections
#'   \item \code{\link{compute_hrblm}}: Add population baseline-mean HR from a
#'     baseline dataset
#'   \item \code{\link{compute_qtcbblm}}: Add population baseline-mean QTcB
#'     from a baseline dataset
#'   \item \code{\link{compute_qtcfblm}}: Add population baseline-mean QTcF
#'     from a baseline dataset
#'   \item \code{\link{compute_deltas}}: Compute change from baseline for ECG
#'     parameters
#'   \item \code{\link{compute_delta_qtcfblm}}: Compute mean baseline QTcF
#'     difference
#'   \item \code{\link{compute_delta_qtcbblm}}: Compute mean baseline QTcB
#'     difference
#'   \item \code{\link{compute_delta_hrblm}}: Compute mean baseline HR
#'     difference
#' }
#'
#' @section EDA:
#' These functions generate exploratory data analysis plots:
#' \itemize{
#'   \item \code{\link{eda_scatter_with_regressions}}: Scatter plot with linear
#'     and loess regression lines
#'   \item \code{\link{eda_qt_rr_plot}}: QT vs RR relationship plot
#'   \item \code{\link{model_results_spec}}: Control the slope, CI, p-value, and
#'     digits reported in QT vs RR plot captions
#'   \item \code{\link{eda_qtc_comparison_plot}}: Compare QTcB and QTcF
#'     corrections
#'   \item \code{\link{eda_mean_dv_over_time}}: Mean dependent variable over
#'     time
#'   \item \code{\link{eda_quantiles_plot}}: Quantile-based visualization of
#'     C-QT data
#'   \item \code{\link{eda_hysteresis_loop_plot}}: Detect and visualize
#'     hysteresis
#' }
#'
#' @section Compute:
#' These functions compute various aspects of QT analyses:
#' \itemize{
#'   \item \code{\link{compute_grouped_mean_sd}}: Compute grouped means and
#'     standard deviations
#'   \item \code{\link{compute_ecg_param_summary}}: Summarize ECG parameters
#'   \item \code{\link{compute_pk_parameters}}: Compute PK parameters (Cmax,
#'     Tmax, etc.)
#'   \item \code{\link{compute_high_qtc_obs}}: Count observations exceeding QTc
#'     thresholds
#'   \item \code{\link{compute_high_qtc_sub}}: Count subjects with at least one
#'     observation exceeding QTc thresholds
#'   \item \code{\link{compute_study_summary}}: Summarize study population
#'   \item \code{\link{compute_fit_results}}: Extract model fit results
#'   \item \code{\link{compute_model_fit_parameters}}: Extract model parameters
#'   \item \code{\link{compute_lm_fit_df}}: Compute linear model fit data frame
#'   \item \code{\link{compute_lme_slope_df}}: Extract slope from mixed-effects
#'     model
#'   \item \code{\link{compute_loess_linear_r_squared}}: Compare loess vs linear
#'     fit
#'   \item \code{\link{compute_quantiles_obs_df}}: Compute quantile-based
#'     summary
#'   \item \code{\link{compute_potential_hysteresis}}: Detect potential
#'     hysteresis
#'   \item \code{\link{compute_hysteresis_labeller}}: Label hysteresis phases
#'   \item \code{\link{compute_enGRI}}: Compute generalized R-squared index
#'   \item \code{\link{compute_contrast_observations}}: Compute model contrasts
#'   \item \code{\link{compute_dataset_simulation}}: Simulate datasets for VPC
#'   \item \code{\link{compute_summary_statistics_of_simulations}}: Summarize
#'     simulation results
#'   \item \code{\link{compute_conc_for_upper_pred}}: Find concentration for
#'     upper prediction bound
#'   \item \code{\link{compute_exposure_predictions}}: Compute QTc predictions
#'     at specified concentrations
#' }
#'
#' @section Tabulate:
#' These functions generate summary and analysis tables:
#' \itemize{
#'   \item \code{\link{tabulate_study_summary}}: Study population summary table
#'   \item \code{\link{tabulate_ecg_param_summary}}: ECG parameter summary table
#'   \item \code{\link{tabulate_pk_parameters}}: PK parameter summary table
#'   \item \code{\link{tabulate_high_qtc_obs}}: High QTc observation counts
#'     table
#'   \item \code{\link{tabulate_high_qtc_sub}}: High QTc subject counts table
#'   \item \code{\link{tabulate_model_fit_parameters}}: Model parameter
#'     estimates table
#'   \item \code{\link{tabulate_exposure_predictions}}: Exposure-response
#'     predictions table
#' }
#'
#' @section Fit:
#' These functions generate the prespecified linear mixed effects model:
#' \itemize{
#'   \item \code{\link{fit_prespecified_model}}: Fit the prespecified linear
#'     mixed-effects model per ICH E14/S7B guidelines
#'   \item \code{\link{fit_qtc_linear_model}}: Fit simple linear model for QTc
#' }
#'
#' @section GoF:
#' These functions generate goodness-of-fit plots of a fitted model:
#' \itemize{
#'   \item \code{\link{gof_plots}}: Generate standard goodness-of-fit plots
#'   \item \code{\link{gof_residuals_plots}}: Residual diagnostic plots
#'   \item \code{\link{gof_residuals_trt_boxplots}}: Residuals by treatment
#'     group
#'   \item \code{\link{gof_residuals_time_boxplots}}: Residuals over time
#'   \item \code{\link{gof_qq_plots}}: Q-Q plots for normality assessment
#'   \item \code{\link{gof_concordance_plots}}: Observed vs predicted plots
#'   \item \code{\link{gof_vpc_plot}}: Visual predictive check plot
#' }
#'
#' @section Predict:
#' These functions generate prediction plots:
#' \itemize{
#'   \item \code{\link{predict_with_observations_plot}}: Plot predictions with
#'     observed data
#'   \item \code{\link{predict_with_quantiles_plot}}: Plot predictions with
#'     quantile summaries
#'   \item \code{\link{predict_with_exposure_plot}}: Plot predictions at
#'     therapeutic exposures
#' }
#'
#' cqtkit also has plot manipulation functions:
#'
#' @section Style:
#' Plots are styled with \pkg{ggstylekit}. Pass a
#' \code{ggstylekit::style_spec()} to any plotting function's \code{style}
#' argument. This helper adds reference lines:
#' \itemize{
#'   \item \code{\link{add_horizontal_references}}: Add reference lines to plots
#' }
#'
#' cqtkit also has included datasets:
#'
#' @section Datasets:
#' Baseline and on-treatment datasets ready for use with cqtkit. Example
#' datasets from Johannesen et al. demonstrating a range of QTc effects:
#' \itemize{
#'   \item \code{\link{cqtkit_data_verapamil}}: Verapamil on-treatment data
#'     (~8 ms QTc effect)
#'   \item \code{\link{cqtkit_data_bl_verapamil}}: Verapamil baseline data
#'   \item \code{\link{cqtkit_data_dofetilide}}: Dofetilide on-treatment data
#'     (~38 ms QTc effect)
#'   \item \code{\link{cqtkit_data_bl_dofetilide}}: Dofetilide baseline data
#'   \item \code{\link{cqtkit_data_ranolazine}}: Ranolazine on-treatment data
#'     (~14 ms QTc effect)
#'   \item \code{\link{cqtkit_data_bl_ranolazine}}: Ranolazine baseline data
#'   \item \code{\link{cqtkit_data_quinidine}}: Quinidine on-treatment data
#'     (~50 ms QTc effect)
#'   \item \code{\link{cqtkit_data_bl_quinidine}}: Quinidine baseline data
#' }
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data %||% :=
NULL


#' cqtkit Preprocessing functions
#'
#' Overview of data preprocessing function in \pkg{cqtkit}
#'
#' @section preprocessing:
#' \itemize{
#'	\item \code{\link{compute_hr}} - This function derives HR and baseline HR from RR intervals
#'	\item \code{\link{compute_qtcb_qtcf}} - This function computes Bazette’s and Fridericia’s QT correction
#'	\item \code{\link{compute_hrblm}} - This function adds the population baseline-mean HR from a baseline dataset
#'	\item \code{\link{compute_qtcbblm}} - This function adds the population baseline-mean QTcB from a baseline dataset
#'	\item \code{\link{compute_qtcfblm}} - This function adds the population baseline-mean QTcF from a baseline dataset
#'	\item \code{\link{compute_delta_qtcbblm}} - This function computes difference between QTcB and mean baseline QTcB
#'	\item \code{\link{compute_delta_qtcfblm}} - This function computes difference between QTcF and mean baseline QTcF
#'	\item \code{\link{compute_delta_hrblm}} - This function computes difference between HR and mean baseline HR
#'	\item \code{\link{compute_deltas}} - This function computes delta ECG parameters between baseline and on treatment
#'	\item \code{\link{preprocess}} - This is a wrapper function to compute all QT corrections, delta ECG, and delta Baseline Mean values
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name preprocessing
#' @rdname cqtkit-preprocessing
NULL

#' cqtkit Exploratory Data Analysis (EDA)
#'
#' There are several eda_ functions that generate various EDA plots for a C-QTc analysis.
#'
#' @section EDA:
#' \itemize{
#' 	\item \code{\link{eda_mean_dv_over_time}} - This function generates a plot of a dependent variable (dQTc, dHR, Concentration) averaged across all subjects over time.
#' 	\item \code{\link{eda_qtc_comparison_plot}} - This function generates a plot of the baseline (pre-dose unaveraged QT and RR values) that compares the various corrections used (QTcF, QTcB, QTcP).
#' 	\item \code{\link{eda_hysteresis_loop_plot}} - This function generates a plot of averaged dQTc and average Concentration aranged by time.
#' 	\item \code{\link{eda_scatter_with_regressions}} - This function generates a scatter plot of dQTc values plotted against time-matched concentrations overlaid with linear and LOESS regression lines.
#' 	\item \code{\link{eda_quantiles_plot}} - This function generates a plot of binned dependent variables (Concentration) with mean independent variabl (QTc) at each bin with a linear regression line.
#' 	\item \code{\link{eda_qt_rr_plot}} - This function generates a scatter plot of QT vs RR with a linear and LOESS regression line. (It generates a single panel of the eda_qtc_comparison_plot figure)
#' 	\item \code{\link{model_results_spec}} - This function builds the spec controlling the slope, CI, p-value, and digits shown in QT vs RR plot captions.
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name eda
#' @rdname cqtkit-eda
NULL

#' cqtkit Computation Functions
#'
#' These functions return tibbles of various computations done for C-QT analyses.
#'
#' @section compute:
#' \itemize{
#' 	\item \code{\link{compute_grouped_mean_sd}} - This function computes averaged dependent variable grouped by time.
#' 	\item \code{\link{compute_pk_parameters}} - This function computes pharmacokinetic parameters.
#' 	\item \code{\link{compute_ecg_param_summary}} - This function computes summary stastics of ECG parameters.
#' 	\item \code{\link{compute_high_qtc_obs}} - This function computes the number of observations with high QTc values.
#' 	\item \code{\link{compute_high_qtc_sub}} - This function computes the number of subjects with at least one high QTc observation.
#' 	\item \code{\link{compute_quantiles_obs_df}} - This function computes the average dependent variable within bins of independent variable.
#' 	\item \code{\link{compute_potential_hysteresis}} - This function computes if hysteresis is detected within a dose group.
#' 	\item \code{\link{compute_hysteresis_labeller}} - This function computes a labeller function to show potential hysteresis results in facet labels.
#' 	\item \code{\link{compute_contrast_observations}} - This funcction computes observations of a dependent variable optionally, contrasted with a control group
#' 	\item \code{\link{compute_exposure_predictions}} - This function computes predicted effect at drug exposure of interest.
#' 	\item \code{\link{compute_dataset_simulation}} - This function computes a simulation of dataset with fitted model.
#' 	\item \code{\link{compute_summary_statistics_of_simulations}} - This function computes the summary statistics of a simulated dataset.
#' 	\item \code{\link{compute_lm_fit_df}} - This function computes extracts the fitted parameters from a linear regression model.
#' 	\item \code{\link{compute_enGRI}} - This function computes the enGRI score for detecting hysteresis.
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name compute
#' @rdname cqtkit-compute
NULL

#' cqtkit Table Functions
#'
#' These functions generate gt tables of various analyses.
#'
#' @section tabulate:
#' \itemize{
#' 	\item \code{\link{tabulate_study_summary}} - This function generates a table of the C-QT study status.
#' 	\item \code{\link{tabulate_ecg_param_summary}} - This function generates central tendency tables for a ECG parameter (QTc, HR).
#' 	\item \code{\link{tabulate_high_qtc_obs}} - This function generates a table of the number of observations with high QTc values.
#' 	\item \code{\link{tabulate_high_qtc_sub}} - This function generates a table of the number of subjects with at least one high QTc observation.
#' 	\item \code{\link{tabulate_pk_parameters}} - This function generates a table of Pharmacokinetic parameters (\eqn{C_max}, \eqn{T_max})
#' 	\item \code{\link{tabulate_model_fit_parameters}} - This function generates a table of estimated fixed effect parameters with confidence intervals.
#' 	\item \code{\link{tabulate_exposure_predictions}} - This function generates a table of predicted dependent variable (\eqn{(\Delta) \Delta }QTc, \eqn{(\Delta) \Delta}HR) at drug exposure levels of interest.
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name tabulate
#' @rdname cqtkit-tabulate
NULL


#' cqtkit Fit Functions
#'
#' These functions are focused on generating and working with mixed-effects models for various aspects of C-QTc analyses
#'
#' @section fit:
#' \itemize{
#' 	\item \code{\link{fit_prespecified_model}} - This generates the prespecified model from the White Paper on C-QT
#' 	\item \code{\link{compute_model_fit_parameters}} - This function extracts the model parameters from the fitted model
#' 	\item \code{\link{compute_fit_results}} - This function computes the model predictions
#' 	\item \code{\link{fit_qtc_linear_model}} - This function fits QTc data to a linear mixed-effect model with (intercept and slope)
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name fit
#' @rdname cqtkit-fit
NULL

#' cqtkit Goodness-of-Fit Functions
#'
#' These functions generate plots for validating the fitted model to the data.
#'
#' @section gof:
#' \itemize{
#' 	\item \code{\link{gof_plots}} - This function generates a summary of GoF plots.
#' 	\item \code{\link{gof_concordance_plots}} - This function generates concordance plots for Individual- and Population-predictions.
#' 	\item \code{\link{gof_residuals_plots}} - This function generates a plot of residuals against dependent and independent variable.
#' 	\item \code{\link{gof_qq_plots}} - This function generates a QQ plot from the model.
#' 	\item \code{\link{gof_residuals_time_boxplots}} - This function generates a boxplot of residuals against time data (categorical).
#' 	\item \code{\link{gof_residuals_trt_boxplots}} - This function generates a boxplot of residuals against the treatment group.
#' 	\item \code{\link{gof_vpc_plot}} - This function generates a Visual Predictive Check of the model.
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name gof
#' @rdname cqtkit-gof
NULL

#' cqtkit Prediction Functions
#'
#' These functions use a fitted model to make plots of model predictions.
#'
#' @section predict:
#' \itemize{
#' 	\item \code{\link{predict_with_observations_plot}} - This generates a plot with all observations of time-matched \eqn{\Delta \Delta}QTc and Concentration data with model predictions overlaid with confidence intervals.
#' 	\item \code{\link{predict_with_quantiles_plot}} - This function generates a plot with binned concentration values and average \eqn{\Delta \Delta}QTc for each bin with model predictions overlaid with confidence intervals.
#' 	\item \code{\link{predict_with_exposure_plot}} - This function generates a plot with model predictions over the range of observed Concentration data with dashed lines for (supra)therapeutic \eqn{C_max}(s).
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name predict
#' @rdname cqtkit-predict
NULL

#' cqtkit Plot Styling Functions
#'
#' These functions customize the style of plots
#'
#' @section style:
#' \itemize{
#' 	\item \code{\link{add_horizontal_references}} - Adds dashed horizontal reference lines to plots
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name style
#' @rdname cqtkit-style
NULL

#' cqtkit Included Datasets
#'
#' cqtkit has datasets from four different drugs included in the packge. These data was originally made available in the following publication and obtained from Physionet.
#' Each drug has been filtered into its own dataset and is ready to use with cqtkit.
#'
#' @section datasets:
#' \itemize{
#' 	\item \code{\link{cqtkit_data_verapamil}} - dataset for Verapamil (minimal QTc prolongation)
#' 	\item \code{\link{cqtkit_data_bl_verapamil}} - baseline data for Verapamil
#' 	\item \code{\link{cqtkit_data_dofetilide}} - dataset for Dofetilide (significant QTc prolongation)
#' 	\item \code{\link{cqtkit_data_bl_dofetilide}} - baseline data for Dofetilide
#' 	\item \code{\link{cqtkit_data_quinidine}} - dataset for Quinidine (significant QTc prolongation)
#' 	\item \code{\link{cqtkit_data_bl_quinidine}} - baseline data for Quinidine
#' 	\item \code{\link{cqtkit_data_ranolazine}} - dataset for Ranolazine (moderate QTc prolongation)
#' 	\item \code{\link{cqtkit_data_bl_ranolazine}} - baseline data for Ranolazine
#' }
#'
#' @seealso \code{\link{cqtkit-package}}
#' @name datasets
#' @rdname cqtkit-datasets
NULL
