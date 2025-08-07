#' C-QT analysis dataset for verapamil with minimal QTc effect (~8 ms prolongation)
#'
#' @format ## `cqtkit_data_verapamil`
#' A tibble: 643 × 15:
#' \describe{
#'   \item{ID}{Uniuqe subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{DOSEF}{Factor value of dose}
#'   \item{NTLD}{Nominal time since last dose (h)}
#'   \item{CONC}{Drug concentration}
#'   \item{CONCU}{Drug concentration unit}
#'   \item{RR}{RR after dose (ms)}
#'   \item{RRBL}{Baseline RR beore dose (ms)}
#'   \item{HR}{HR after dose (bpm)}
#'   \item{HRBL}{Baseline HR beore dose (ms)}
#'   \item{HRBLM}{Mean baseline HR across subjects (ms)}
#'   \item{deltaHRBL}{Difference between baseline HR and mean baseline HR across subjects (ms)}
#'   \item{QT}{QT after dose (ms)}
#'   \item{QTBL}{Baseline QT before dose (ms)}
#'   \item{QTCB}{Bazett's corrected QT after dose (ms)}
#'   \item{QTCBBL}{Baseline Bazett's corrected QT before dose (ms)}
#'   \item{QTCBBLM}{Mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCBBL}{Difference between baseline QTCB and mean baseline QTCB across subjects (ms)}
#'   \item{QTCF}{Fridericia's corrected QT after dose (ms)}
#'   \item{QTCFBL}{Baseline Fridericia's corrected QT before dose(ms)}
#'   \item{QTCFBLM}{Mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCFBL}{Difference between baseline QTCF and mean baseline QTCF across subjects (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_verapamil"


#' Baseline data for verapamil C-QT study
#'
#' @format ## `cqtkit_data_bl_verapamil`
#' A data frame with 65 rows and 10 columns:
#' \describe{
#'   \item{ID}{Uniuqe subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{TPT}{Timepoint of collection relative to first dose (h)}
#'   \item{RR}{RR before dose (ms)}
#'   \item{HR}{HR before dose (bpm)}
#'   \item{QT}{QT before dose (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_bl_verapamil"


#' C-QT analysis dataset for dofetilide with significant QTc effect (~38 ms prolongation)
#'
#' @format ## `cqtkit_data_dofetilide`
#' A tibble: 643 × 15:
#' \describe{
#'   \item{ID}{Uniuqe subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{DOSEF}{Factor value of dose}
#'   \item{NTLD}{Nominal time since last dose (h)}
#'   \item{CONC}{Drug concentration}
#'   \item{CONCU}{Drug concentration unit}
#'   \item{RR}{RR after dose (ms)}
#'   \item{RRBL}{Baseline RR beore dose (ms)}
#'   \item{HR}{HR after dose (bpm)}
#'   \item{HRBL}{Baseline HR beore dose (ms)}
#'   \item{HRBLM}{Mean baseline HR across subjects (ms)}
#'   \item{deltaHRBL}{Difference between baseline HR and mean baseline HR across subjects (ms)}
#'   \item{QT}{QT after dose (ms)}
#'   \item{QTBL}{Baseline QT before dose (ms)}
#'   \item{QTCB}{Bazett's corrected QT after dose (ms)}
#'   \item{QTCBBL}{Baseline Bazett's corrected QT before dose (ms)}
#'   \item{QTCBBLM}{Mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCBBL}{Difference between baseline QTCB and mean baseline QTCB across subjects (ms)}
#'   \item{QTCF}{Fridericia's corrected QT after dose (ms)}
#'   \item{QTCFBL}{Baseline Fridericia's corrected QT before dose(ms)}
#'   \item{QTCFBLM}{Mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCFBL}{Difference between baseline QTCF and mean baseline QTCF across subjects (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_dofetilide"


#' Baseline data for dofetilide C-QT study
#'
#' @format ## `cqtkit_data_bl_dofetilide`
#' A data frame with 65 rows and 10 columns:
#' \describe{
#'   \item{ID}{Uniuqe subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{TPT}{Timepoint of collection relative to first dose (h)}
#'   \item{RR}{RR before dose (ms)}
#'   \item{HR}{HR before dose (bpm)}
#'   \item{QT}{QT before dose (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_bl_dofetilide"


#' C-QT analysis dataset for ranolazine with moderate QTc effect (~14 ms prolongation)
#'
#' @format ## `cqtkit_data_ranolazine`
#' A tibble: 643 × 15:
#' \describe{
#'   \item{ID}{Uniuqe subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{DOSEF}{Factor value of dose}
#'   \item{NTLD}{Nominal time since last dose (h)}
#'   \item{CONC}{Drug concentration}
#'   \item{CONCU}{Drug concentration unit}
#'   \item{RR}{RR after dose (ms)}
#'   \item{RRBL}{Baseline RR beore dose (ms)}
#'   \item{HR}{HR after dose (bpm)}
#'   \item{HRBL}{Baseline HR beore dose (ms)}
#'   \item{HRBLM}{Mean baseline HR across subjects (ms)}
#'   \item{deltaHRBL}{Difference between baseline HR and mean baseline HR across subjects (ms)}
#'   \item{QT}{QT after dose (ms)}
#'   \item{QTBL}{Baseline QT before dose (ms)}
#'   \item{QTCB}{Bazett's corrected QT after dose (ms)}
#'   \item{QTCBBL}{Baseline Bazett's corrected QT before dose (ms)}
#'   \item{QTCBBLM}{Mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCBBL}{Difference between baseline QTCB and mean baseline QTCB across subjects (ms)}
#'   \item{QTCF}{Fridericia's corrected QT after dose (ms)}
#'   \item{QTCFBL}{Baseline Fridericia's corrected QT before dose(ms)}
#'   \item{QTCFBLM}{Mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCFBL}{Difference between baseline QTCF and mean baseline QTCF across subjects (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_ranolazine"


#' Baseline data for ranolazine C-QT study
#'
#' @format ## `cqtkit_data_bl_ranolazine`
#' A data frame with 65 rows and 10 columns:
#' \describe{
#'   \item{ID}{Uniuqe subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{TPT}{Timepoint of collection relative to first dose (h)}
#'   \item{RR}{RR before dose (ms)}
#'   \item{HR}{HR before dose (bpm)}
#'   \item{QT}{QT before dose (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_bl_ranolazine"

#' C-QT analysis dataset for quinidine with significant QTc effect (~50 ms prolongation)
#'
#' @format ## `cqtkit_data_quinidine`
#' A tibble: 643 × 15:
#' \describe{
#'   \item{ID}{Uniuqe subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{DOSEF}{Factor value of dose}
#'   \item{NTLD}{Nominal time since last dose (h)}
#'   \item{CONC}{Drug concentration}
#'   \item{CONCU}{Drug concentration unit}
#'   \item{RR}{RR after dose (ms)}
#'   \item{RRBL}{Baseline RR beore dose (ms)}
#'   \item{HR}{HR after dose (bpm)}
#'   \item{HRBL}{Baseline HR beore dose (ms)}
#'   \item{HRBLM}{Mean baseline HR across subjects (ms)}
#'   \item{deltaHRBL}{Difference between baseline HR and mean baseline HR across subjects (ms)}
#'   \item{QT}{QT after dose (ms)}
#'   \item{QTBL}{Baseline QT before dose (ms)}
#'   \item{QTCB}{Bazett's corrected QT after dose (ms)}
#'   \item{QTCBBL}{Baseline Bazett's corrected QT before dose (ms)}
#'   \item{QTCBBLM}{Mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCBBL}{Difference between baseline QTCB and mean baseline QTCB across subjects (ms)}
#'   \item{QTCF}{Fridericia's corrected QT after dose (ms)}
#'   \item{QTCFBL}{Baseline Fridericia's corrected QT before dose(ms)}
#'   \item{QTCFBLM}{Mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCFBL}{Difference between baseline QTCF and mean baseline QTCF across subjects (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_quinidine"


#' Baseline data for quinidine C-QT study
#'
#' @format ## `cqtkit_data_bl_quinidine`
#' A data frame with 65 rows and 10 columns:
#' \describe{
#'   \item{ID}{Uniuqe subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{TPT}{Timepoint of collection relative to first dose (h)}
#'   \item{RR}{RR before dose (ms)}
#'   \item{HR}{HR before dose (bpm)}
#'   \item{QT}{QT before dose (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_bl_quinidine"
