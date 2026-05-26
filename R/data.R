#' C-QT analysis dataset for verapamil with minimal QTc effect (~8 ms prolongation)
#'
#' @format ## `cqtkit_data_verapamil`
#' A tibble: 657 × 30:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{DOSEF}{Factor value of dose}
#'   \item{NTLD}{Nominal time since last dose (h)}
#'   \item{TAFD}{Time after first dose (h)}
#'   \item{CONC}{Drug concentration}
#'   \item{CONCU}{Drug concentration unit}
#'   \item{RR}{RR after dose (ms)}
#'   \item{RRBL}{Baseline RR before dose (ms)}
#'   \item{deltaRR}{Difference between RR and baseline RR (ms)}
#'   \item{HR}{HR after dose (bpm)}
#'   \item{HRBL}{Baseline HR before dose (ms)}
#'   \item{HRBLM}{Mean baseline HR across subjects (ms)}
#'   \item{deltaHRBL}{Difference between baseline HR and mean baseline HR across subjects (ms)}
#'   \item{deltaHR}{Difference between HR and baseline HR (bpm)}
#'   \item{QT}{QT after dose (ms)}
#'   \item{QTBL}{Baseline QT before dose (ms)}
#'   \item{deltaQT}{Difference between QT and baseline QT (ms)}
#'   \item{QTCB}{Bazett's corrected QT after dose (ms)}
#'   \item{QTCBBL}{Baseline Bazett's corrected QT before dose (ms)}
#'   \item{QTCBBLM}{Mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCBBL}{Difference between baseline QTCB and mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCB}{Difference between QTCB and baseline QTCB (ms)}
#'   \item{QTCF}{Fridericia's corrected QT after dose (ms)}
#'   \item{QTCFBL}{Baseline Fridericia's corrected QT before dose(ms)}
#'   \item{QTCFBLM}{Mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCFBL}{Difference between baseline QTCF and mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCF}{Difference between QTCF and baseline QTCF (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_verapamil"


#' Baseline data for verapamil C-QT study
#'
#' @format ## `cqtkit_data_bl_verapamil`
#' A data frame with 131 rows and 17 columns:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{SEX}{Sex either M (Male) or F (Female)}
#'   \item{AGE}{Age in years at screening}
#'   \item{HGHT}{Height in cm at screening}
#'   \item{WGHT}{Weight in kg at screening}
#'   \item{RACE}{Race as provided}
#'   \item{ETHNIC}{Ethnicity as provided}
#'   \item{VISIT}{Visit code}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{TPT}{Timepoint of collection relative to first dose (h)}
#'   \item{RR}{RR before dose (ms)}
#'   \item{QT}{QT before dose (ms)}
#'   \item{HR}{HR before dose (bpm)}
#'   \item{QTCB}{Bazett's corrected QT before dose (ms)}
#'   \item{QTCF}{Fridericia's corrected QT before dose (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_bl_verapamil"


#' C-QT analysis dataset for dofetilide with significant QTc effect (~38 ms prolongation)
#'
#' @format ## `cqtkit_data_dofetilide`
#' A tibble: 657 × 30:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{DOSEF}{Factor value of dose}
#'   \item{NTLD}{Nominal time since last dose (h)}
#'   \item{TAFD}{Time after first dose (h)}
#'   \item{CONC}{Drug concentration}
#'   \item{CONCU}{Drug concentration unit}
#'   \item{RR}{RR after dose (ms)}
#'   \item{RRBL}{Baseline RR before dose (ms)}
#'   \item{deltaRR}{Difference between RR and baseline RR (ms)}
#'   \item{HR}{HR after dose (bpm)}
#'   \item{HRBL}{Baseline HR before dose (ms)}
#'   \item{HRBLM}{Mean baseline HR across subjects (ms)}
#'   \item{deltaHRBL}{Difference between baseline HR and mean baseline HR across subjects (ms)}
#'   \item{deltaHR}{Difference between HR and baseline HR (bpm)}
#'   \item{QT}{QT after dose (ms)}
#'   \item{QTBL}{Baseline QT before dose (ms)}
#'   \item{deltaQT}{Difference between QT and baseline QT (ms)}
#'   \item{QTCB}{Bazett's corrected QT after dose (ms)}
#'   \item{QTCBBL}{Baseline Bazett's corrected QT before dose (ms)}
#'   \item{QTCBBLM}{Mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCBBL}{Difference between baseline QTCB and mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCB}{Difference between QTCB and baseline QTCB (ms)}
#'   \item{QTCF}{Fridericia's corrected QT after dose (ms)}
#'   \item{QTCFBL}{Baseline Fridericia's corrected QT before dose(ms)}
#'   \item{QTCFBLM}{Mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCFBL}{Difference between baseline QTCF and mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCF}{Difference between QTCF and baseline QTCF (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_dofetilide"


#' Baseline data for dofetilide C-QT study
#'
#' @format ## `cqtkit_data_bl_dofetilide`
#' A data frame with 132 rows and 17 columns:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{SEX}{Sex either M (Male) or F (Female)}
#'   \item{AGE}{Age in years at screening}
#'   \item{HGHT}{Height in cm at screening}
#'   \item{WGHT}{Weight in kg at screening}
#'   \item{RACE}{Race as provided}
#'   \item{ETHNIC}{Ethnicity as provided}
#'   \item{VISIT}{Visit code}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{TPT}{Timepoint of collection relative to first dose (h)}
#'   \item{RR}{RR before dose (ms)}
#'   \item{QT}{QT before dose (ms)}
#'   \item{HR}{HR before dose (bpm)}
#'   \item{QTCB}{Bazett's corrected QT before dose (ms)}
#'   \item{QTCF}{Fridericia's corrected QT before dose (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_bl_dofetilide"


#' C-QT analysis dataset for ranolazine with moderate QTc effect (~14 ms prolongation)
#'
#' @format ## `cqtkit_data_ranolazine`
#' A tibble: 656 × 30:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{DOSEF}{Factor value of dose}
#'   \item{NTLD}{Nominal time since last dose (h)}
#'   \item{TAFD}{Time after first dose (h)}
#'   \item{CONC}{Drug concentration}
#'   \item{CONCU}{Drug concentration unit}
#'   \item{RR}{RR after dose (ms)}
#'   \item{RRBL}{Baseline RR before dose (ms)}
#'   \item{deltaRR}{Difference between RR and baseline RR (ms)}
#'   \item{HR}{HR after dose (bpm)}
#'   \item{HRBL}{Baseline HR before dose (ms)}
#'   \item{HRBLM}{Mean baseline HR across subjects (ms)}
#'   \item{deltaHRBL}{Difference between baseline HR and mean baseline HR across subjects (ms)}
#'   \item{deltaHR}{Difference between HR and baseline HR (bpm)}
#'   \item{QT}{QT after dose (ms)}
#'   \item{QTBL}{Baseline QT before dose (ms)}
#'   \item{deltaQT}{Difference between QT and baseline QT (ms)}
#'   \item{QTCB}{Bazett's corrected QT after dose (ms)}
#'   \item{QTCBBL}{Baseline Bazett's corrected QT before dose (ms)}
#'   \item{QTCBBLM}{Mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCBBL}{Difference between baseline QTCB and mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCB}{Difference between QTCB and baseline QTCB (ms)}
#'   \item{QTCF}{Fridericia's corrected QT after dose (ms)}
#'   \item{QTCFBL}{Baseline Fridericia's corrected QT before dose(ms)}
#'   \item{QTCFBLM}{Mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCFBL}{Difference between baseline QTCF and mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCF}{Difference between QTCF and baseline QTCF (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_ranolazine"


#' Baseline data for ranolazine C-QT study
#'
#' @format ## `cqtkit_data_bl_ranolazine`
#' A data frame with 132 rows and 17 columns:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{SEX}{Sex either M (Male) or F (Female)}
#'   \item{AGE}{Age in years at screening}
#'   \item{HGHT}{Height in cm at screening}
#'   \item{WGHT}{Weight in kg at screening}
#'   \item{RACE}{Race as provided}
#'   \item{ETHNIC}{Ethnicity as provided}
#'   \item{VISIT}{Visit code}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{TPT}{Timepoint of collection relative to first dose (h)}
#'   \item{RR}{RR before dose (ms)}
#'   \item{QT}{QT before dose (ms)}
#'   \item{HR}{HR before dose (bpm)}
#'   \item{QTCB}{Bazett's corrected QT before dose (ms)}
#'   \item{QTCF}{Fridericia's corrected QT before dose (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_bl_ranolazine"

#' C-QT analysis dataset for quinidine with significant QTc effect (~50 ms prolongation)
#'
#' @format ## `cqtkit_data_quinidine`
#' A tibble: 639 × 30:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{DOSEF}{Factor value of dose}
#'   \item{NTLD}{Nominal time since last dose (h)}
#'   \item{TAFD}{Time after first dose (h)}
#'   \item{CONC}{Drug concentration}
#'   \item{CONCU}{Drug concentration unit}
#'   \item{RR}{RR after dose (ms)}
#'   \item{RRBL}{Baseline RR before dose (ms)}
#'   \item{deltaRR}{Difference between RR and baseline RR (ms)}
#'   \item{HR}{HR after dose (bpm)}
#'   \item{HRBL}{Baseline HR before dose (ms)}
#'   \item{HRBLM}{Mean baseline HR across subjects (ms)}
#'   \item{deltaHRBL}{Difference between baseline HR and mean baseline HR across subjects (ms)}
#'   \item{deltaHR}{Difference between HR and baseline HR (bpm)}
#'   \item{QT}{QT after dose (ms)}
#'   \item{QTBL}{Baseline QT before dose (ms)}
#'   \item{deltaQT}{Difference between QT and baseline QT (ms)}
#'   \item{QTCB}{Bazett's corrected QT after dose (ms)}
#'   \item{QTCBBL}{Baseline Bazett's corrected QT before dose (ms)}
#'   \item{QTCBBLM}{Mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCBBL}{Difference between baseline QTCB and mean baseline QTCB across subjects (ms)}
#'   \item{deltaQTCB}{Difference between QTCB and baseline QTCB (ms)}
#'   \item{QTCF}{Fridericia's corrected QT after dose (ms)}
#'   \item{QTCFBL}{Baseline Fridericia's corrected QT before dose(ms)}
#'   \item{QTCFBLM}{Mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCFBL}{Difference between baseline QTCF and mean baseline QTCF across subjects (ms)}
#'   \item{deltaQTCF}{Difference between QTCF and baseline QTCF (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_quinidine"


#' Baseline data for quinidine C-QT study
#'
#' @format ## `cqtkit_data_bl_quinidine`
#' A data frame with 129 rows and 17 columns:
#' \describe{
#'   \item{ID}{Unique subject identifier}
#'   \item{SEX}{Sex either M (Male) or F (Female)}
#'   \item{AGE}{Age in years at screening}
#'   \item{HGHT}{Height in cm at screening}
#'   \item{WGHT}{Weight in kg at screening}
#'   \item{RACE}{Race as provided}
#'   \item{ETHNIC}{Ethnicity as provided}
#'   \item{VISIT}{Visit code}
#'   \item{TRTG}{Treatment Group (Drug or Placebo)}
#'   \item{DOSE}{numeric value of dose}
#'   \item{DOSEU}{Units of DOSE}
#'   \item{TPT}{Timepoint of collection relative to first dose (h)}
#'   \item{RR}{RR before dose (ms)}
#'   \item{QT}{QT before dose (ms)}
#'   \item{HR}{HR before dose (bpm)}
#'   \item{QTCB}{Bazett's corrected QT before dose (ms)}
#'   \item{QTCF}{Fridericia's corrected QT before dose (ms)}
#' }
#' @source Derived from Johannesen et. al. (https://ascpt.onlinelibrary.wiley.com/doi/10.1038/clpt.2014.155)
"cqtkit_data_bl_quinidine"
