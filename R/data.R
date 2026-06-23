#' Simulated lung cancer patients data by Maringe et al. (2020)
#'
#' The dataset is from supplmentary files of [Maringe et al. (2020)](https://doi.org/10.1093/ije/dyaa057).
#' The dataset is a set of 200 simulated lung cancer patients. 
#' These patients are followed up for a year following their cancer diagnosis:
#' 106 of them received surgery within six months of their diagnosis and 
#' 48 died in the year.
#'
#' @format ## `lungcancer`
#' A data frame with 200 rows and 12 columns:
#' \describe{
#'   \item{id}{patient identifier}
#'   \item{fup_obs}{observed follow-up time (time to death or 1 year if censored alive)}
#'   \item{death}{observed event of interest (all-cause death) 1: dead, 0:alive}
#'   \item{timetosurgery}{time to surgery (NA if no surgery)}
#'   \item{surgery}{observed treatment 1 if the patient received surgery within 6 month, 0 otherwise}
#'   \item{age}{age at diagnosis}
#'   \item{sex}{patient's sex}
#'   \item{perf}{performance status at diagnosis}
#'   \item{stage}{stage at diagnosis}
#'   \item{deprivation}{deprivation score}
#'   \item{charlson}{Charlson's comorbidity index}
#'   \item{emergency}{route to diagnosis}
#' }
#' @source <https://doi.org/10.1093/ije/dyaa057>
"lungcancer"