#' Data download
#'
#' Download vulture project data from the Movebank repository, with some minor specifications. Note that you must specify your movebank credentials.
#' @param loginObject A Movebank login object, created by passing a username and password to move::movebankLogin
#' @return A movestack
#' @export
downloadVultures <- function(loginObject, extraSensors = F, removeDup = T,
                             dateTimeStartUTC = NULL, dateTimeEndUTC = NULL){
  move::getMovebankData(study = Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel,
                  login = loginObject,
                  includeExtraSensors = FALSE,
                  deploymentAsIndividuals = FALSE,
                  removeDuplicatedTimestamps = TRUE,
                  timestamp_start = dateTimeStartUTC,
                  timestamp_end = dateTimeEndUTC)
}
