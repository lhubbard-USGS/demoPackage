#' @title summary_dv
#'
#' @description Calculate min max of day of year
#'
#' @importFrom dataRetrieval readNWISdv
#' @importFrom dataRetrieval renameNWISColumns
#' @importFrom dplyr mutate group_by summarise
#'
#' @export
#' @param site character USGS site id
#'
#' @examples
#' sum_table <- summary_dv("08279500")
#'
summary_dv <- function(site){

  Date <- Flow <- doy <- ".dplyr"    #To use non-standard evaluation code
  dv_data <- readNWISdv(siteNumbers=site,
                        parameterCd = "00060", startDate = "", endDate = "")

  dv_summ <- renameNWISColumns(dv_data)
  dv_summ <- mutate(dv_summ, doy = as.numeric(strftime(Date, format = "%j")))
  dv_summ <- group_by(dv_summ, doy)
  dv_summ <- summarise(dv_summ,
                       max = max(Flow, na.rm = TRUE),
                       min = min(Flow, na.rm = TRUE))
  return(dv_summ)

}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}
