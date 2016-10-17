#' discharge
#'
#' The function \code{sdis.q()} computes the discharge from a terrain feature routed through the river.
#' @param waterContent excess of water in millimeters
#' @param area in squared meters
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param UHxConvRiver convolution of the Unit Hydrograph of x to the Unit Hydrograph of the river
#' @return a discharge value in m3/s
#' @keywords dddDischarge
#' @export
#' @examples
#' \dontrun{
#' sdis.q()
#' }

sdis.q <-function(waterContent,area,Timeresinsec,UHxConvRiver){
  res <- ((waterContent*area)/Timeresinsec)*UHxConvRiver
  return(res)
}
