#' Volume of unsaturated zone
#'
#' The function \code{sdis.D()} computes the volume of unsaturared zone
#' @param q discharge in m3/s
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param area in squared meters
#' @return A value in meters
#' @keywords dddDischarge
#' @export
#' @examples
#' \dontrun{
#' dddDischarge.D()
#' }
sdis.D <-function(q,Timeresinsec,area){
  D <- q*Timeresinsec/(area)
  return(D)
}
