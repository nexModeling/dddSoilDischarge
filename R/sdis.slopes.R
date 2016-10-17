#' soil discharge on slopes
#'
#' The function \code{sdis.slopes()} computes soil discharge on slopes routed through the river
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param layerUH Unit Hydrograph of the saturation layers
#' @param ddist states of each saturation level
#' @param UHriver Unit Hydrograp of the river
#' @param waterContent excess of water in millimeters
#' @param area in squared meters
#' @keywords soilDischarge
#' @export
#' @examples
#' \dontrun{
#' sdis.slopes()
#' }
sdis.slopes <-function(Timeresinsec,
                       layerUH,
                       ddist,
                       UHriver,
                       waterContent,
                       area){

  #UH SLOPES
  res_UHslopes <- dddUH::UHslopes(layerUH=layerUH,ddist=ddist)

  #Convolution SLOPES-RIVER
  UHslopesConvRiver <- dddUH::xConvRiver(UHx=res_UHslopes,UHriver=UHriver)

  # Discharge from slopes
  res <- sdis.q(waterContent=waterContent,area=area,Timeresinsec=Timeresinsec,UHxConvRiver=UHslopesConvRiver)

  return(res)

}
