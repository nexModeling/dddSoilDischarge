#' soil discharge on bogs
#'
#' The function \code{sdis.bogs()} computes soil discharge on bogs routed through the river
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param UHriver Unit Hydrograp of the river
#' @param modelBog list of parameters of the bog
#'  list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e))
#' @param waterContent excess of water in millimeters
#' @param area in squared meters
#' @keywords soilDischarge
#' @export
#' @examples
#' \dontrun{
#' sdis.bogs()
#' }

sdis.bogs <-function(Timeresinsec,
                     UHriver,
                     modelBog,
                     waterContent,
                    area){

  # UH BOG
  model <- list(z=modelBog$z,distr=modelBog$distr,param=modelBog$param)
  if(area>0){
    res_UHbog <- dddUH::UHvec(maxL=modelBog$maxL,speed=modelBog$speed,Timeresinsec=Timeresinsec,model=model)
  } else {res_UHbog <- 1}


  #Convolution BOG-RIVER
  UHbogConvRiver <- dddUH::xConvRiver(UHx=res_UHbog,UHriver=UHriver)


  # discharge in m3/s from bogs routed through the river
  if (area>0) {
     res  <- sdis.q(waterContent=waterContent,area=area,Timeresinsec=Timeresinsec,UHxConvRiver=UHbogConvRiver)
  } else {
    res <- rep(0,length(UHbogConvRiver))
  }

  return(res)

}
