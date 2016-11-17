#' state of soil discharge
#'
#' The function \code{stateX.soilDischarge()} process the soil discharge:
#' 1- process the soil discharge of slopes
#' 2- process the soil discharge of bogs
#' 3- process the accumulated discharge with a dynamic
#' Unit is in m3/s
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param layerUH Unit Hydrograph of the saturation layers
#' @param ddist states of each saturation level
#' @param UHriver Unit Hydrograp of the river
#' @param waterContent excess of water in millimeters over Slopes
#' @param area in squared meters
#' @param modelBog list of parameters of the bog
#'  list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e))
#' @param waterContentBog excess of water in millimeters over Bogs
#' @param areabog in squared meters
#' @param qsimX soil discharge value to take into account while doing the accumulation
#' @return The output is a list of i- the discharge at the time step and ii- the accumulatied discharge
#' @keywords soilDischarge
#' @export
#' @examples
#' \dontrun{
#' stateX()
#' }
stateX.soilDischarge <-function(Timeresinsec,
                  layerUH,
                  ddist,
                  UHriver,
                  waterContent,
                  area,
                  modelBog,
                  waterContentBog,
                  areabog,
                  qsimX){
    #########################
    # Discharge at time t   #
    #########################
    qslopes <- sdis.slopes(Timeresinsec=Timeresinsec,
                                      layerUH=layerUH,
                                      ddist=ddist,
                                      UHriver=UHriver,
                                      waterContent=waterContent,
                                      area=area)

    qBogut <- sdis.bogs(Timeresinsec=Timeresinsec,
                                    UHriver=UHriver,
                                    modelBog=modelBog,
                                    waterContent=waterContentBog,
                                    area=areabog)


    if (modelBog$nbStepsDelay<length(qslopes)) {
       qslopes[1:modelBog$nbStepsDelay] <- qslopes[1:modelBog$nbStepsDelay] + qBogut[1:modelBog$nbStepsDelay]
       qsimutx <- qslopes
    } else {
      qBogut[1:length(qslopes)] <- qBogut[1:length(qslopes)] + qslopes[1:length(qslopes)]
      qsimutx <- qBogut
    }

    #########################################
    # Accumulated discharge with a dynamic  #
    #########################################
    if(length(qsimutx)>1) {
        qsimX[1:(length(qsimX)-1)] <- qsimX[2:length(qsimX)]
        qsimX[length(qsimX)] <-0
        qsimX[1:(length(qsimutx))] <- qsimX[1:(length(qsimutx))] + qsimutx[1:(length(qsimutx))]
    } else if(length(qsimutx)==1) qsimX[1] <-qsimutx[1]

    res <- list(D = (-1)*ddistAll$S,
                qsimX = qsimX,
                qsimutx = qsimutx)

    return(res)


}
