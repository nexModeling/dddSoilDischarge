#' init

#' The funtion initializes the soil discharge
#' Unit is in m3/s
#' @param MAD Mean Annual Discharge values
#' @param q1 first value of the runoff timeserie
#' @param D volume of the unsaturated zone
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param modelArea list of parameters about the area
#'  list(totarea,slopesriverarea,nobognoglacarea,bogarea)
#' @param modelLayer list of parameters about the Layers
#'  list(maxL,speed,nbStepsDelay,z,distr,param,NoL)
#' @param modelRiver list of parameters about the river
#'  list(maxL,speed,nbStepsDelay,z,distr,param)
#' @param modelBog list of parameters of the bog
#'  list(maxL,speed,nbStepsDelay,z,dist,param)
#' @param layerUH Unit Hydrograph of the saturation layers
#' @param NoL number of level zone
#' @param UHriver Unit Hydrograp of the river
#' @keywords soilDischarge
#' @export
#' @examples
#' \dontrun{
#' init()
#' }

init <-function(MAD,
                q1,
                D,
                Timeresinsec,
                modelArea,
                modelLayer,
                modelRiver,
                modelBog,
                layerUH,
                NoL,
                UHriver){

    qsimlength <- (modelLayer$nbStepsDelay[NoL]+modelRiver$nbStepsDelay-1)

    soilDischarge <- list(D       = D,
                          qsimutx = rep(0,qsimlength),
                          qsimX   = rep(0,qsimlength))

    MAD1    <- ifelse(q1 > 0, q1, MAD) #m3/s
    D_MAD       <- sdis.D(q=MAD1,Timeresinsec=Timeresinsec,area=modelArea$slopesriverarea)

    qsimutxInit <- stateX(Timeresinsec = Timeresinsec,
                          layerUH = layerUH,
                          ddist = rep(1/NoL,NoL),
                          UHriver = UHriver,
                          waterContent = D_MAD,
                          area = modelArea$slopesriverarea,
                          modelBog = modelBog,
                          waterContentBog = D_MAD,
                          areabog = modelArea$bogarea,
                          qsimX=soilDischarge$qsimX)
    # OUTPUT (list)
    # qsimX : accumulation of qsimX at time t-1 and qsimutx
    # qsimutx : sim at time t

    soilDischarge$qsimutx[1:length(qsimutxInit$qsimutx)] <- soilDischarge$qsimutx[1:length(qsimutxInit$qsimutx)] +qsimutxInit$qsimutx
    soilDischarge$qsimX <- soilDischarge$qsimutx

    return(soilDischarge)

}
