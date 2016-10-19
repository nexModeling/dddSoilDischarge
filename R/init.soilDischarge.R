#' Initializing the soil discharge

#' The funtion initializes the soil discharge
#' Unit is in m3/s
#' @param method method for the initialization, "load", "source", "manual", "processed"
#' @param path directory where to get the files, in used when method is "load" or "source"
#' @param qsimutx soil discharge at time t
#' @param qsimX accumulated soil discharge
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
#' @param UHriver Unit Hydrograp of the river
#' @keywords soilDischarge
#' @export
#' @examples
#' \dontrun{
#' init.soilDischarge()
#' }

init.soilDischarge <-function(method=NULL,path=NULL,qsimutx=NULL,qsimX=NULL,MAD=NULL,q1=NULL,D=NULL,Timeresinsec=NULL,modelArea=NULL,modelLayer=NULL,modelRiver=NULL,modelBog=NULL,layerUH=NULL,UHriver=NULL){

  soilDischarge <- switch(method,
    "manual"    = init.manual(D=D,qsimutx=qsimutx,qsimX=qsimX),
    "processed" = init.processed(MAD=MAD,q1=q1,D=D,Timeresinsec=Timeresinsec,modelArea=modelArea,modelLayer=modelLayer,modelRiver=modelRiver,modelBog=modelBog,layerUH=layerUH,UHriver=UHriver),
    "load"      = init.load(path=path),
    "source"    = init.source(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(soilDischarge)
}


init.manual <- function(D,qsimutx,qsimX){
  res <- list(D       = D,
              qsimutx = qsimutx,
              qsimX   = qsimX)
  return(res)
}

init.load <- function(path){
  load(paste0(path,"soilDischarge.rda"))
  return(soilDischarge)
}


init.source <- function(path){
  source(paste0(path,"soilDischarge.R"),local=TRUE)
  return(soilDischarge)
}


init.processed <-function(MAD,q1,D,Timeresinsec,modelArea,modelLayer,modelRiver,modelBog,layerUH,UHriver){
   if ( (!is.null(MAD)) && (!is.null(q1)) && (!is.null(D)) && (!is.null(Timeresinsec)) &&
        (!is.null(modelArea)) && (!is.null(modelLayer)) && (!is.null(modelBog)) &&
        (!is.null(layerUH)) && (!is.null(UHriver)) ){

    qsimlength <- (modelLayer$nbStepsDelay[modelLayer$NoL]+modelRiver$nbStepsDelay-1)

    soilDischarge <- init.manual(D=D,qsimutx=rep(0,qsimlength),qsimX=rep(0,qsimlength))

    MAD1    <- ifelse(q1 > 0, q1, MAD) #m3/s
    D_MAD       <- sdis.D(q=MAD1,Timeresinsec=Timeresinsec,area=modelArea$slopesriverarea)

    qsimutxInit <- stateX(Timeresinsec = Timeresinsec,
                          layerUH = layerUH,
                          ddist = rep(1/modelLayer$NoL,modelLayer$NoL),
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

   } else stop("NULL arguments in init.processed Soil Discharge")
}
