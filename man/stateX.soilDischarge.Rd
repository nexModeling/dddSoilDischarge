% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stateX.soilDischarge.R
\name{stateX.soilDischarge}
\alias{stateX.soilDischarge}
\title{state of soil discharge}
\usage{
stateX.soilDischarge(Timeresinsec, layerUH, ddistAll, UHriver, waterContent,
  area, modelBog, waterContentBog, areabog, qsimX)
}
\arguments{
\item{Timeresinsec}{time resolution of the process in second (1hour: 3600s, ... etc)}

\item{layerUH}{Unit Hydrograph of the saturation layers}

\item{ddistAll}{states of each saturation level}

\item{UHriver}{Unit Hydrograp of the river}

\item{waterContent}{excess of water in millimeters over Slopes}

\item{area}{in squared meters}

\item{modelBog}{list of parameters of the bog
list(maxL=a,speed=b,nbStepsDelay=c,z=d,distr="dexp",param=c(e))}

\item{waterContentBog}{excess of water in millimeters over Bogs}

\item{areabog}{in squared meters}

\item{qsimX}{soil discharge value to take into account while doing the accumulation}
}
\value{
The output is a list of i- the discharge at the time step and ii- the accumulatied discharge
}
\description{
The function \code{stateX.soilDischarge()} process the soil discharge:
1- process the soil discharge of slopes
2- process the soil discharge of bogs
3- process the accumulated discharge with a dynamic
Unit is in m3/s
}
\examples{
\dontrun{
stateX()
}
}
\keyword{soilDischarge}

