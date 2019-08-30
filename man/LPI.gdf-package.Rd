\name{LPI.gdf-package}
\alias{LPI.gdf-package}
\docType{package}
\title{Lag Profile Inversion with gdf files}
\description{ gdf format I/O routines, wrapper function to LPI, and graphics }
\details{
  \tabular{ll}{
    Package: \tab LPI.gdf \cr
    Version: \tab 0.1-4 \cr
    Date: \tab 2014-01-27\cr
    License: FreeBSD\tab \cr
    Depends: LPI, radarPointings, lattice \tab \cr
    Built: \tab \cr
  }
  Index:

  \code{\link{LPI.gdf}} Wrapper to LPI::LPI with gdf format spesific
  parameter parsing.

  \code{\link{readData.gdf}} gdf data input

  \code{\link{getDataEndTimes.gdf}} gdf data end time function

  \code{\link{getSamplingFrequencies.gdf}} gdf format sample rate
  function
  
    
  \code{\link{LPIparam.default.gdf}} default gdf format input

  \code{\link{readLPIdata.gdf}} gdf data input function for LPI

  \code{\link{getFileLengths.gdf}} gdf format file lengths

  \code{\link{getSamplingStartTimes.gdf}} gdf data sampling start times
  
  \code{\link{plotLagProfiles}} Plot of lag profiles as function of time
  
  \code{\link{plotACF}} Plot single ACF

}

\author{
Ilkka Virtanen (University of Oulu, Finland) \cr \email{ilkka.i.virtanen@oulu.fi}
}

\keyword{ package }