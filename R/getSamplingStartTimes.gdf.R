## file:getSampingStartTimes.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

## 
## Read the sampiling start times in each data directory
## and return the results as unix-times with fractional
## seconds ( seconds from 1970-01-01 )
##
## Arguments:
##  LPIparam  A LPI parameter list
##
## Returns:
##  startTimes A named vector of sampling start times
##             with elements "RX1", "RX2", "TX1", "TX2"
##

getSamplingStartTimes.gdf <- function( LPIparam )
  {

    startTimes <- rep(NA,4)
    names( startTimes ) <- c( "RX1" , "RX2" , "TX1" , "TX2" )

    for( XXN in c( "RX1" , "RX2" , "TX1" , "TX2" ) ){
      startTimes[XXN] <- firstSampleTime.gdf( LPIparam[['dataDir']][[XXN]] , prefix=LPIparam[["fileNamePrefix"]][[XXN]] , nInt=6 , extension=LPIparam[["fileNameExtension"]][XXN] , stampFile="timestamps.log" , fLen=LPIparam[["dataFileLengths"]][[XXN]] , sampFreq=LPIparam[["dataSampleFreqs"]][[XXN]] )
    }

    return( startTimes )

  }
