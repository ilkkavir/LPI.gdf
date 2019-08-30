## file:getDataEndTimes.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

## 
## Find the sampling times of latest recorded data sample
##in each data directory, assuming that recording has
## been continuous since starting the sampling
##
## Arguments:
##  LPIparam A LPI parameter list
##
## Returns:
##  endTimes A named vector of data end times
##           with entries "RX1", "RX2", "TX1", "TX2"
##

getDataEndTimes.gdf <- function( LPIparam )
  {
    # A vector for the end times
    endTimes <- rep(NA,4)
    names( endTimes ) <- c( "RX1" , "RX2" , "TX1" , "TX2" )

    # The time may be different for each data type
    for( XXN in c( "RX1" , "RX2" , "TX1" , "TX2" ) ){

      endTimes[XXN] <- latestSampleTime.gdf( LPIparam[['dataDir']][XXN] , prefix=LPIparam[["fileNamePrefix"]][XXN] , extension=LPIparam[["fileNameExtension"]][XXN] , fLen=LPIparam[["dataFileLengths"]][XXN] , sampFreq=LPIparam[["dataSampleFreqs"]][XXN] , firstSampleTime=LPIparam[["dataStartTimes"]][XXN] )
                            
    }
    
    return(endTimes)

  }
