## file:getSamplingFrequencies.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Read sampling frequencies from sampler log files.
##
## Arguments:
##  LPIparam  A LPI parameter list
##
## Returns:
##  sampFreqs A named list of sample rates with
##            elements "RX1", "RX2", "TX1", "TX2"
##

getSamplingFrequencies.gdf <- function( LPIparam )
  {
    # A vector for the sample rates
    sampFreqs <- rep(NA,4)
    names( sampFreqs ) <- c( "RX1" , "RX2" , "TX1" , "TX2" )

    # try to read the sampling frequencies from sampler
    # log files, but do not overwrite possible
    # user-input if the read fails

    for( XXN in c( "RX1" , "RX2" , "TX1" , "TX2" ) ){
      
      # First try from the data directory
      tmpfreq <- readSamplingFrequency.gdf( LPIparam[["dataDir"]][XXN] , samplerLog="sampler.log")
      
      # If the first read fails, try one step up in directory hierarcy
      if( is.null( tmpfreq )  ) tmpfreq <- readSamplingFrequency.gdf( LPIparam[["dataDir"]][XXN] , samplerLog=file.path("..","sampler.log") )
      
      # If the read succeeded copy the value,
      # otherwise print a warning message.
      if( !is.null( tmpfreq ) ){
        sampFreqs[XXN] <- tmpfreq
      }else{
        cat("Could not read sampler log file from" , LPIparam[["dataDir"]][XXN] , "\n")
      }
    }

    return( sampFreqs )
    
  }


