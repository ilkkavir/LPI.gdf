## file:latestSampleTime.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Calculate sampling time of the last data file based
## on sampling start time, file length, sample rate,
## and number of data files. The number of files
## is read from the timestamps.log file.
##
## Arguments:
##  dataDir   Data directory
##  prefix    File name prefix
##  extension File name extension
##  fLen      File length
##  sampFreq  Sample rate
##  firstSampleTime Sampling time of the first sample
##                  of the data stream.
##
## Returns:
##  t Sampling time of the last recorded sample
##

latestSampleTime.gdf <- function( dataDir , prefix , extension , fLen , sampFreq , firstSampleTime )
  {
    # Seek for the timestamp.log file
    sfile <- file.path( dataDir , "timestamps.log" )
    if(!file.exists(sfile))     sfile <- file.path( dataDir , '..' , "timestamps.log" )
    if(!file.exists(sfile))     sfile <- file.path( dataDir , '..' , '..' , "timestamps.log" )

    # Each file has its own line in sampler.log
    nf <- length( readLines( sfile ) )

    # Convert to time in seconds and return
    return( firstSampleTime + nf*fLen/sampFreq )
    
  }
