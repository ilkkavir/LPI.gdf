## file:firstSampleTime.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Read the time stamps file and try to use the possible
## PPS bits to find the  sampling time of the first
## sample in a gdf data directory
##
## Arguments:
##  dataDir   Data directory path
##  prefix    File name prefix
##  nInt      Number of digits in gdf format file number
##  extension File name extension
##  stampFile Time stamp file name
##  fLen      File length
##  sampFreq  Sample rate
##
## Returns:
##  firstTime Sampling time of the first data sample
##            in POSIX format
##

firstSampleTime.gdf <- function( dataDir , prefix="data-" , nInt=6 , extension=".gdf" , stampFile="timestamps.log", fLen=1e6 , sampFreq=1e6 )
  {

    timeStamp <- readSamplingTime.gdf( dataDir , stampFile , prefix , nInt , extension )

    offset <- findOffset.gdf( dataDir , prefix , nInt , extension , fLen , sampFreq , timeStamp )

    return( timeStamp + offset )

  }

