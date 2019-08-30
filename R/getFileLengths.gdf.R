## file:getFileLengths.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Find number of samples in a single data file
##
## Arguments:
##  LPIparam  A LPI parameter list
##
## Returns:
##  dataFileLengths A named list of file lengths
##                  with elements "RX1", "RX2", "TX1", "TX2"
##

getFileLengths.gdf <- function( LPIparam )
  {

    # A vector for file lengths
    fileLengths <- rep(NA,4)
    names(fileLengths) <- c( "RX1" , "RX2" , "TX1" , "TX2" )

    # Read the file lengths in another function.
    for( XXN in c( "RX1" , "RX2" , "TX1" , "TX2" ) ){
      fileLengths[XXN] <- fileLength.gdf( LPIparam[["dataDir"]][[XXN]] , prefix=LPIparam[["fileNamePrefix"]][[XXN]] , extension=LPIparam[["fileNameExtension"]][[XXN]] )
    }
    
    return( list( dataFileLengths = fileLengths ) )
    
  }


