## file:readLPIdata.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Read one integration period of raw voltage data for
## lag profile inversion from gdf files.
## 
## Arguments:
##  LPIparam     LPI parameter list
##  intPeriod    Integration period number
##
## Returns:
##  LPIrawData  A named list with elements
##              "RX1", "RX2", "TX1", "TX2", and "success".
##              Each of the first four elements is
##                a list with elements
##                'cdata'  complex data vector
##                'idata'  logical vector of usable samples
##                'ndata'  vector length
##              "success" is TRUE if all data was read
##

readLPIdata.gdf <- function( LPIparam , intPeriod)
  {
    
    # List the files that belong to this integration period
    LPIfilelist <- listLPIfiles.gdf( intPeriod , LPIparam)

    # Initialise a list for the data vectors
    datalist <- vector( mode="list" , length=5 )
    dTypes <- c( "RX1" , "RX2" , "TX1" , "TX2" )
    names(datalist) <- c( dTypes , "success" )


    # Check data endiannes, default bigEndian=TRUE
    if(is.null(LPIparam[["bigEndian"]])) LPIparam[["bigEndian"]] <- TRUE
    LPIparam[["bigEndian"]] <- LPIexpand.input( LPIparam[["bigEndian"]] )
    
    k <- 1
    for( XXN in dTypes ){

      # If any of the file lists is empty
      # just set success to FALSE and return
      if( length(LPIfilelist[["files"]][[XXN]] ) == 0 ){
        datalist[["success"]]  <- FALSE
        return( datalist )
      }


      # Check the data file names that were already read
      # and copy from memory if possible
      readThis <- TRUE
      if( k > 1 ){
        for( n in seq(k-1) ){
          if( length( LPIfilelist[["files"]][[XXN]] ) == length( LPIfilelist[["files"]][[dTypes[n]]] )){
            if( all( LPIfilelist[["files"]][[XXN]] == LPIfilelist[["files"]][[dTypes[n]]] )){
              datalist[[XXN]] <- datalist[[dTypes[n]]]
              readThis <- FALSE
              break
            }
          }
        }
      }

      k <- k+1

      # Read the data from files if necessary
      if(readThis){
        datalist[[XXN]] <- readData.gdf( files  = LPIfilelist[["files"]][[XXN]], n = LPIfilelist[["n"]][[XXN]] , istart = LPIfilelist[["starts"]][[XXN]] , bigEndian = LPIparam[["bigEndian"]][XXN] )
      }
    }

    # Create proper index vectors
    datalist[["RX1"]][["idata"]] <- !datalist[["RX1"]][["idatai"]]
    datalist[["RX2"]][["idata"]] <- !datalist[["RX2"]][["idatai"]]
    datalist[["TX1"]][["idata"]] <-  datalist[["TX1"]][["idatai"]]
    datalist[["TX2"]][["idata"]] <-  datalist[["TX2"]][["idatai"]]

    # Remove extra idata vectors
    datalist[["RX1"]][["idatai"]] <- NULL
    datalist[["RX1"]][["idatar"]] <- NULL
    datalist[["RX2"]][["idatai"]] <- NULL
    datalist[["RX2"]][["idatar"]] <- NULL
    datalist[["TX1"]][["idatai"]] <- NULL
    datalist[["TX1"]][["idatar"]] <- NULL
    datalist[["TX2"]][["idatai"]] <- NULL
    datalist[["TX2"]][["idatar"]] <- NULL

    # Create a combined success variable, which is TRUE only
    # if all reads were successfull
    datalist[["success"]] <-
      all(datalist[["RX1"]][["success"]] ,
          datalist[["RX2"]][["success"]] ,
          datalist[["TX1"]][["success"]] ,
          datalist[["TX2"]][["success"]] )

    # Return the data
    return(datalist)
    
  }


