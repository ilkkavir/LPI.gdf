## file:LPIexpandInput.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Expansion of gdf specifix input arguments into
## internally used format
##
## Arguments:
##  LPIparam A LPI parameter list
##
## Returns:
##  outlist  A list with elements dataDir, fileNamePrefix
##           and fileNameExtension. 
## 

LPIexpandInput.gdf <- function( LPIparam )
  {

    outlist <- list()
    
    outlist[["dataDir"]] <- unlist(LPIexpand.input( LPIparam[["dataDir"]] ))
    outlist[["fileNamePrefix"]] <- unlist(LPIexpand.input( LPIparam[["fileNamePrefix"]] ))
    outlist[["fileNameExtension"]] <- unlist(LPIexpand.input( LPIparam[["fileNameExtension"]] ))


    return( outlist )


  }
