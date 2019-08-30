## file:LPI.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## A wrapper to the function LPI::LPI
##
## Arguments:
##   ... Arguments to be passed for collectLPIparam.gdf that
##       constructs the final LPI parameter list
##
## 

LPI.gdf <- function( ... )
  {
    # Collect the LPI parameter list from default values
    # and optional input arguments.
    LPIparam  <- collectLPIparam.gdf( ... )

    # add the full argument list to the LPI list, this will be saved
    # in all data files. Coersed into a listed in order to avoid
    # Inf loop when LPI calls eval with all elements of LPIparam.
    # The call can be re-evaluated with eval(as.call(LPIparam$LPI.gdfCall))
    LPIparam[["LPI.gdfCall"]] <- as.list( match.call(expand.dots=TRUE) )

    # Call the main analysis loop of LPI
    do.call( LPI , LPIparam )

    # This function does not return anything,
    # results are written to files.
    invisible()

  }

