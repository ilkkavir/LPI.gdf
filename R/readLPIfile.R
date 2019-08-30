## file:readLPIfile.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Return the list ACF stored in an LPI result files
##
## Arguments:
##  fpath  data file path
##

readLPIfile <- function(fpath)
  {

    # Initialize ACF to NULL
    # the following load should overwrite this
    ACF <- NULL
    
    # load the data file
    load(fpath)

    # return the list "ACF"
    return(ACF)

  }

