## file:collectLPIparam.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Collect the final LPI parameter list from defaults and
## input argument list. If the input list contains the
## entry "paramFile", the file will be loaded and its
## contents will be added to the parameter list. Values
## read from file will override the defaults, and
## explicitly given command line arguments will override
## both the defaults and anything read from file.
##
## Arguments:
##  ... A list of named command line arguments
##
## Returns:
##  LPIparam A named list of analysis parameters
##

collectLPIparam.gdf <- function( ... )
  {

    # Default parameters
    LPIparamCol <- LPIparam.default.gdf()

    # Make a list of the optional input arguments
    pars <- list( ... )

    # If there is an entry named "paramFile"
    # load the file and add its contents to LPIparamCol
    parfile  <- pars[['paramFile']]
    if(!is.null(parfile)){
      filepars <- get(load(parfile))
      LPIparamCol[names(filepars)] <- filepars
    }

    # Explicit input arguments will override everything else.
    LPIparamCol[names(pars)] <- pars

    # Make sure that the parameter file name is
    # *not* included in the return list.
    # Otherwise it could be written in file and
    # cause problems when using the file as input.
    LPIparamCol[['paramFile']] <- NULL

    # Expand arguments that are accepted in several formats
    # to the internally used named lists or vectors with
    # entries "RX1", "RX2", "TX1", and "TX2".
    LPIparamCol[["freqOffset.Hz"]] <- LPIexpand.input( LPIparamCol[["freqOffset.Hz"]] )
    # If index shifts is given as vector,
    # convert it into a list before the expansion
    if( ! is.list( LPIparamCol[["indexShifts.us"]] ) ){
      LPIparamCol[["indexShifts.us"]] <- list(LPIparamCol[["indexShifts.us"]])
    }
    LPIparamCol[["indexShifts.us"]] <- LPIexpand.input( LPIparamCol[["indexShifts.us"]] )
    LPIparamCol[["maxClutterRange.km"]] <- LPIexpand.input( LPIparamCol[["maxClutterRange.km"]] )
    LPIparamCol[["clutterFraction"]] <- LPIexpand.input( LPIparamCol[["clutterFraction"]] )

    # Create the output directory and save the parameters in
    # <resultDir>/LPIparam.Rdata
    LPIparam <- LPIparamCol
    if( !is.na( LPIparamCol[["resultDir"]])){
      dir.create( LPIparamCol[["resultDir"]] , showWarnings=FALSE , recursive=TRUE     )
      save( LPIparam , file=file.path( LPIparamCol[["resultDir"]] , "LPIparam.Rdata" ) )
    }

    #####################################################
    ## Create the final internally used parameter list ##
    #####################################################



    # Call the additional functions listed in "extraFunctions"
    # First load the additional packages because the extra
    # fucntions may be included in them
    for( pn in LPIparam[["inputPackages"]] ){
      require( pn , character.only=TRUE )
    }
    for( fn in LPIparam[["extraFunctions"]] ){
      tmplist <- eval( as.name( fn ) )( LPIparamCol)
      LPIparamCol[names(tmplist)] <- tmplist
    }

    # Sample rates
    LPIparamCol[["dataSampleFreqs"]] <- eval(as.name(LPIparamCol[["sampleRateFunction"]]))( LPIparamCol )

    # Sampling start times
    LPIparamCol[["dataStartTimes"]] <- eval(as.name(LPIparamCol[["startTimeFunction"]]))( LPIparamCol )

    # Last available data samples (these will be
    # updated repeatedly in  the analysis loop)
    LPIparamCol[["dataEndTimes"]]    <- eval(as.name(LPIparamCol[["dataEndTimeFunction"]]))( LPIparamCol )

    # Conversion of beginTime to seconds, doubles used
    # instead of POSIXct objects in order to enable
    # all arithmetics
    LPIparamCol[["startTime"]] <- as.double( ISOdate( LPIparamCol[["beginTime"]][1] , LPIparamCol[["beginTime"]][2] , LPIparamCol[["beginTime"]][3] , LPIparamCol[["beginTime"]][4] , LPIparamCol[["beginTime"]][5] , floor( LPIparamCol[["beginTime"]][6] ) ) ) + LPIparamCol[["beginTime"]][6]%%1

    # Conversion of endTime to seconds, doubles instead of
    # POSIXct objects in order to enable all arithmetics
    LPIparamCol[["stopTime"]] <- as.double( ISOdate( LPIparamCol[["endTime"]][1] , LPIparamCol[["endTime"]][2] , LPIparamCol[["endTime"]][3] , LPIparamCol[["endTime"]][4] , LPIparamCol[["endTime"]][5] , floor( LPIparamCol[["endTime"]][6] ) ) ) + LPIparamCol[["endTime"]][6]%%1

    # The first integration period, counted from startTime,
    # that is actually available. .01 s tolerance to make
    # sure that data vector length will never be zero
    LPIparamCol[["firstIntPeriod"]]  <- max( 1 , ceiling(  ( max( unlist( LPIparamCol[["dataStartTimes"]] )) - LPIparamCol[["startTime"]] + .01 ) /  LPIparamCol[["timeRes.s"]] ) )

    # The last integration period to analyze
    LPIparamCol[["lastIntPeriod"]]   <- floor( ( LPIparamCol[["stopTime"]] - LPIparamCol[["startTime"]] ) /  LPIparamCol[["timeRes.s"]] )

    # Update startTime to match with the data start time
    LPIparamCol[["startTime"]] <- LPIparamCol[["startTime"]] + ( LPIparamCol[["firstIntPeriod"]] - 1 ) * LPIparamCol[["timeRes.s"]]

    #################################################
    # Conversions from SI units to sample intervals #
    #################################################

    # Frequency offsets
    dTypes <- c( "RX1" , "RX2" , "TX1" , "TX2" )
    LPIparamCol[["freqOffset"]] <- rep( NA , 4 )
    names(LPIparamCol[["freqOffset"]]) <- dTypes
    for( XXN in dTypes ){
      LPIparamCol[["freqOffset"]][XXN] <- LPIparamCol[["freqOffset.Hz"]][XXN] / LPIparamCol[["dataSampleFreqs"]][XXN]
    }

    # Receiver filter length and upsampling factor
    LPIparamCol[["filterLength"]] <- rep( NA , 4 )
    LPIparamCol[["nup"]] <- rep( NA , 4 )
    names(LPIparamCol[["filterLength"]]) <- dTypes
    names(LPIparamCol[["nup"]]) <- dTypes
    for( XXN in dTypes ){
      LPIparamCol[["nup"]][XXN] <- as.integer( which( ( ( LPIparamCol[["filterLength.us"]] / ( ( 1 / ( LPIparamCol[["dataSampleFreqs"]][XXN] / 1e6 ) ) / seq(10000) ) ) %% 1) == 0 )[1] )
      if( is.na( LPIparamCol[["nup"]][XXN] )) stop("Could not find suitable resampling for sample frequency ", LPIparamCol[["dataSampleFreqs"]][XXN], "Hz ", "and filter length", LPIparamCol[["filterLength.us"]], " us." )
      LPIparamCol[["filterLength"]][XXN] <- round( LPIparamCol[["filterLength.us"]] * LPIparamCol[["dataSampleFreqs"]][XXN] / 1e6 * LPIparamCol[["nup"]][XXN])
    }

    # Lag limits
    LPIparamCol[["lagLimits"]] <- sort( unique( floor( LPIparamCol[["lagLimits.us"]] / LPIparamCol[["filterLength.us"]] + 1e-5 ) ) )

    # Range gate limits
    LPIparamCol[["rangeLimits"]] <- sort(unique( floor( LPIparamCol[["rangeLimits.km"]] / .299792458 * 2 / LPIparamCol[["filterLength.us"]] + 1e-5 ) ) )

    # Maximum range at each lag
    LPIparamCol[["maxRanges"]] <- ceiling( LPIparamCol[["maxRanges.km"]] / .299792458 * 2 / LPIparamCol[["filterLength.us"]] )

    # Maximum range for clutter suppression
    LPIparamCol[["maxClutterRange"]] <- ceiling( LPIparamCol[["maxClutterRange.km"]] / .299792458 * 2 / LPIparamCol[["filterLength.us"]] )

    # Corrections to TX and RX indices
    LPIparamCol[["indexShifts"]] <- rep( list( c(NA,NA) ) , 4 )
    names(LPIparamCol[["indexShifts"]]) <- dTypes
    for( XXN in dTypes ){
      # Make sure that each element of indexshifts.us
      # is  a vector of length two
      LPIparamCol[["indexShifts.us"]][[XXN]] <- rep( LPIparamCol[["indexShifts.us"]][[XXN]] , length.out=2 )
      LPIparamCol[["indexShifts"]][[XXN]] <- round( LPIparamCol[["indexShifts.us"]][[XXN]] * LPIparamCol[["dataSampleFreqs"]][XXN] / 1e6 )
    }

    # Convert filter name to lower-case
    LPIparamCol[["decodingFilter"]] <- tolower(LPIparamCol[["decodingFilter"]])

    # Only the solvers rlips and ffts can provide
    # a full posterior covariance matrix,  print
    # warning if this is requested with other solvers
    if( LPIparamCol[["fullCovar"]] & !any(LPIparamCol[["solver"]]==c("rlips","fishs"))){
      LPIparamCol[["fullCovar"]] <- FALSE
      warning("The inverse problem solver cannot provide full covariance matrix, setting fullCovar=FALSE. Use solver='rlips' or solver='fishs' to get a full covariance matrix")
    }

    return(LPIparamCol)

  }
