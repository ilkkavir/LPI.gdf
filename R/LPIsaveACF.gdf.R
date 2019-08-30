## file:LPIsaveACF.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Save resolved ACF to file
##
## Arguments:
##  LPIparam  A LPI parameter list
##  intPeriod Integration period number
##  ACF       An ACF list returned by LPIsolve
##
## Returns:
##  resFile   Result file name
##

LPIsaveACF.gdf <- function( LPIparam , intPeriod , ACF )
  {

    # mirror the spectrum if LPIparam[["mirrorSpectra"]] is set
    if( LPIparam[["mirrorSpectra"]] ){
        ACF[["ACF"]] <- Conj(ACF[["ACF"]])
        if(LPIparam[["fullCovar"]]) ACF[["covariance"]] <- Conj(ACF[["covariance"]])
    }

    # Divide the estimated ACF with filter length in metres in order to
    # get power per metre.
    # Background ACF is not scaled, because it does not have a range
    # ambiguity function!
    acfscale <- LPIparam[["filterLength.us"]] * 299.792458 / 2
    ACF[["ACF"]] <- ACF[["ACF"]] / acfscale
    ACF[["var"]] <- ACF[["var"]] / acfscale ** 2
      
    # Number of range gates
    ngates <- length(ACF[["range"]])

    # Number of lags
    nlags  <- length(ACF[["lag"]])
    
    # Seconds since 1970
    ACF[["time.s"]] <-  LPIparam[["startTime"]] + intPeriod*LPIparam[["timeRes.s"]]

    # The same time as a string, useful for debugging
    # time conversions and for plotting
    ACF[["timeString"]] <-
      format( as.POSIXct( ACF[["time.s"]] , origin='1970-01-01' , tz='UTC' ) , "%Y-%m-%d %H:%M:%OS3 UT")

    # Result file name
    resFile <- file.path( LPIparam[["resultDir"]] , paste( sprintf( '%13.0f' , trunc( ACF[["time.s"]]  * 1000 ) ) , "LP.Rdata" , sep='') )

    # Range
    names(ACF[["range"]]) <- paste('gate',seq(ngates),sep='')

    # Lag
    names(ACF[["lag"]]) <- paste('lag',seq(nlags),sep='')

    # Background ACF
    ACF[["backgroundACF"]] <- ACF[["ACF"]][(ngates+1),]
    ACF[["backgroundvar"]] <- ACF[["var"]][(ngates+1),]
    names(ACF[["backgroundACF"]]) <- paste('lag',seq(nlags),sep='')
    names(ACF[["backgroundvar"]]) <- paste('lag',seq(nlags),sep='')

    # ACF and variance without the background samples
    ACF[["ACF"]] <- matrix(ACF[["ACF"]][1:ngates,],ncol=nlags)
    ACF[["var"]] <- matrix(ACF[["var"]][1:ngates,],ncol=nlags)
    dimnames(ACF[["ACF"]]) <- list(paste('gate',seq(ngates),sep=''),paste('lag',seq(nlags),sep=''))
    dimnames(ACF[["var"]]) <- list(paste('gate',seq(ngates),sep=''),paste('lag',seq(nlags),sep=''))

    # Dimnames for the optional full covariance matrix
    if(LPIparam[["fullCovar"]]) dimnames(ACF[["covariance"]]) <- list( c(paste('gate',seq(ngates),sep=''),'background') , c(paste('gate',seq(ngates),sep=''),'background') , paste('lag',seq(nlags),sep=''))
    
    # Strip off skipped time lags
#    laginds <- apply( ACF[["ACF"]] , FUN=function(x){ any( !is.na( x ) ) } , MARGIN = 2 )
    laginds <- which( c( LPIparam[["maxRanges"]] , rep( LPIparam[["maxRanges"]][length(LPIparam[["maxRanges"]])] , nlags ))[1:nlags] >= LPIparam[["rangeLimits"]][1] )
    ACF <- stripACF( ACF , rgates = seq( ngates ) , lags=laginds , fullCovar=LPIparam[["fullCovar"]])

    # Lag in us
    ACF$lag.us <- ACF$lag * LPIparam[["filterLength.us"]]

    # Range in km
    ACF$range.km <- ACF$range * LPIparam[["filterLength.us"]] * .299792458 / 2

    # Copy transmitter and receiver information from LPIparam
    ACF[["llhT"]] <- LPIparam[["llhT"]]
    ACF[["llhR"]] <- LPIparam[["llhR"]]
    ACF[["azelT"]] <- LPIparam[["azelT"]]
    ACF[["azelR"]] <- LPIparam[["azelR"]]
    ACF[["radarFreq"]] <- LPIparam[["radarFreq"]]
    
    # Range gate limits
    ACF[["rangeLimits"]] <- LPIparam[["rangeLimits"]]
    names(ACF[["rangeLimits"]]) <- ""

    # Lag integration limits
    ACF[["lagLimits"]] <- LPIparam[["lagLimits"]]
    names(ACF[["lagLimits"]]) <- ""

    # Maximum ranges
    ACF[["maxRanges"]] <- LPIparam[["maxRanges"]]
    names(ACF[["maxRanges"]]) <- ""

    # Range gate limits in km
    ACF[["rangeLimits.km"]] <- ( LPIparam[["rangeLimits"]] -0.5 ) * LPIparam[["filterLength.us"]] * .299792458 / 2
    names(ACF[["rangeLimits.km"]]) <- ""

    # Lag integration limits in us
    ACF[["lagLimits.us"]] <- ( LPIparam[["lagLimits"]] - 0.5 ) * LPIparam[["filterLength.us"]]
    names(ACF[["lagLimits.us"]]) <- ""

    # Maximum ranges in km
    ACF[["maxRanges.km"]] <- LPIparam[["maxRanges"]] * LPIparam[["filterLength.us"]] * .299792458 / 2
    names(ACF[["maxRanges.km"]]) <- ""

    # Add the copy of the LPI.gdf call to the output list
    ACF[["functionCall"]] <- LPIparam[["LPI.gdfCall"]]

    # Write the output list to the file
    save( ACF=ACF , file=resFile )

    # Return the file name invisibly
    invisible( resFile )

  }
