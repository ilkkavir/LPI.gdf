## file:UHFazelUpdate.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## LPI parameter list update function
##
## Update the range limits using the UHFazel.log file
## so that the height gate limits do not change when
## the antenna moves. The LPIparam list must contain the
## entry LPIparam$rangeLimits.km, which must initially contain
## the range gate limits for a zenith-pointed beam
##
## Arguments:
##  LPIparam  A LPI parameter list
##  intPeriod Integration period number
##
## Returns:
##  LPIparam The LPIparam list with modified rangeLimits in the first call
##           NULL in the second call
##

UHFazelUpdate <-  function( LPIparam , intPeriod )
    {

      if(!is.null(LPIparam[["callN"]])) return(NULL)

      LPIparam[["callN"]] <- 1

      # read the log file
      azelt <- matrix(
        as.numeric(
          unlist(
            sapply(
              strsplit(
                readLines( LPIparam[["UHFazellog"]] ),' '
                ),
              function(x){return(x[1:3])}
              )
            )
          ),
        ncol=3,byrow=T
        )

      # start and end time of this integration period
      iperstart <- LPIparam[["startTime"]] + LPIparam[["timeRes.s"]] * ( intPeriod - 1 )
      iperend <- iperstart + LPIparam[["timeRes.s"]]

      # antenna pointings during this integration

      # pointing at beginning of the integration period
      azelt0 <- c(0,0,0)
      if(any(azelt[,3]<=iperstart)){
        azelt0 <- azelt[ max( which( azelt[,3] <= iperstart ) ) , ]
      }

      # pointing at end of the integration period
      azelt1 <- c(0,0,0)
      if(any(azelt[,3]<=iperend)){
        azelt1 <- azelt[ max( which( azelt[,3] <= iperend ) ) , ]
      }

      azel <- ( azelt0[1:2] + azelt1[1:2] ) / ( ( azelt0[3]>0) + (azelt1[3]>0))
      
      if(!any(is.na(azel))) LPIparam[["azelT"]] <- LPIparam[["azelR"]] <- azel
      
      # convert the heights in LPIparam$rangeLimits.km into ranges

      r <- LPIparam[["rangeLimits.km"]]
      for(k in seq(length(LPIparam[["rangeLimits.km"]]))){
        r[k] <- height2range( llhT=LPIparam[["llhT"]] , llhR=LPIparam[["llhR"]] , azelT=LPIparam[["azelT"]] , h=LPIparam[["rangeLimits.km"]][k]*1000)
      }

      LPIparam[["rangeLimits"]] <- sort(unique( floor( r / 299.792458 * 2 / LPIparam[["filterLength.us"]] + 1e-5 ) ) )

      LPIparam[["rangeLimits.km"]] <- LPIparam[["rangeLimits"]] * .299792458 / 2 * LPIparam[["filterLength.us"]]
      
      
      return( LPIparam )
      
    }
        
