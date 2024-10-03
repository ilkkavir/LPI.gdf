## file:mergeACFaltit.R
## (c) 2024- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##
##
##
## merge acf lists that cover different altitude ranges for easy plotting. 
## NOTICE: this is just for plotting, the lists are manipulated in a way that makes the insufficient for most other purposes!!
##
## INPUT:
##  acfs   a list of ACF lists as returned by readACF
##
##
mergeACFaltit <- function(acfs){

    ## find range gates and their limits (these are different for different beams...)
    ranges <- unlist(lapply(acfs,function(x){x$range}));names(ranges)<-NULL
    ranges.km <- unlist(lapply(acfs,function(x){x$range.km}));names(ranges.km)<-NULL
    wr <- unlist(lapply(acfs,function(x){diff(x$rangeLimits)}));names(wr)<-NULL
    rmin <- min(unlist(lapply(acfs,function(x){x$rangeLimits})))
    lags <- sort(unique(unlist(lapply(acfs,function(x){x$lag}))))
    lags.us <- sort(unique(unlist(lapply(acfs,function(x){x$lag.us}))))
    nlags <- length(lags) 
    
    ru <- unique(ranges)
    nru <- length(ru)
    wru <- rep(NA,nru)
    for(k in seq(nru)){
        wru[k] <- max(wr[ranges==ru[k]])
    }
    
    rlims <- rmin
    while(max(rlims)<max(ranges)){
        nextru <- min(ru[ru>max(rlims)])
        rlims <- c(rlims,max(rlims)+max(wru[ru <= nextru]))
    }

    nr <- length(rlims)-1
    rr <- rlims[1:nr] + diff(rlims)/2
    rr.km <- approx(ranges,ranges.km,rr)$y
    
    mergeACF <- list()
    mergeACF$ACF <- matrix(NA,nrow=nr,ncol=nlags)
    mergeACF$var <- matrix(NA,nrow=nr,ncol=nlags)
    mergeACF$lag <- lags
    mergeACF$lag.us <- lags.us
    mergeACF$range <- rr
    mergeACF$range.km <- rr.km
    mergeACF$timeString <- acfs[[1]]$timeString
    mergeACF$time.s <- acfs[[1]]$time.s
    mergeACF$llhT <- acfs[[1]]$llhT
    mergeACF$llhR <- acfs[[1]]$llhR
    mergeACF$azelT <- acfs[[1]]$azelT # assume that all measurements are from the same TX beam..
    mergeACF$radarFreq <- acfs[[1]]$radarFreq
    
    ## find the most accurate measurement from each range 
    for(k in seq(nr)){
        minerr <- Inf
        ii <- c(NA,NA)
        for(l in seq(length(acfs))){
            for(m in seq(length(acfs[[l]]$range))){
                if(acfs[[l]]$range[m] >= rlims[k] & acfs[[l]]$range[m] < rlims[k+1]){
                    if((tmperr <- sqrt(acfs[[l]]$var[m,1])/abs(acfs[[l]]$ACF[m,1])) < minerr){
#                    if(tmperr <- 1/Re(acfs[[l]]$ACF[m,1]) < minerr){
                        minerr <- tmperr
                        ii <- c(l,m)
                    }
                }
            }
        }
        if(!any(is.na(ii))){
            nl <- length(acfs[[ii[1]]]$lag)
            mergeACF$ACF[k,1:nl] <- acfs[[ii[1]]]$ACF[ii[2],]
            mergeACF$var[k,1:nl] <- acfs[[ii[1]]]$var[ii[2],]
        }
    }

    return(mergeACF)
    
}

