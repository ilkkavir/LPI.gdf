## file:mergeACF.R
## (c) 2024- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##
##
##
## merge acf lists for easy plotting.
## NOTICE: this is just for plotting, the lists are manipulated in a way that makes the insufficient for most other purposes!!
##
## The inputs must be ordered so that the lag values are monotonically increasing, the first one has largest number of range gates, and
## the subsequent ones have the same range gates with the first one (but gates may be missing from the top)
##
##
##
mergeACF <- function(acf1,acf2=NULL,...){
    if(is.null(acf2)){
        return(acf1)
    }

    acfmerge <- list()
    acfnames <- names(acf1)
    for(nn in acfnames){
        if(!any(nn==c('ACF','nGates','lagFLOP','rangeLimits.km','var','backgroundACF','analysisTime','lagAddTime','rangeLimits','latLimits.us','lag','backgroundvar','FLOP','lag.us','lagLimits','lagLimits.us','maxRanges.km','range','addTime','range.km','maxRanges','functionCall'))){
            if(any(acf1[[nn]]!=acf2[[nn]])){
                stop(paste('Values of ',nn,'are not identical'))
            }
            acfmerge[[nn]] <- acf1[[nn]]
        }
    }

    dim1 <- dim(acf1$ACF)
    dim2 <- dim(acf2$ACF)
    if(dim2[1]>dim1[1]){
        stop('The first input must have largest number of range gates')
    }
    ## number of ranges, 
    nr <- dim1[1]
    nl <- dim1[2] + dim2[2] + 1
    acfmerge$ACF <- acfmerge$var <- matrix(NA,nrow=nr,ncol=nl)
    ## lag step in acf1
    lagdiff1 <- mean(diff(acf1$lag))
    lagdiff1.us <- mean(diff(acf1$lag.us))
    ## add one point in the lag vector to produce nice plots
    acfmerge$lag <- c(acf1$lag,acf1$lag[dim1[2]]+lagdiff1,acf2$lag)
    acfmerge$lag.us <- c(acf1$lag.us,acf1$lag.us[dim1[2]]+lagdiff1.us,acf2$lag.us)
    ## range from acf1
    acfmerge$range <- acf1$range
    acfmerge$range.km <- acf1$range.km
    nr2 <- dim2[1]

    ## the merged ACF and variance vectors
    acfmerge$ACF[,1:dim1[2]] <- acf1$ACF
    acfmerge$var[,1:dim1[2]] <- acf1$var

    acfmerge$ACF[1:nr2,(dim1[2]+2):nl] <- acf2$ACF
    acfmerge$var[1:nr2,(dim1[2]+2):nl] <- acf2$var
    ## repeat the first lag profile from acf2 to get the pixels right in plotACF
#    acfmerge$ACF[1:nr2,(dim1[2]+1)] <- acf2$ACF[,1]
 #   acfmerge$var[1:nr2,(dim1[2]+1)] <- acf2$var[,1]

    ## call recursively to add the remaining ones
    return(mergeACF(acfmerge,...))
    
}
