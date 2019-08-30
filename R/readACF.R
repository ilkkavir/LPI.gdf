## file:readACF.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##
## An average of all lag profile mamtrices in a directory
##
## Arguments:
##  dpath     data path(s), a directory, a vector of file names
##            or a combination of these
##  lags      lag selection
##  ranges    range gate selection
##  stdThrsh  standard deviation threshold, data points with standard
##            deviation larger than stdThrsh are not used in the average
##
## Returns:
##  ACF an ACF list of the averaged data
## 

readACF <- function( dpath , lags=NULL , ranges=NULL , stdThrsh=Inf )
  {
    
    if(is.null(dpath))   return(NULL)
    if(length(dpath)==0) return(NULL)
    
    fpath <- dpath[!file.info(dpath)$isdir]
    fpath <- fpath[grep('LP.Rdata',fpath)]
    dpath <- dpath[file.info(dpath)$isdir]
    
    # list the LPI result files
    flist <- dir(dpath[1],pattern='LP.Rdata',full.names=TRUE)
    if(length(dpath)>1){
      for(k in seq(2,length(dpath))){
        flist <- c(flist,dir(dpath[k],pattern='LP.Rdata',full.names=TRUE))
      }
    }
    flist <- c(flist,fpath)
    
    if(length(flist)==0) return(NULL)
    
    # number of data files
    nFile  <- length(flist)
    
    # read the last data file
    load(flist[nFile])

    # number of time lags
    if(is.null(lags)){
      nLag <- length(ACF$lag)
      lags <- seq(nLag)
    }else{
      lags <- lags[lags<length(ACF$lag)]
      lags <- lags[lags>0]
      nLag <- length(lags)
    }
    
    
    # number of range-gates
    if(is.null(ranges)){
      nRange   <- length(ACF$range)
      ranges   <- seq(nRange)
    }else{
      ranges <- ranges[ranges<length(ACF$range)]
      ranges <- ranges[ranges>0]
      nRange <- length(ranges)
    }
    

    # allocate the necessary arrays
    ACFave        <- array(0,dim=c(nRange,nLag))
    varave        <- array(0,dim=c(nRange,nLag))
    BG            <- rep(0+0i,nLag)
    BGvar         <- rep(0+0i,nLag)
    
    # read the data and average
    for( fn in flist){
      load(fn)
      ind <- ( sqrt(ACF$var[ranges,lags]) < stdThrsh ) & (!is.na(ACF$ACF[ranges,lags]))
      ind[is.na(ind)] <- FALSE
      ACFave[ind] <- ACFave[ind] + (ACF$ACF[ranges,lags] / ACF$var[ranges,lags])[ind]
      varave[ind] <- varave[ind] + (1/ACF$var[ranges,lags])[ind]
      BG     <- BG + ACF$backgroundACF[lags] / ACF$backgroundvar[lags]
      BGvar  <- BGvar + 1/ACF$backgroundvar[lags]
    }
    varave <- 1/varave
    ACFave <- ACFave * varave
    BGvar  <- 1/BGvar
    BG     <- BG * BGvar
    
    
    # replace the values in the last data file with the averaged ones and return
    ACF[["ACF"]] <- ACFave
    ACF[["var"]] <- varave
    ACF[["backgroundACF"]] <- BG
    ACF[["backgroundvar"]] <- BGvar
    ACF[["lag"]] <- ACF[["lag"]][lags]
    ACF[["lag.us"]] <- ACF[["lag.us"]][lags]
    ACF[["range"]] <- ACF[["range"]][ranges]
    ACF[["range.km"]] <- ACF[["range.km"]][ranges]
    ACF[["nGates"]] <- nRange
    ACF[["covariance"]] <- NULL
    
    return(ACF)
    
  }
