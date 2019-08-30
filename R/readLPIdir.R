## file:readLPIdir.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Collect ACFs from LPI result files into an 3D array
##
## Arguments:
##  dpath     data path
##  recursive logical, if TRUE all subdirectories are
##            searched recursively
##  lags      time lag selection, any character string
##            is interpreted as 'all' (inlucde all lags)
##  stdThresh standard deviation threshold. Data points with
##            standard deviation larger than stdThresh times
##            median standard deviation in the profile are
##            replaced with NA
##
##  Returns:
##   reslist  A list with elements
##              ACF    an nRange x nLag x nACF  array of lag profile matrices
##              var    ACF variance estimates
##              time.s timestamps in seconds
##              timeString timestamps as character strings
##              nACF   number of lag profile matrices
##              nLag   number of time lags
##              nRange number of range gates
##
##             In addition, all other elements stored in the ACF
##             list of the last data file are copied to the output
##             (e.g. lag, range, etc. )
##

readLPIdir <- function(dpath,recursive=FALSE,lags='all',stdThresh=NULL)
  {

    if(is.null(dpath))   return(NULL)
    if(length(dpath)==0) return(NULL)

    fpath <- dpath[!file.info(dpath)$isdir]
    fpath <- fpath[length(grep('LP.Rdata',fpath))>0]
    dpath <- dpath[file.info(dpath)$isdir]

    # list the LPI result files
    flist <- dir(dpath[1],pattern='LP.Rdata',recursive=recursive,full.names=TRUE)
    if(length(dpath)>1){
      for(k in seq(2,length(dpath))){
        flist <- c(flist,dir(dpath[k],pattern='LP.Rdata',recursive=recursive,full.names=TRUE))
      }
    }
    flist <- c(flist,fpath)

    if(length(flist)==0) return(NULL)

    # read first of the data files
    dTmp <- readLPIfile(flist[1])

    # number of data files
    nFile  <- length(flist)

    # number of time lags
    if(is.character(lags)){
      nLag   <- length(dTmp$lag)
      lags   <- seq(nLag)
    }else{
      if(is.null(lags)){
        nLag <- 1
        lags <- c(1)
      }else{
        nLag <- length(lags)
      }
    }

    # number of range gates
    nRange <- length(dTmp$range)

    # allocate the necessary arrays
    ACF        <- array(0,dim=c(nRange,nLag,nFile))
    var        <- array(0,dim=c(nRange,nLag,nFile))
    time.s     <- vector(length=nFile,mode='numeric')
    timeString <- vector(length=nFile,mode='character')
    dTmp       <- vector(length=nFile,mode='list')


    # read the data from files
    for (k in seq(nFile)){
      dTmp             <- readLPIfile( flist[k] )
      ACF[,,k]         <- dTmp$ACF[,lags]
      var[,,k]         <- dTmp$var[,lags]
      # the time is under different name in older LPI versions
      if( !is.null(dTmp[["POSIXtime"]]) ) time.s[k]     <- as.double(dTmp$POSIXtime)
      if( !is.null(dTmp[["time.s"]]   ) ) time.s[k]     <- dTmp$time.s
      if( !is.null(dTmp[["timeString"]])) timeString[k] <- dTmp[["timeString"]]
    }

    if(!any(is.null(stdThresh))){
      rmInds <- rep(F,length(var[1,1,]))
      for(k in seq(nRange)){
        medvar        <- median(var[k,1,],na.rm=T)
        rmInds        <- rmInds | (sqrt(var[k,1,])<(stdThresh[1]*sqrt(medvar))) | (sqrt(var[k,1,])>(stdThresh[2]*sqrt(medvar)))
      }
      ACF[,,rmInds] <- NA
      var[,,rmInds] <- NA
      cat('Removed integration periods:\n')
      cat(paste(flist[intersect(which(rmInds),which(!is.na(rmInds)))],'\n'))
    }


    reslist                 <- dTmp
    reslist[["ACF"]]        <- ACF
    reslist[["var"]]        <- var
    reslist[["time.s"]]     <- time.s
    reslist[["timeString"]] <- timeString
    reslist[["nACF"]]       <- nFile
    reslist[["nLag"]]       <- nLag
    reslist[["nRange"]]     <- nRange
    reslist[["lag"]]        <- reslist[["lag"]][lags]
    reslist[["lag.us"]]     <- reslist[["lag.us"]][lags]

    return(reslist)

  }
