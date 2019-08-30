## file:readSamplingTimeTime.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Read sampling start time from timestamp file
##
## Arguments:
##  dataDirs  Data directory(ies)
##  stampFile Timestamp file name
##  prefix    File name prefix
##  nInt      Number of digits in gdf file number
##  extension File name extension
##
## Returns:
##  sTimes    Sampling start time(s)
##

readSamplingTime.gdf <- function( dataDirs , stampFile , prefix , nInt , extension )
  {

    sTimes <- rep(0,length(dataDirs))

    # Data file name
    dFile <- paste(prefix,substr('000000000000000000',1,nInt-1),'1',extension,sep='')
    # Seek from all listed directories
    for(k in seq(length(dataDirs))){
      sFile <- file.path(dataDirs[[k]],stampFile)
      # If the timestamp file is not found from the data
      # directory try a couple of steps upwards in hierarchy
      if(!file.exists(sFile)) sFile <- file.path(dataDirs[[k]],'..',stampFile)
      if(!file.exists(sFile)) sFile <- file.path(dataDirs[[k]],'..','..',stampFile)
      if(file.exists(sFile)){
        sLines <- readLines(sFile,100)
        for(n in sLines){
          if(length(grep(dFile,n))>0){
            if(grep(dFile,n)==1){
              tmpStr <- strsplit(n,split=dFile)[[1]][2]
              sTimes[k] <- as.numeric(substr(strsplit(n,split=dFile)[[1]][2],1,32))
            }
          }
        }
      }
    }

    # Return NULL if any of the log files was missing
    if(any(sTimes==0)) sTimes <- NULL

    return(sTimes)
    
  }

