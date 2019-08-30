## file:findOffset.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Find the offset in startTime using PPS bits in raw data
##
## Arguments:
##  dirn       Data directory path
##  prefix     File name prefix
##  nInt       Number of digits in gdf file names
##  extentsion File name extension
##  fLen       File length (number of complex samples)
##  sampFreq   Sample rate [Hz]
##  startTime  Sampling start time from timestamps
##             file (POSIX)
##
## Returns:
##  offset     Timestamp offset [s]
##

findOffset.gdf <- function( dirn , prefix , nInt , extension , fLen , sampFreq , startTime )
  {

    # Number of files in one second
    nfMax <- ceiling(sampFreq/fLen)

    # Vector for one second of PPS data
    nt <- nfMax * fLen
    PPSvec <- rep(F,nt)
  
    # List data files in the directory
    df  <- dir(dirn,pattern=prefix,full.names=T)
    df  <- df[grep(extension,df)]
    
    # First data file
    df1 <- df[1]
    
    # Number of characters in the whole file path
    ncdf1 <- nchar(df1)
    
    # Number of the first file
    firstFile <- as.integer(substr(df1,ncdf1-nInt-3,ncdf1-4))
    
    # Read the data
    for(k in seq(nfMax)){
      kchar <- as.character(k+firstFile-1)
      fname <- file.path(dirn,paste(prefix,substr('000000000000',1,nInt-nchar(kchar)),kchar,sep='',extension))
      PPSvec[ ((k-1)*fLen+1) : (k*fLen) ] <- readData.gdf(fname,fLen)$idatar
    }

    # Exactly one second of PPS data
    PPSvec <- PPSvec[1:sampFreq]

    secLoc <- which( diff( PPSvec ) == 1 )
    
    if( length( secLoc ) == 0 ){
      if( PPSvec[1] ){
        secLoc <- 1
      }else{
        return(0)
      }
    }
    
    # If the PPS pulses really are recorded in the lowest
    # bit, there should be only one point in secLoc
    if( length( secLoc ) > 1 ) return(0)
    
    secLoc <- secLoc 
    
    # Expected secLoc, based on startTime, taking into
    # account that we did not necessarily read from
    # the first data file:

    # Expected time in seconds (in data file number one)
    secLocExpect <- 1 - startTime + trunc(startTime)
    
    # Expected time in seconds (in the data file number firstFile)
    secLocExpect <-  (secLocExpect - (firstFile-1)*fLen/sampFreq)%%1
    
    # Conversion to sample units
    secLocExpect <- trunc(secLocExpect*sampFreq) + 1                 
    
    
    offset <- secLocExpect - secLoc

    # In absence of better knowledge, we assume that
    # the offset is always less than half a second
    while(abs(offset)>(sampFreq/2)) offset <- offset - sign(offset)*sampFreq
    
    return(offset/sampFreq)
    
  }

