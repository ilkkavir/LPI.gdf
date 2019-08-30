## file:fileLength.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Number of complex samples in a gdf file.
##
## Arguments:
##  ddir      Data directory path
##  prefix    File name prefix
##  extension File name extension
##
## Returns:
##  fileLength Number of complex samples in a single file
##

fileLength.gdf <- function( ddir , prefix , extension )
  {

    # Select files that have proper beginning and end,
    # and only digits in between.
    df <- dir( ddir , pattern=paste(prefix,"[[:digit:]]*",extension,sep='') , full.names=TRUE )

    # Stop if there are no data files
    if( length(df) == 0 ) stop(sprintf("No valid data files found from directory '%s' \n" , ddir ))


    # the file could be bzipped
    if (substr(df[1],nchar(df[1])-3,nchar(df[1]))=='.bz2'){
        fcon <- bzfile(df[1],open='rb')
    } else {
        fcon <- file(df[1],open='rb')
    }

    # then count the samples
    nd <- 0
    while( (ndd <- length(readBin(fcon,what='integer',size=4,n=1e6)))==1e6 ){
        nd <- nd + 1e6
    }
    nd <- nd + ndd
    close(fcon)


    return(nd)


#    # Read file size and convert it to
#    # number of complex samples
#    return(file.info(df[1])$size/4)

  }
