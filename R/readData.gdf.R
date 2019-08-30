## file:latestSampleTime.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Read raw voltage data from gdf files
##
## Arguments:
##  files  A character vector of file paths
##  n      Number of samples to read from each file
##         (an integer vector)
##  istart Indices of the first samples to read
##         from each file (an integer vector)
##  bigEndian logical, is the data big-endian? default TRUE
##  controlBits logical, are there TX and PPS bits stored on
##         the lowest bits of Im/Re parts? Default TRUE
##  trybz2 logical, should we check for data file existence,
##         and look for a bzipped version of the requested file
##         does not exist? Default TRUE
##
## Returns:
##  gdfdata A named list with elements
##           cdata   complex sample vector
##           idatai  lowest bits in imaginary part (TX)
##           idatar  lowest bits in real part (PPS)
##           ndata   number of data points
##           success TRUE if all requested data was read
##

readData.gdf <- function(files,n=1e6,istart=1,bigEndian=TRUE,controlBits=TRUE,trybz2=TRUE)
  {

    # Number of files
    nfiles  <- as.integer(length(files))

    # Start index for each file. Indexing begins from 0 in c.
    istarts <- as.integer(rep(istart-1,length.out=nfiles))

    # Number of samples to read from each file
    ns      <- as.integer(rep(n,length.out=nfiles))

    # Total number of data samples to read
    ndata   <- as.integer(sum(ns));

    # End indices for each file
    iends   <- as.integer(istarts + ns - 1)

    # Select only files from which data will really be read.
    inds    <- ns > 0
    nfiles  <- as.integer(sum(inds))
    files   <- path.expand( files[inds] )
    istarts <- istarts[inds]
    iends   <- iends[inds]

    # Return now if nothing was left
    if(nfiles<=0) return(NULL)

    # check that files exist, unzip as necessary
    # this will be rather slow due to the disk write, but at least this works.
    # a faster version will be possible with some extra work
    if( trybz2 ){
        # a temporary directory for the decompressed files
        tmpdir <- tempdir()
        # a list of temporary file names for cleanup
        tempfiles <- c()
        for (k in seq(nfiles) ){
            # Do nothing if the file exists and is not compressed
            if(  substr(files[k],nchar(files[k])-3,nchar(files[k]))=='.gdf' & file.exists(files[k])) next
            # if a full name of a compressed file was given
            if ( substr(files[k],nchar(files[k])-3,nchar(files[k]))=='.bz2') bzf <- files[k]
            # else try to add the .bz2 extension
            if ( !file.exists(files[k]) ) bzf <- paste(files[k],'.bz2',sep='')
            # if the file exists try to decompress it
            if(file.exists(bzf)){
                # temporary file for the decompressed data
                tmpfile <- tempfile(pattern=as.character(Sys.getpid()),tmpdir=tmpdir,fileext='.gdf')
                # open a connection to the compressed file
                fcon <- bzfile(bzf,open='rb')
                # select endiannes (could we avoid this if 'raw' was used?)
                if(bigEndian){
                    # read the data and write immediately to the temporary file
                    writeBin(readBin(fcon,what='integer',size=2,endian='big',n=(iends[k]+1)*2),tmpfile,size=2,endian='big')
                }else{
                    writeBin(readBin(fcon,what='integer',size=2,endian='little',n=(iends[k]+1)*2),tmpfile,size=2,endian='little')
                }
                # close connection to the compressed file
                close(fcon)
                # replace the file name with the temprorary (uncompressed) one
                files[k] <- tmpfile
                # add the file name to list of temp files
                tempfiles <- c(tempfiles,tmpfile)
            }
        }
    }

    # Call the C function and return its output
    res <- .Call("read_gdf_data_R",
                 ndata       = ndata,
                 nfiles      = nfiles,
                 filepaths   = files,
                 istart      = istarts,
                 iend        = iends,
                 bigendian   = as.logical(bigEndian),
                 controlbits = as.logical(controlBits)
                 )

    # If trybz2 we will need to clean the temp files, the tempdir will be
    # removed when R is shut down
    if(trybz2) tryCatch(file.remove(tempfiles),error=function(e){0})

    return(res)


}
