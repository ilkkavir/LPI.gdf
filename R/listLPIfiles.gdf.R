## file:listLPIfiles.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

## List files and sample indices needed for
## the given integration period.
##
## Arguments:
##  intPeriod Integration period number
##  LPIparam  A LPI parameter list
##
## Returns:
##  datalist A named list with elements
##            'files'  A character vector of file names
##            'n'      A numeric vector of number of samples
##                     to read from each file
##            'starts' A numeric vector of sample indices
##                     from which to start in each file
##

listLPIfiles.gdf <- function( intPeriod , LPIparam )
  {
    # A list for the file names and indices
    datalist <- list()
    datalist[["files"]] <- list()
    datalist[["starts"]] <- list()
    datalist[["n"]] <- list()

    for( XXN in c( "RX1" , "RX2" , "TX1" , "TX2" ) ){

      # Number of samples in between sampling start time
      # and analysis start time
#      sampleDiff <- round( ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN] )
      # add 1, because the dataStartTimes are for "falling edge" of a sample, IV 20151202. The difference is significant when
      # TX and RX are recorded with different sample rates
#      sampleDiff <- round( ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN] ) + 1
      # do not round yet...
      sampleDiff <- ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN]  + 1

      # Range of samples included in the integration period,
      # counted from the start of recording
#      intPerSampleRange <- sampleDiff + ceiling( c( (intPeriod - 1) , intPeriod ) * LPIparam[["timeRes.s"]] * LPIparam[["dataSampleFreqs"]][XXN] ) + c(1,0)
      # round, not ceiling. This is important if TX and RX are sampled differently
      intPerSampleRange <- round( sampleDiff + c( (intPeriod - 1) , intPeriod ) * LPIparam[["timeRes.s"]] * LPIparam[["dataSampleFreqs"]][XXN] ) + c(1,0)

      # List files only if the whole integration period
      # is after the sampling start time
      if( intPerSampleRange[1] > 0 ){

        # Range of file numbers included
        # in the integration period
        intPerFileRange <- ceiling( intPerSampleRange / LPIparam[["dataFileLengths"]][XXN] )

        # Number of data files
        intPerNfiles <- diff(intPerFileRange) + 1

        # A vector for the data file names
        datalist[["files"]][[XXN]] <- vector(mode="character",length=intPerNfiles)

        # A vector for the data start indices
        datalist[["starts"]][[XXN]] <- rep( 1, intPerNfiles )

        # A vector for the number of samples
        datalist[["n"]][[XXN]] <- rep( LPIparam[["dataFileLengths"]][XXN] , intPerNfiles )

        # Start index in the first file
        datalist[["starts"]][[XXN]][1] <- ceiling( intPerSampleRange[1] %% LPIparam[["dataFileLengths"]][XXN] )
        # Our indices start from 1, if the above line gives 0, it should actually be file length
        if(datalist[["starts"]][[XXN]][1]==0) datalist[["starts"]][[XXN]][1] <- LPIparam[["dataFileLengths"]][XXN]

        # Number of samples to read from the first file
        # (this will be corrected if the first is also last)
        datalist[["n"]][[XXN]][1] <- LPIparam[["dataFileLengths"]][XXN] - datalist[["starts"]][[XXN]][1] + 1

        # Number of samples to read from the last file
        datalist[["n"]][[XXN]][intPerNfiles] <- floor( intPerSampleRange[2] %% LPIparam[["dataFileLengths"]][XXN] ) - datalist[["starts"]][[XXN]][intPerNfiles] + 1

        # If all samples are read from the last (first) file
        # the row above gives a wrong result
        if( datalist[["n"]][[XXN]][intPerNfiles] <= 0 ) datalist[["n"]][[XXN]][intPerNfiles] <- LPIparam[["dataFileLengths"]][XXN] - datalist[["starts"]][[XXN]][intPerNfiles] + 1

        # Generate the actual file names
        for( k in 1:intPerNfiles){
          n <- intPerFileRange[1] + k - 1
          fstr <- paste( LPIparam[["fileNamePrefix"]][XXN] , paste(rep('0',(6-ceiling(log10(n+.1)))) , sep='',collapse='') , n , LPIparam[["fileNameExtension"]][XXN] , sep='',collapse='')
          datalist[["files"]][[XXN]][k] <- file.path( LPIparam[["dataDir"]][XXN] , fstr )
        }
      }
    }

    # Return the file and index list
    return(datalist)

  }
