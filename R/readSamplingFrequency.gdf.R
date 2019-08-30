## file:readSamplingFrequency.gdf.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Try to read the sampling frequency from sampler log files
##
## Arguments:
##  dataDir    Data directory
##  samplerLog Log file name
##
## Returns:
##  freq       Sample rate if a proper log file was found,
##             NULL otherwise.
##

readSamplingFrequency.gdf <- function( dataDir , samplerLog )
  {

    # Initialise to NULL
    freq <- NULL

    # Construct the file name
    logFile <- file.path( dataDir , samplerLog )

    # If the file exists try to read it
    if(file.exists(logFile)){
        
      logLines <- readLines(logFile)

      # Look for line with the string "Sample rate"
      for(n in logLines){
          
        if( length( grep( 'Sample rate' , n ) ) > 0 ){

          # Make sure that the string was found
          # from a correct position
          if(grep('Sample rate',n,)==1){

            # Read the sample rate and convert it into Hz
            freq <- 1e6 * as.numeric( strsplit( strsplit( n , split='rate' )[[1]][2] , split = 'MHz' )[[1]][1] )

            # We are ready, break the loop
            break
          }
        }
      }
    }

    # Return the frequency
    return(freq)
    
  } 

