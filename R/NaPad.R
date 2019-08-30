## file:NaPad.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Pad NA values in places of apprently skipped lag profiles
##
## Arguments:
##  ACF An acf list
##
## Returns:
##  ACF A padded acf list
##

NaPad <- function(ACF)
  {
    
    if(length(ACF$lag)==1) return(ACF)
    
    # the smallest lag difference in the data
    minDiff <- min(diff(ACF$lag))

    # make a new lag axis
    lag2 <- sort(ACF$lag)
    
    k <- 2
    while(k<length(lag2)){
      if( (lag2[k]-lag2[k-1]) > (2*minDiff)){
        lag2 <- c(lag2[1:(k-1)],(lag2[k-1]+minDiff),lag2[k:length(lag2)])
      }
      k <- k+1
    }
    
    # if nothing changed
    if(all(lag2==ACF$lag)) return(ACF)

    # actual padding when necessary
    ACF2 <- matrix(NA,nrow=length(ACF$range),ncol=length(lag2))
    var2 <- matrix(NA,nrow=length(ACF$range),ncol=length(lag2))
    
    for(k in seq(length(ACF$lag))){
      for(n in seq(length(lag2))){
        if(ACF$lag[k]==lag2[n]){
          ACF2[,n] <- ACF$ACF[,k]
          var2[,n] <- ACF$var[,k]
          break
        }
      }
    }
    
    ACF$ACF <- ACF2
    ACF$var <- var2
    ACF$lag <- lag2
    
    return(ACF)
    
  }

