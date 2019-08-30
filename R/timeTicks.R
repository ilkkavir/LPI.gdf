## file:timeTicks.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Time axis tick marks
##
## Arguments:
##  times A vector of times [s], only the smallest and largest
##        value are used
##  tickres Tick mark resolution in seconds
##
## Returns:
##  A list with elements "tick" (tick mark positions)
##  and "string" (tick mark labels)
##

timeTicks <- function(times,tickres=NULL)
  {
    
    ttick <- vector()
    tstr <- character()
    result <- list()
    if (is.null(tickres)){
      ttot <- max(times)-min(times)
      tickres <- 240
      if (ttot<(60*60*24)){
        tickres <- 120
      }
      if (ttot<(60*60*12)){
        tickres <- 60
      }
      if (ttot<(60*60*6)){
        tickres <- 30
      }
      if (ttot<(60*60*3)){
        tickres <- 15
      }
      if (ttot<(60*60*2)){
        tickres <- 10
      }
      if (ttot<(60*60)){
        tickres <- 5
      }
      if (ttot<(60*5)){
        tickres <- 1
      }
    }
    
    time_tmp <- seq(min(times),max(times))
    ttick <- floor(time_tmp/(60*tickres))*60*tickres
    l<-2
    for (k in seq(2,length(time_tmp))){
      if (ttick[k]>ttick[k-1]){
        ttick[l]<-ttick[k]
        l<-l+1
      }
    }
    ttick<-ttick[seq(1,(l-1))]
    for (k in seq(1,length(ttick))){
      tstr[k] <- '00:00'
      if (as.integer(((ttick[k]/(60*60*24))%%1)*24+.01)<10){
        substr(tstr[k],2,2)<-as.character(floor(((ttick[k]/(60*60*24))%%1)*24+.01))
      }else{
        substr(tstr[k],1,2)<-as.character(floor(((ttick[k]/(60*60*24))%%1)*24+.01))
      }
      if (as.integer(((ttick[k]/(60*60))%%1)*60+.01)<10){
        substr(tstr[k],5,5)<-as.character(floor(((ttick[k]/(60*60))%%1)*60+.1))
      }else{
        substr(tstr[k],4,5)<-as.character(floor(((ttick[k]/(60*60))%%1)*60+.1))
      }
    }
    
    result[['tick']] <- ttick
    result[['string']] <- tstr
    
    return(result)
    
  }
	
