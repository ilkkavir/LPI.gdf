## file:plotLagProfiles.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Color-coded image of lag profiles as function of time
##
## Arguments:
##  dpath         Data path
##  lags          Lag numbers to include
##  tLim          Time axis limits in hours
##  ylim          y-axis limits
##  rzlim         z-axis limits for the real part plot
##  izlim         z-axis limits for the imaginary part plot
##  errlim        z-axis limits for the standard deviation plot
##  pdf           pdf format output file name
##  jpg           jpeg format output file name
##  figNum        Figure number when plotting to an existing device
##  tickRes       Time tick resolution
##  width         Plot width
##  height        Plot height
##  paper         Plot paper size when printing to file
##  res           Resolution of jpeg plots
##  cex           Magnification in labels etc.
##  re            Include real part plot (TRUE/FALSE)
##  im            Include imaginary part plot (TRUE,FALSE)
##  mod           Include modulus (amplitude) plot (TRUE/FALSE)
##  arg           Include argument (phase) plot (TRUE/FALSE)
##  modlim        z-axis limits for the modulus limits
##  arglim        z-axis limits for the argument plot
##  err           Include standard deviation plot (TRUE/FALSE)
##  f             Function to apply to the data before plotting
##  col           Figure color palette
##  stdThresh     Threshold for first lag standard deviation.
##                Integration periods with standard deviation
##                larger than stdThresh times the median at any
##                height are stripped off.
##  range.km      Logical, TRUE to plot range in km, otherwise in sample intervals
##
##
##
##
## Returns:
##  ACFlist       A list with elements:
##                  acf the plotted lag profiles
##                  var variances of the acf data points
##                  range range gate numbers
##                  range.km ranges in km
##                  time.s timestamps in seconds
##                  lag lag numbers
##                  lag.us time lags in us
##                  timeString timestamps as strings
##
##
##

plotLagProfiles <- function( data , lags=c(1) , f=nothing, re=TRUE , im=FALSE , mod=FALSE , arg=FALSE , err=FALSE, xlim=NULL , ylim=NULL , relim=NULL , imlim=NULL , errlim=NULL , modlim=NULL , arglim=c(-pi,pi)  , stdThresh=NULL , cex=1.0  , col=beer , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , SIunits=TRUE , tickRes=NULL , figNum=NULL , pdf=NULL , jpg=NULL , width=8.24 , height=5.845 , paper='special' , res=NA  , cutgaps=0 )
  {

    UseMethod("plotLagProfiles")

  }

plotLagProfiles.character <- function( data , lags=c(1) , f=nothing, re=TRUE , im=FALSE , mod=FALSE , arg=FALSE , err=FALSE, xlim=NULL , ylim=NULL , relim=NULL , imlim=NULL , errlim=NULL , modlim=NULL , arglim=c(-pi,pi)  , stdThresh=NULL , cex=1.0  , col=beer , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , SIunits=TRUE , tickRes=NULL , figNum=NULL , pdf=NULL , jpg=NULL , width=8.24 , height=5.845 , paper='special' , res=NA  , cutgaps=0 )
  {

    data <- readLPIdir(dpath=data,lags=lags,stdThresh=stdThresh)
    par <- formals()
    parnames <- names(par)
    par <- lapply( names( par ) , FUN=function(x){ eval( as.name( x ) ) } )
    names(par) <- parnames
    par[["lags"]] <- seq(data[["nLag"]])

    do.call(plotLagProfiles,par)

  }


plotLagProfiles.list <- function( data , lags=c(1) , f=nothing, re=TRUE , im=FALSE , mod=FALSE , arg=FALSE , err=FALSE, xlim=NULL , ylim=NULL , relim=NULL , imlim=NULL , errlim=NULL , modlim=NULL , arglim=c(-pi,pi)  , stdThresh=NULL , cex=1.0  , col=beer , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , SIunits=TRUE , tickRes=NULL , figNum=NULL , pdf=NULL , jpg=NULL , width=8.24 , height=5.845 , paper='special' , res=NA , cutgaps=0 )
  {

    # Return immediately if no data was found
    if(is.null(data)) stop("No data")

    # check that the requested lag profiles exist in the data, cut off with warning if not
    # these are lag profile indices, not the actual times!
    if( length(lags)==0 ) stop("No lags selected")
    if( is.character(lags)) lags <- seq(data[["nLag"]])
    if( !is.numeric(lags)) stop("lags must be either a numeric vector or the string 'all'")
    if( any(lags<1) | any(lags>data[["nLag"]]) ){
        lags <- lags[lags>0]
        lags <- lags[lags<=data[["nLag"]]]
        print(lags)
        # stop if nothing was left
        if( length( lags ) == 0 ){
            if( data[["nLag"]] == 1 ){
                stop("The data list contains only lag profile ",1)
            }else{
                stop("The data list contains only lag profiles ",1," - ",data[["nLag"]])
            }
        }else{
            warning("Skipping non-existent lag profiles, using lags = ",lags)
        }
    }

    # Select the correct range vector
    if( SIunits ){
      r <- data[["range.km"]]
      if( is.null(r) ){
        r <- data[["range"]]
        SIunits <- FALSE
        warning("The data does not contain range in km, setting SIunits=FALSE")
      }
    }else{
      r <- data[["range"]]
    }

    # Select the correct lag vector
    if( SIunits ){
      l <- data[["lag.us"]]/1000
      if( is.null(l) ){
        l <- data[["lag"]]
        SIunits <- FALSE
        r <- data[["range"]]
        warning("The data does not contain lag in us, setting SIunits=FALSE")
      }
    }else{
      l <- data[["lag"]]
    }

    # If site locations and pointing directions were not given as
    # input they must be read from files
    if( is.null( llhT ) ) llhT <- data[["llhT"]]
    if( is.null( llhR ) ) llhR <- data[["llhR"]]
    if( is.null( azelT ) ) azelT <- data[["azelT"]]


    if(yheight){
      # If site locations or pointing directions are still unknown
      # we must stop
      if( is.null( llhT ) ) stop("Transmitter location not known, cannot convert range to height")
      if( is.null( llhR ) ) stop("Receiver location not known, cannot convert range to height")
      if( is.null( azelT ) ) stop("Transmitter pointing not known, cannot convert range to height")
      for(k in seq(length(r))) r[k] <- range2llh(r[k]*1000,llhT,azelT,llhR)["h"]/1000
    }

    if( SIunits ){
      if(yheight){
        ylab <- "Height [km]"
      }else{
        ylab <- "Range [km]"
      }
      mainstr <- paste(format(mean(l,na.rm=TRUE),digits=3),"ms")
    }else{
      if(yheight){
        ylab <- "Height gate"
      }else{
        ylab <- "Range gate"
      }
      mainstr <- paste("Lag",format(mean(l,na.rm=TRUE),digits=3))
    }


    # y-axis limits
    rInds <- rep(T,data[["nRange"]])
    ylim2 <- range(r,na.rm=T)
    if(!is.null(ylim)) ylim2 <- ylim
    if(!is.null(ylim)) rInds <- (r >= ylim2[1]) & (r <= ylim2[2])

    # Time axis limits
    tInds <- rep(T,data[["nACF"]])
    if(is.null(xlim)){
      tLim2 <- range(data[["time.s"]],na.rm=T)
    }else{
      tLim2 <- (floor(data[["time.s"]][1] / 3600 / 24) * 24 + xlim) * 3600
      tInds <- (data[["time.s"]] >= tLim2[1]) & (data[["time.s"]] <= tLim2[2])
    }

    # Stop if no data was left
    if(!any(rInds)) stop('No data from the given range interval')
    if(!any(tInds)) stop('No data from the given time interval')

    # Cut off large variances
    if(!is.null(stdThresh)) data[["ACF"]][data[["var"]] > (stdThresh**2)] <- NA

    # Axis limits
    if(is.null(relim))  relim  <- range(f(Re(data[["ACF"]])),na.rm=T,finite=T)
    if(is.null(imlim))  imlim  <- range(f(Im(data[["ACF"]])),na.rm=T,finite=T)
    if(is.null(errlim)) errlim <- range(f(sqrt(data[["var"]])),na.rm=T,finite=T)
    if(is.null(modlim)) modlim <- range(f(abs(data[["ACF"]])),na.rm=T,finite=T)


    # Variance-weighted averages
#    if(data[["nLag"]]==1){
    if( length(lags) == 1){
      acf <- data[["ACF"]][,lags,]
      var <- data[["var"]][,lags,]
    }else{
      acf <- apply(data[["ACF"]][,lags,]/data[["var"]][,lags,],MARGIN=c(1,3),FUN=sum,na.rm=T)
      var <- 1/apply(1/data[["var"]][,lags,],MARGIN=c(1,3),FUN=sum,na.rm=T)
      acf <- acf*var
    }

    # set  NA values to edges of data gaps
    if(cutgaps){
        td <- diff(data[["time.s"]])
        tdmed <- median(td)
        tdlarge <- which(td>cutgaps*tdmed)
        acf[,tdlarge] <- NA
        acf[,(tdlarge+1)] <- NA
        var[,tdlarge] <- NA
        var[,(tdlarge+1)] <- NA
        }


    # How many frames will we have in the plot
    nFig <- sum(re,im,err,mod,arg)

    # Height of the full plot window
    wHeight <- nFig*height

    # Open the proper figure
    figList <- c(is.null(figNum),is.null(pdf),is.null(jpg))
    if(sum(figList) < 2 ) stop('Only one output device can be selected at a time')
    # A new x11 by defaul
    if(sum(figList) == 3) x11(width=width,height=wHeight)
    # New plot to an existing x11 window
    if(!is.null(figNum)) {dev.set(figNum);plot.new()}
    # A new pdf file
    if(!is.null(pdf)) pdf(file=paste(pdf,'.pdf',sep=''),paper=paper,width=width,height=wHeight)
    # A new jpeg file
    if(!is.null(jpg)) jpeg(filename=paste(jpg,'.jpg',sep=''),width=width,height=wHeight,units='in',res=res)

    # Tick marks in the time axis
    ticks <- timeTicks(tLim2,tickRes)

    # Time string (year, month, and day, the UT time will be on x-axis labels)
    tstr  <- substr(data[["timeString"]][tInds][1],1,10)

    # Actual plotting
    curFig <- 1
    if(re) curFig <- addLPplot(d=f(Re(acf[rInds,tInds])),r=r[rInds],t=data[["time.s"]][tInds],zlim=relim,tLim2,ylim2,col=col,
                   main=list(paste(tstr,mainstr,'Real part'),cex=cex),cex=cex,ticks=ticks,
                   nFig=nFig, curFig , ylab=ylab )
    if(im) curFig <- addLPplot(d=f(Im(acf[rInds,tInds])),r=r[rInds],t=data[["time.s"]][tInds],zlim=imlim,tLim2,ylim2,col=col,
                   main=list(paste(tstr,mainstr,'Imaginary part'),cex=cex),cex=cex,ticks=ticks,
                   nFig=nFig, curFig , ylab=ylab )
    if(mod) curFig <- addLPplot(d=f(abs(acf[rInds,tInds])),r=r[rInds],t=data[["time.s"]][tInds],zlim=modlim,tLim2,ylim2,col=col,
                   main=list(paste(tstr,mainstr,'Modulus'),cex=cex),cex=cex,ticks=ticks,
                   nFig=nFig, curFig , ylab=ylab )
    if(arg) curFig <- addLPplot(d=Arg(acf[rInds,tInds]),r=r[rInds],t=data[["time.s"]][tInds],zlim=arglim,tLim2,ylim2,col=col,
                   main=list(paste(tstr,mainstr,'Argument'),cex=cex),cex=cex,ticks=ticks,
                   nFig=nFig, curFig , ylab=ylab )
    if(err) curFig <- addLPplot(d=sqrt(var[rInds,tInds]),r=r[rInds],t=data[["time.s"]][tInds],zlim=errlim,tLim2,ylim2,col=col,
                   main=list(paste(tstr,mainstr,'Standard deviation'),cex=cex),cex=cex,ticks=ticks,
                   nFig=nFig, curFig , ylab=ylab )

    if(!is.null(pdf)) dev.off()
    if(!is.null(jpg)) dev.off()

    invisible(data)

}


