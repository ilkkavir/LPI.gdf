## file:plotACF.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Color-coded image of an ACF
##
## Arguments:
##  data      a data directory path, data file name vector,
##            a combination of these two, or a list returned
##            by readACF or plotACF.
##  part      're', 'im', or 'er'
##  pdf       pdf file name
##  jpg       jpeg file name
##  figNum    figure number
##  zlim      z limits
##  ylim      y axis limits
##  xlim      x axis limits
##  cex       Text magnification
##  bg        Background color
##  fg        Foreground color
##  width     Plot width
##  height    Plot height
##  paper     paper dimensions when printing to files
##  res       Resolution for jpg images
##  stdThreh  Threshold for ACF variance(in some units?)
##  yheight   Logical, if TRUE y-axis is height, otherwise range
##  llhT      latitude, longitude, height of transmitter
##  azelT     azimuth and elevation of transmitter beam
##  llhR      latitude, longitude, height of receiver
##  lags      Indices of lags to include in the plot
##  SIunits   Logical, if TRUE range / height is expressed in km
##            and lag in ms, otherwise gates (sample intervals)
##            are used.
##  xlog      Logical, should the time-lag axis be logarithmic?
##  rscale    Logical, scale with range squared?
##
##  Returns:
##   data     A list similar to that returned by readACF
##            This list is suitable input for a new plotACF
##            call. Notice that the lags argument is not
##            effective when the input argument is a list.
##
##

plotACF <- function( data , part='real' , pdf=NULL , jpg=NULL , figNum=NULL , zlim=NULL , ylim=NULL , xlim=NULL , cex=1 , bg='white' , fg='black' , width=8.27 , height=11.69 , paper='a4' , res=300 , stdThrsh=Inf , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , lags=NULL , SIunits=TRUE , xlog=FALSE , rscale=FALSE )
  {
    UseMethod("plotACF")
  }

plotACF.character <- function( data , part='real' , pdf=NULL , jpg=NULL , figNum=NULL , zlim=NULL , ylim=NULL , xlim=NULL , cex=1 , bg='white' , fg='black' , width=8.27 , height=11.69 , paper='a4' , res=300 , stdThrsh=Inf , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , lags=NULL , SIunits=TRUE , xlog=FALSE , rscale=FALSE )
  {

    data <- readACF( dpath=data , lags=lags , stdThrsh=stdThrsh )

    par <- formals()
    parnames <- names(par)
    par <- lapply( names( par ) , FUN=function(x){ eval( as.name( x ) ) } )
    names(par) <- parnames

    do.call(plotACF,par)

  }

plotACF.list <- function( data , part='real' , pdf=NULL , jpg=NULL , figNum=NULL , zlim=NULL , ylim=NULL , xlim=NULL , cex=1 , bg='white' , fg='black' , width=8.27 , height=11.69 , paper='a4' , res=300 , stdThrsh=Inf , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , lags=NULL , SIunits=TRUE , xlog=FALSE , rscale=FALSE , ... )
  {

    # copy the data list
    data2 <- data

    # open the proper figure
    figList <- c(is.null(figNum),is.null(pdf),is.null(jpg))
    if(sum(figList) < 2 ) stop('Only one output device can be selected at a time')
    # a new x11 by defaul
    if(sum(figList) == 3) x11(width=width,height=height)
    # new plot to an existing x11 window
    if(!is.null(figNum)) {dev.set(figNum);plot.new()}
    # a new pdf file
    if(!is.null(pdf)) pdf(file=paste(pdf,'.pdf',sep=''),paper=paper,width=width,height=height)
    # a new jpeg file
    if(!is.null(jpg)) jpeg(filename=paste(jpg,'.jpg',sep=''),width=width,height=height,units='in',res=res)

    # Check if requested range and lag vectors exist
    # in the data
    if( SIunits ){
      if( is.null(data[["range.km"]]) ){
        warning('The data does not contain range in km, settings SIunits=FALSE')
        SIunits <- FALSE
      }else if( is.null(data[["lag.us"]]) ){
        warning('The data does not contain lag in us, settings SIunits=FALSE')
        SIunits <- FALSE
      }
    }

    # Select the correct range and lag vectors
    if(SIunits){
      r <- data[["range.km"]]
      l <- data[["lag.us"]]/1000
    }else{
      r <- data[["range"]]
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
      h <- r
      for(k in seq(length(r))) h[k] <- range2llh(r[k]*1000,llhT,azelT,llhR)["h"]/1000
      grid <- expand.grid(x=l,y=h)
      if(SIunits){
          data[["height.km"]] <- h
          data2[["height.km"]] <- h
      }else{
          data[["height"]] <- h
          data2[["height"]] <- h
      }
    }else{
      grid <- expand.grid(x=l,y=r)
    }

    if( rscale ){
        for( k in seq( length( data[["range"]] ) ) ){
            data[["ACF"]][k,] <- data[["ACF"]][k,]*data[["range"]][k]**2
            data[["var"]][k,] <- data[["var"]][k,]*data[["range"]][k]**4
        }
    }


    grid$z <- switch(substr(tolower(part),1,2),
                     re   = c(t(Re(data$ACF))),
                     im   = c(t(Im(data$ACF))),
                     er   = c(t(sqrt(data$var)))
                     )

    main <- switch(substr(tolower(part),1,2),
                   re   = list(paste(data$timeString,' real part'),cex=cex,col=fg),
                   im   = list(paste(data$timeString,' imaginary part'),cex=cex,col=fg),
                   er   = list(paste(data$timeString,' standard deviation'),cex=cex,col=fg),
                   )

    if(SIunits){
      if(yheight){
        ylab <- "Height [km]"
      }else{
        ylab <- "Range [km]"
      }
      xlab <- "Lag [ms]"
    }else{
      if(yheight){
        ylab <- "Height gate"
      }else{
        ylab <- "Range gate"
      }
      xlab <- "Lag"
    }
    trebg <- trellis.par.get(name='background')
    trebg$col <- bg
    trellis.par.set(col=fg,background=trebg)
    if (is.null(ylim)){
      if(yheight){
        ylim=range(h,na.rm=TRUE)
      }else{
        ylim=range(r,na.rm=TRUE)
      }
    }
    if (is.null(xlim)) xlim=range(l,na.rm=TRUE)
    if (is.null(zlim)) zlim=range(grid$z,na.rm=T,finite=T)

    if( xlog ){
        xlim <- log10(xlim)
        grid$x <- log10(grid$x)
        xlab <- paste('Log10(',xlab,')')
    }

    if (length(l)>1){
      par(cex.axis=cex,cex.lab=cex,cex.main=cex,bg=bg,fg=fg,lwd=cex,col.lab=fg,
          col.axis=fg,col.main=fg,bty='n',mar=c(5,8,0,0)*cex,mgp=c(6,2,0)*cex)
      print(
        levelplot(
          z~x*y,
          grid,
          col.regions=beer,
          ylim=ylim,
          xlim=xlim,
          at=seq(zlim[1],zlim[2],length.out=100),
          cuts=100,
          xlab=list(xlab,cex=cex,col=fg),
          ylab=list(ylab,cex=cex,col=fg),
          colorkey=list(labels=list(col=fg,cex=cex)),
          scales=list(col=fg,cex=cex),
          main=main,
          ...
          )
        )
    }else{
      par(cex.axis=cex,cex.lab=cex,cex.main=cex,bg=bg,fg=fg,lwd=cex,col.lab=fg,
          col.axis=fg,col.main=fg,bty='n',mar=c(5,8,0,0)*cex,mgp=c(6,2,0)*cex)
      plot(
        grid$z,
        grid$y,
        ylab=ylab,
        xlim=zlim,
        ylim=ylim,
        type='l',
        xlab='',
        main=main
        )
    }

    if(!is.null(pdf)) dev.off()
    if(!is.null(jpg)) dev.off()

    invisible(data2)

  }
