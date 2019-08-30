## file:plotSpectrumR
## (c) 2015- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Color-coded image of power spectral density
## estimated by means of the Lomb-Scargle periodogram
## 
## Arguments:
##  data an ACF list from LPI.gdf output file, from the
##      function readACF, or from plotACF
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
##  rscale    Logical, scale with range squared?
##
##
##  Returns:
##   spectrum The input list padded with the power spectral density
##



plotSpectrum <- function( data, normalize=TRUE , pdf=NULL , jpg=NULL , figNum=NULL , zlim=NULL , ylim=NULL , xlim=NULL , cex=1 , bg='white' , fg='black' , width=8.27 , height=11.69 , paper='a4' , res=300 , stdThrsh=Inf , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , rscale=FALSE)
  {
    UseMethod("plotSpectrum")
  }



plotSpectrum.character <- function( data, normalize=TRUE , pdf=NULL , jpg=NULL , figNum=NULL , zlim=NULL , ylim=NULL , xlim=NULL , cex=1 , bg='white' , fg='black' , width=8.27 , height=11.69 , paper='a4' , res=300 , stdThrsh=Inf , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , rscale=FALSE)
  {

    data <- readACF( dpath=data , stdThrsh=stdThrsh )

    par <- formals()
    parnames <- names(par)
    par <- lapply( names( par ) , FUN=function(x){ eval( as.name( x ) ) } )
    names(par) <- parnames

    do.call(plotSpectrum,par)

  }



plotSpectrum.list <- function( data , normalize=TRUE , pdf=NULL , jpg=NULL , figNum=NULL , zlim=NULL , ylim=NULL , xlim=NULL , cex=1 , bg='white' , fg='black' , width=8.27 , height=11.69 , paper='a4' , res=300 , stdThrsh=Inf , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , rscale=FALSE)
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
    if( is.null(data[["range.km"]]) ){
        stop('The data does not contain range in km')
    }else if( is.null(data[["lag.us"]]) ){
        stop('The data does not contain lag in us')
    }

    # Select the correct range and lag vectors
    r <- data[["range.km"]]
    l <- data[["lag.us"]]/1000

    # create the frequency axis
    fmax <- 1/min(diff(l),na.rm=T)
    ftmp <- seq(0,fmax,length.out=length(l))
    f <- c(-rev(ftmp[2:length(l)]),ftmp)


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
      grid <- expand.grid(x=f,y=h)
      data[["height.km"]] <- h
      data2[["height.km"]] <- h
    }else{
      grid <- expand.grid(x=f,y=r)
    }

    if( rscale ){
        for( k in seq( length( data[["range"]] ) ) ){
            data[["ACF"]][k,] <- data[["ACF"]][k,]*data[["range"]][k]**2
        }
    }



    # ok, this will still require some thinking
    # looks like the simple summation over data[["ACF"]][height,lag]*exp(1i*om*lag) is the correct way to do this?
    ssc <- matrix(ncol=length(f),nrow=length(data[["range"]]))
    for(hh in seq(length(data[["range"]]))){
        if(any(!is.na(data[["ACF"]][hh,]))){
            ssc[hh,] <- 0
            for(ll in seq(length(data[["lag.us"]]))){
                if(!is.na(data[["ACF"]][hh,ll])){
                    ssc[hh,] <- ssc[hh,] + data[["ACF"]][hh,ll]*exp(1i*2*pi*f*data[["lag.us"]][ll]*1e-6) + Conj(data[["ACF"]][hh,ll]*exp(-1i*2*pi*f*data[["lag.us"]][ll]*1e-6))
                }
            }
        }
    }

    ss <- abs(ssc)**2

    if(normalize){
        for(hh in seq(length(data[["range"]]))){
            ss[hh,] <- ss[hh,]/max(ss[hh,],na.rm=TRUE)
        }
    }


    grid$z <- c(t(ss))

    main <- "Power spectrum"

    if(yheight){
        ylab <- "Height [km]"
    }else{
        ylab <- "Range [km]"
    }
    xlab <- "Frequency [Hz]"
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
    if (is.null(xlim)) xlim=range(f,na.rm=TRUE)
    if (is.null(zlim)) zlim=range(grid$z,na.rm=T,finite=T)


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
        )
    )
    data2[["spectrum"]] <- ss
    data2[["f"]] <- f
    if(!is.null(pdf)) dev.off()
    if(!is.null(jpg)) dev.off()

    invisible(data2)

  }
