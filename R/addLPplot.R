## file:addLPplot.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Create new plots for plotLagProfiles
##
## Arguments:
##  d      input data matrix
##  r      y-axis
##  t      x-axis
##  zlim   z limits
##  xlim   x limits
##  ylim   y limits
##  main   main title string
##  cex    text maginication
##  ticks  time axis ticks
##  nFig   total number of figure panes in the plot window
##  curFig current figure pane number
##  col    color palette
##  ylab   y axis label string
##
## Returns:
##  nextFig number of the next figure pane
##

addLPplot <- function(d,r,t,zlim,xlim,ylim,main,cex,ticks,nFig,curFig,col=beer,ylab='range.km')
  {

    grid   <- expand.grid(x=t,y=r)
    grid$z <- as.vector(t(d))
    
    print(
      levelplot(
        z~x*y,
        grid,
        col.regions=col,
        at=seq(zlim[1],zlim[2],length.out=100),
        scales=list(x=list(at=ticks[['tick']],labels=ticks[['string']]),cex=cex),
        xlim = xlim,
        ylim = ylim,
        xlab=list("UT",cex=cex),
        ylab=list(ylab,cex=cex),
        main = main,
        colorkey=list(labels=list(cex=cex))
        ),
      split=c(1,curFig,1,nFig),
      more=TRUE
      )
    
    return((curFig+1))
    
  }

