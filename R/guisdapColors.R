## file:guisdap.colors.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##
## A colormap similar to one used in some version of guisdap
##

guisdap.colors <- function(n)
  {
    f <- matrix(c(0,0,0, 0,0,1, 0,0,2, 0,1,1, 0,2,0, 1,2,0, 2,2,0, 2,1,0, 2,0,0, 2,0,1, 2,0,2, 2,1,2)/2,nrow=3,byrow=F)
    nr <- 12
    b <- round(seq(0,(nr-1))/(nr-1)*(n-1)) + 1
    f2 <- matrix(nrow=3,ncol=n)
    for(k in seq(3)){
      f2[k,] <- tanh(approx(x=b,y=f[k,],xout=seq(n))$y)/tanh(1)
    }
    
    cols <- c()
    for(k in seq(n)){
      cols[k] <- rgb(f2[1,k],f2[2,k],f2[3,k])
    }
    
    return(cols)
 
  }
