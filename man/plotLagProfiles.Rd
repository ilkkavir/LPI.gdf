\name{plotLagProfiles}
\title{plot lag profiles}
\alias{plotLagProfiles}
\description{Plot lag profiles as function of time and range.}
\usage{ plotLagProfiles( data , lags=c(1) , f=nothing, re=TRUE , im=FALSE , mod=FALSE , arg=FALSE , err=FALSE, xlim=NULL , ylim=NULL , relim=NULL , imlim=NULL , errlim=NULL , modlim=NULL , arglim=c(-pi,pi)  , stdThresh=NULL , cex=1.0  , col=beer , yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , SIunits=TRUE , tickRes=NULL , figNum=NULL , pdf=NULL , jpg=NULL , width=8.24 , height=5.845 , paper='special' , res=NA  )}

\arguments{
  \item{ data }{ Data directory path(s) or an output list from
    readLPIdir or plotLagProfilfes. The data path is a vector that may
    contain both full paths to files and directory names. }
  \item{ lags }{ Lags to integrate before plotting. Numbered starting
    from shortest lag in result files. Default 1. See details. }
  \item{ f }{ Function to apply to the data points before plotting. The
    default function "nothing" does nothing. }
  \item{ re }{ Logical, include a plot of the lag profile real parts? }
  \item{ im }{ Logical, include a plot of the lag profile imaginary parts? }
  \item{ mod }{ Logical, include a plot of the lag profile modulus? }
  \item{ arg }{ Logical, include a plot of the lag profile argument
    (phase angle) ? }
  \item{ err }{ Logical, include a plot of the lag profile standard
    deviation? }
  \item{ xlim }{ x (time) axis limits in hours. Counting starts from the
    beginning of the day when the first integration period found
    from "dpath" ended. }
  \item{ ylim }{ Range / height limits in the plot}
  \item{ relim }{ z limits for real part}
  \item{ imlim }{ z limits for imaginary part}
  \item{ errlim }{ z limits for standard deviation }
  \item{ modlim }{ z limits for modulus }
  \item{ arglim }{ z limits for argument (phase) }
  \item{ stdThresh}{ Standard deviation threshold to pass to the
    function readLPIdir.  }
  \item{ cex }{ Scaling factor for figure labels and titles. }
  \item{ col }{ Color scale to use. Default "beer" is Lovibond scale. }
  \item{ yheight }{ Logical, should the range be converted to height?
    Works only if the result files contain site positions and pointing
    directions. }
  \item{ llhT }{ c( Latitude [deg], longitude [deg], height [m] ) of the transmitter site }
  \item{ azelT }{ c(azimuth [deg] , elevation [deg]) of the transmitter beam}
  \item{ llhR }{ c( Latitude [deg], longitude [deg], height [m] ) of the receiver site }
  \item{ SIunits }{ Logical, should range be expressed in km and lag in
    ms? Works only if range.km and lag.us are stored in the data files}
  \item{ tickRes }{ Time tick resolution in seconds, NULL=automatic}
  \item{ figNum }{ Device number to use for plotting }
  \item{ pdf }{ pdf output file name }
  \item{ jpg }{ jpg output file name }
  \item{ width }{ plot width }
  \item{ height }{ plot height}
  \item{ paper }{ paper seletion }
  \item{ res }{ resolution for jpg images}

  }
  \value{
    A list similar to that returned by \link{readACF}
  }
  

  
  
  \author{Ilkka Virtanen (University of Oulu, Finland) \cr
    \email{ilkka.i.virtanen@oulu.fi}}
