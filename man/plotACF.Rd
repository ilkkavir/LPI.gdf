\name{plotACF}
\title{plot ACF}
\alias{plotACF}
\description{Plot incoherent scatter autocovariance function.}
\usage{ plotACF( data , part='real' , pdf=NULL , jpg=NULL , figNum=NULL
, zlim=NULL , ylim=NULL , xlim=NULL , cex=1 , bg='white' , fg='black' ,
width=8.27 , height=11.69 , paper='a4' , res=300 , stdThrsh=Inf ,
yheight=FALSE , llhT=NULL , azelT=NULL , llhR=NULL , lags=NULL ,
SIunits=TRUE )
}
\arguments{
  \item{ data }{ Data directory path(s) or an output list from
    readLPIdir or plotLagProfilfes. The data path is a vector that may
    contain both full paths to files and directory names. }
  \item{ part }{ Real part / imaginary part / standard deviation plot
    selection. Use string "real", "imaginary", or "error". (Only the
    first two characters are used, case non-sensitive. )}
  \item{ pdf }{ pdf output file name }
  \item{ jpg }{ jpg output file name }
  \item{ figNum }{ Device number to use for plotting }
  \item{ zlim }{ z axis limits }
  \item{ ylim }{ y axis limits }
  \item{ xlim }{ x axis (lag) limits }
  \item{ cex }{ Scaling factor for figure labels and titles. }
  \item{ bg }{ Background color }
  \item{ fg }{ Foreground color}
  \item{ width }{ plot width }
  \item{ height }{ plot height}
  \item{ paper }{ paper seletion }
  \item{ res }{ resolution for jpg images}
  \item{ stdThrsh }{ Standard deviation threshold for readACF}
  \item{ yheight }{ Logical, should the range be converted to height?
    Works only if the result files contain site positions and pointing
    directions. }
  \item{ llhT }{ c( Latitude [deg], longitude [deg], height [m] ) of the transmitter site }
  \item{ azelT }{ c(azimuth [deg] , elevation [deg]) of the transmitter beam}
  \item{ llhR }{ c( Latitude [deg], longitude [deg], height [m] ) of the receiver site }
  \item{ lags }{ Time lag selection. Effective only if data is a file path.}
  \item{ SIunits }{ Logical, should range be expressed in km and lag in
    ms? Works only if range.km and lag.us are stored in the data files}
  }
  \value{
    A list similar with that returned by \link{readACF}

  }
  
  \author{Ilkka Virtanen (University of Oulu, Finland) \cr
    \email{ilkka.i.virtanen@oulu.fi}}
