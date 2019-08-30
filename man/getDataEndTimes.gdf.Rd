\name{getDataEndTimes.gdf}
\title{getDataEndTimes.gdf}
\alias{getDataEndTimes.gdf}
\description{Read sampling times of latest data samples }
\usage{getDataEndTimes.gdf( LPIparam ) }

\arguments{
  \item{ LPIparam }{ An LPI parameter list from \link{LPI.gdf}}
}

\value{
  
  A named vector with elements "RX1","RX2","TX1","TX2", each of which is
  the sampling time of the latest recorded data sample of the
  corresponding type in POSIX format. 
}

\details{

  Following elements of the LPI parameter list are used in this
  function.

  \describe{

    \item{'dataDir'}{A named character vector that contains the data
      directories for each data type. }
    \item{'fileNamePrefix'}{ A named character vector that contains the
      file name prefixes for each data type. }
    \item{'fileNameExtension'}{ A named character vector that contains the
      file name extensions for each data type. }
    \item{'dataFileLengths'}{ A named numeric vector that contains
      number of samples in a single data file of each data type. }
    \item{'dataSampleFreqs'}{ A named numeric vector that contains
      sample rates of each data type in Hz.}
    \item{'dataStartTimes'}{ A named numeric vector that contains
      sampling times of the first samples of each data type in POSIX
      format. }
  }


}



  \author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}
