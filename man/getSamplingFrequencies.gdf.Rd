\name{getSamplingFrequencies.gdf}
\title{getSamplingFrequencies.gdf}
\alias{getSamplingFrequencies.gdf}
\description{ Read sample rates of gdf format data }
\usage{getSamplingFrequencies.gdf( LPIparam ) }

\arguments{
  \item{ LPIparam }{ An LPI parameter list from \link{LPI.gdf} }
}

\value{
  
  A named vector with elements "RX1","RX2","TX1","TX2", each of which is
  the sample rate of the corresponding data type in Hz.
}

\details{

  Following elements of the LPI parameter list are used in this
  function.

  \describe{

    \item{'dataDir'}{A named character vector that contains the data
      directories for each data type. }
  }


}



  \author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}
