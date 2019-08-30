\name{getFileLengths.gdf}
\title{getFileLengths.gdf}
\alias{getFileLengths.gdf}
\description{ Read file lengths of gdf format data }
\usage{getFileLengths.gdf( LPIparam ) }

\arguments{
  \item{ LPIparam }{ An LPI parameter list from \link{LPI.gdf} }
}

\value{
  
  A named vector with elements "RX1","RX2","TX1","TX2", each of which is
  the number of samples in data files of the corresponding data type. 
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
  }


}



  \author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}
