\name{readData.gdf}
\title{readData.gdf}
\alias{readData.gdf}
\description{Read gdf data files}
\usage{readData.gdf( files , n=1e6 , istart=1 , bigEndian=TRUE )}

\arguments{
  \item{ files }{ A vector of data file names. The files are read in the
    same order in which they are in the vector. }
  \item{ n }{ Number of samples to read from each file, the vector is
    repeated as necessary to match its length with the "files" vector }
  \item{ istart }{ Indices from which to start the reading in each file,
    repeated as necesary to mathc its length with the "files" vector.}
  \item{ bigEndian }{ Logical telling whether the files are in big
  endian (default) or little endian format. }

  }

  \value{
    A list with the following entries:

    \describe{


      \item{ 'cdata' }{ A vector of complex data values. }

      \item{ 'idatar' }{ Lowest bit in real part of the data. Should
	contain the PPS bits in radar measurements. }
      
      \item{ 'idatai' }{ Lowest bit in imaginary part of the data. Should
	contain the TX bits in radar measurements. }
      
      \item{ 'ndata' }{ Total number of data points read. This is always the
	sum of the number of samples *intended* to be read from each
	file. If n is larger than file length for any of the input files,
	vectors cdata, idatar, and idatai will contain arbitrary values
	correspondingly.}

      \item{ 'success' }{ A logical indicating whether ndata points were
	successfully read ( success = TRUE ), or not ( success = FALSE
	). e.g. any (n + istart - 1) larger than file length will lead to
	success=FALSE.}
  
    }
  }

  \examples{


\dontrun{

  # read all data from file "file1.gdf"
  d <- readData.gdf( files = "file1.gdf" , n = file.info( "file1.gdf" )$size / 4 )

  # read samples 1001 ... 10000 from "file1.gdf", samples 1 ... 10000 from
  # "file2.gdf", and samples 1 ... 200 from "file3.gdf".
  d <- readData.gdf( files = c( "file1.gdf" , "file2.gdf" , "file3.gdf" ) ,
                         n = c( 9000 , 10000, 200 ) ,
                    istart = c( 1001 , 1 , 1) )
}
}



  \author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}
