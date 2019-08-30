\name{readACF}
\title{Average ACF}
\alias{readACF}
\description{ Read LPI result files and return the average ACF}
\usage{ readACF( dpath , lags=NULL , ranges=NULL , stdThrsh=Inf )}

\arguments{
  \item{ dpath }{ Data file / directory path(s) as a vector of character
    strings.}
  \item{ lags }{ Lag selection. Either a vector of lag numbers, or
    "all" to read all lags}
  \item{ ranges }{ Range gate selection}
  \item{ stdThrsh}{ Standard deviation threshold. Individual data
  points with standard deviation larger than stdThresh in absolute units
  are not included in the average}
  }
  \value{
    A list with the following elements plus everything listed in the
    "ACF" list stored in the last data file, except the full covariance matrix.

    \describe{
      
      \item{ 'ACF' }{ An average lag profile matrix }
      \item{ 'var' }{ ACF variance estimates }
      \item{ 'backgroundACF' }{ Average background ACF }
      \item{ 'backgroundvar' }{ Variance estimate for backgroundACF }
      \item{ 'lag' }{ Lag numbers }
      \item{ 'lag.us' }{ Time lags in us }
      \item{ 'range'}{ Range gates }
      \item{ 'range.km'}{ Range in km}
      \item{ 'nGates' }{Number of range gates}
      
    }
  }
  
  
  
  \author{Ilkka Virtanen (University of Oulu, Finland) \cr
    \email{ilkka.i.virtanen@oulu.fi}}
