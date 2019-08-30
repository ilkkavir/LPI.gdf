
\name{LPI.gdf}

\title{LPI.gdf}

\alias{LPI.gdf}

\description{Lag Profile Inversion with gdf files}

\usage{LPI.gdf( ... )}

\arguments{
  \item{ ... }{ Arguments to be passed for functions that parse the
    final LPI input argument list. Accepts a reference to a stored input
    argument file and / or direct command line arguments. All
    parameters have default values which are used if the parameter is
    not found elsewhere.
    
    Following arguments are effective:
    
    \describe{

      \item{'paramFile'}{A file containing any subset of the accepted
	input arguments. In conflict situations values from command line
	will override those read from file. See details.

	Default: NULL
      }

      \item{'clusterNodes'}{A list defining the computer cluster, see
	help of the main LPI package.

	Default:
      list(tesla1=8,tesla2=8,tesla3=8,tesla4=8,tesla5=8,localControl=TRUE) 
      }
      
      \item{'begintTime'}{Analysis start time,
	c( year , month , day , hour , minute , seconds), see details.

	Default: c(1970,1,1,0,0,0)
      }
      
      \item{'endTime'}{Analysis end time, see details.
	c( year , month , day , hour , minute , seconds), see details.

	Default: c(3000,1,1,0,0,0)
      }
      
      \item{'timeRes.s'}{Analysis time-resolution (integration time) in
	seconds.

	Default: 10
      }
      
      \item{'maxWait.s'}{Maximum time to wait for new data before stopping
	the analysis, in seconds.

	Default: -1
      }

      \item{# Basic definitions of the analysis}{}
      
      \item{'freqOffset.Hz'}{Frequency offset from baseband in Hz, see
	details.

	Default: 0.0
      }
      
      \item{'filterLength.us'}{Length of the boxcar-shaped post-detection
	filter in us.

	Default: 10
      }
      
      \item{'lagLimits.us'}{Limits of time-lag gates in us, see details.

	Default: seq(50)*10
      }
      
      \item{'rangeLimits.km'}{Limits of range gates in km, see details.

	Default:
      c(seq(50,149.9,by=.9),seq(154.4,298.4,by=4.5),seq(343.4,793.5,by=45)) 
      }
      
      \item{'maxRanges.km'}{ Maximum range for each lag profile. Allows
	the number of range gates to be limited at selected lags.
	See details.

	Default: Inf	
      }

      \item{'nCode'}{Code cycle length for the optional lag profile
	pre-averaging, see help of the main LPI package. 
	
	Default: NA
      }
      
      \item{'backgroundEstimate'}{Optional background ACF
	suppression. See help of the main LPI package.

	Default: TRUE
      }
      
      \item{'fullCovar'}{Full covariance matrix / variance calculation
	selection. See help of the main LPI package. 

	Default: FALSE
      }
      
      \item{'decodingFilter'}{Amplitude domain decoding filter
	selection. See help of the main LPI package.
	
	Default: "none"
      }
      
      \item{'maxClutterRange.km'}{ Maximum range at which ground clutter
	will be subtracted. Set below min(rangeLimits.km) in order to
	disable the clutter suppression. See details.

	Default: 60
      }

      \item{'clutterFraction'}{ The fraction of a full integration
	period used for the coherent ground clutter profile
	estimation. A float from between (0,1]. Values smaller
	than 1 should be used carefully. See help of the main LPI
	package. 

	Default: 1.0
      }
      
      \item{'remoteRX'}{ Monostatic / bistatic analysis selection. See
	help of the main LPI package.

	Default: FALSE
      }

      \item{'ambInterp'}{Range ambiguity function oversampling on / off
	switch. See help of the main LPI package. 

	Default: FALSE
      }
	      
      \item{'indexShifts.us'}{Additional adjustements to TX and RX indices
	in us, see details.

	Default: list( TX=c(0,0) , RX=c(0,0) )
      }
      
      \item{'normTX'}{Optional TX amplitude normalization. See help of
	the main LPI package. 

	Default: FALSE
      }

      \item{'llhT'}{ Latitude [deg], longitude [deg], and height [m] of
	the transmitter.

	Default: c( 69.58  , 19.23 , 86.00 ) # EISCAT Tromso
      }
      
      \item{'llhR'}{ Latitude [deg], longitude [deg], and height [m] of
	the receiver.
      
	Default: c( 69.58  , 19.23 , 86.00 ) # EISCAT Tromso
      }
      
      \item{'azelT'}{ Azimuth and elevation of the transmitter beam
	[deg]. Azimuth 0 ... 360, north=0, east=90.

	Default: c(0,90)
      }
      
      \item{'azelR'}{ Azimuth and elevation of the receiver beam.

	Default: c(0,90)
      }
      
      \item{'radarFreq'}{ Transmitter carrier frequency [Hz].

	Default: 224e6
      }

      \item{'solver'}{Inverse problem solver selection. See help of the
	main LPI package.
	
	Default: "fishs"
      }
      
      \item{'nBuf'}{Number of theory matrix rows to buffer before
	calling the solver function.

	Default: 10000
      }
      
      \item{'rlips.options'}{Additional options to the rlips solver. See
	rlips help for details.

	Default: list( type="c" , nbuf=1000 , workgroup.size=128)
      }

      \item{'dataDir'}{Raw voltage data  directory / directories, see
	details.

	Default: "."
      }
      
      \item{'resultDir'}{Output directory.

	Default: paste(format(Sys.time(),"%Y-%m-%d_%H:%M"),'LP',sep='_')
      }

    }
  }
}



\details{

  The final argument list is constructed from default values, values
  read from the optional parameter file, and command line arguments. 
  Priorities of these, from lowest to highest, are defaults, values from
  a file, and command line arguments. Name collisions are solved by
  selecting the option with highest priority. In other words, command
  line arguments will override both defaults and file input, and file
  input will override the defaults. Defaults are used only if nothing
  else is available. 
  
  \describe{
    
    \item{'paramFile'}{ Usually a file that the analysis has
      automatically stored in a previous LPI run. The file is stored in
      the output directory ('resultDir') under the name
      "LPIparam.Rdata". Giving such a file as an input argument is the
      simplest way to restart an analysis with identical parameters. It
      is also possible to e.g. store parameters specific to an experiment
      in a file in order to avoid long input argument lists.
    }
    
    \item{'lagLimits.us'}{ A vector of time lag limits. The vector is
      first divided with filterLength.us, then rounded to nearest integer, and
      finally possible dublicate values are removed. Output of this process
      is a vector lagLimits = c( l1, l2, l3, lN ). The analysis then
      integrates lags l1, l1+1, ... l2-1 (of decimated data) into lag
      profile 1, lags l2, l2+1, ... l3-1 into lag profile 2, etc. As an
      example, in order to decimate data to 10 us sample rate and to
      calculate all lags up to 100 us separately, one would use
      filterLength.us=10 and
      lagLimits.us = c(10,20,30,40,50,60,70,80,90,100,110). The steps in
      lagLimits.us do not need to be of equal length. It is possible to
      use coarser lag resolution at longer lags by selecting e.g.
      lagLimits.us = c(10,20,30,50,70,90,110).
    }

    \item{'rangeLimits.km'}{ A vector of range-gate limits. The vector
      is first converted into time-delays assuming that signal propagates at
      speed of light, then decimated to filterLength.us sample rate, and
      possible duplicate values are removed. Range-gates are then defined in
      exactly the same way as lag-gates (see above).}

    \item{'maxRanges.km'}{ Allows one to reduce the number of range-gates
      at selected lags. If the vector is shorter than lagLimits.us its last
      value is repeated as necessary. Defaults to Inf, which means that all
      lags are solved at all range gates defined in rangeLimits.km.

      maxRanges.km may be used in combined D/E/F-region experiments
      when correlations longer than certain limit are known to exist
      only in D-region. As an example, in order to solve
      first 30 lags at all range gates, but the longer ones only below
      100 km, one can use maxRanges.km = c( rep( Inf , 30 ) , 100 ).

      Another use case is a dedicated D region experiment that makes use
      of voltage level decoding. Most time lags will then have zero
      range ambiguity functions and can be safely skipped. Assuming that
      modulation bit length is 2 us and inter-pulse period is 1 ms, one
      could use e.g. the following combination:
      filterLength.us=2, decodingFilter='matched',
      lagLimits.us=c(1000,1002,2000,2002,3000,3002),
      maxRanges=c(Inf,0,Inf,0,Inf)
      which would solve three pulse-to-pulse lags and save the analysis
      from inspecting all time lags (only 1 in 500 of them can actually
      be measured).

    }
  
    \item{'beginTime'}{ A vector
      c( year , month , day , hour , minute , seconds )
      of the desired analysis start time. If data is available from the given
      time, this is the start time of the first integration period. If data
      is not available from this time, the first integration period is still
      aligned with "beginTime", and the analys is started from the first
      available data sample that was recorded after "beginTime". Notice
      that, in order to make the software more suitable for real-time
      analysis, the *latest* available data is always used first, and the
      first integration period may thus be the last to actually analyse.}

    \item{'endTime'}{ A vector
      c( year , month , day , hour , minute , seconds )
      of the analysis end time. Data samples recorded after "endTime" are
      not used.}
    
    \item{'freqOffset.Hz'}{ Frequency offset from baseband to the signal
      centre frequency. Currently only one frequency per data vector,
      i.e. either a single offset for all data, or different shifts for
      "RX1", "RX2", "TX1", and "TX2". See the details for "dataDir" for
      instructions.}


    \item{'indexShifts.us'}{ Additional adjustments to the TX (and) RX indices
      returned by 'dataInputFunction'. The user input is converted into
      a list with entries "RX1", "RX2", "TX1", and "TX2" in the way
      explained in details for "dataDir". Each entry is a two-element
      vector c( shift1, shift2 ) where, in case of TX, shift1 is the
      adjustment at rising edge of each pulse, and shift2 is the
      adjustment at falling edge of each pulse. For RX, shift1 is the
      adjustment at each start of reception, and shift2 at end of
      reception.  All shifts are positive forwards, use negative values
      to adjust towards earlier times. As an example, if the recorded TX
      bit for "TX1" starts 10 us *before* the actual pulse, and ends 20
      us *after* the actual end of  the pulse, one should have
      indexShifts.us["TX1"] == c(10,-20).
    }


    \item{'dataDir'}{ Either a character string of a single data
      directory, or a named vector of several data directories. The
      internally used "dataDir" is always a vector with entries "RX1",
      "RX2", "TX1", and "TX2", some or all of which may be equal. The
      analysis calculates lagged products in between data samples from
      "RX1" and "RX2", and range ambiguity functions as lagged products
      in between data samples from "TX1" and "TX2". The internally used
      list is formed from user input as follows:
  
      1. If the input vector contains any of the entries "RX1", "RX2", "TX1",
      "TX2", they are copied as such.

      2. If any of the four entries is missing after step 1, the input is
      searched for "RX", which is used to replace missing "RX1" and /
      or "RX2". A similar search is performed for "TX".

      3. If any of the four entries is missig after steps 1 and 2, the
      input is searched for "TR1", which is used to replace "RX1" and /
      or "TX1". A similar search is performed for "TR2"

      4. If any of the four entries is missing after steps 1 to 3, the
      *unnamed* elements of the input vector are repeated until a
      vector of length 4 is formed, and its elements are named "RX1",
      "RX2", "TX1", and "TX2".

      5. If still unsuccessfull, i.e. the input vector was either empty or
      it contained only named entries with unknown names, the analysis
      will stop with an error message.

    }
    
  }
}       


\author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}

