	MODULE MODV_MXCSB

C	  MXCSB is the maximum number of subsets that can be written
C	  into a compressed BUFR message by the BUFRLIB software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXCSB.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXCSB = 4000

#else

#  ifdef C32BITS_BUILD
	  PARAMETER ( MXCSB = 2000 )
#  else
	  PARAMETER ( MXCSB = 4000 )
#  endif

#endif

	END MODULE
