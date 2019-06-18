	MODULE MODV_MXCDV

C	  MXCDV is the maximum number of data values that can be
C	  written into a subset of a compressed BUFR message by the
C	  BUFRLIB software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXCDV.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXCDV = 3000

#else

#  if defined SUPERSIZE_BUILD
	  PARAMETER ( MXCDV = 50000 )
#  elif defined C32BITS_BUILD
	  PARAMETER ( MXCDV = 1000 )
#  else
	  PARAMETER ( MXCDV = 3000 )
#  endif

#endif

	END MODULE
