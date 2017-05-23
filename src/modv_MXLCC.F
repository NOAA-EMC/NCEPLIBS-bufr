	MODULE MODV_MXLCC

C	  MXLCC is the maximum length of a character string that can be
C	  written into a subset of a compressed BUFR message by the
C	  BUFRLIB software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXLCC.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXLCC = 32
#else
	  PARAMETER ( MXLCC = 32 )
#endif

	END MODULE
