	MODULE MODV_MXBTM

C	  MXBTM is the maximum number of bitmaps that can be stored
C	  internally for a BUFR subset.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXBTM.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXBTM = 5
#else
	  PARAMETER ( MXBTM = 5 )
#endif

	END MODULE
