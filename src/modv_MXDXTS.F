	MODULE MODV_MXDXTS

C	  MXDXTS is the maximum number of dictionary tables that can
C	  be stored for use with BUFR messages in internal memory.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXDXTS.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXDXTS = 200
#else
	  PARAMETER ( MXDXTS = 200 )
#endif

	END MODULE
