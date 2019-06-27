	MODULE MODV_MXRST

C	  MXRST is the maximum number of "long" character strings (i.e.
C	  greater than 8 bytes) that can be read from a subset of a
C	  compressed BUFR message.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXRST.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXRST = 50
#else
	  PARAMETER ( MXRST = 50 )
#endif

	END MODULE
