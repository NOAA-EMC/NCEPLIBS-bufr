	MODULE MODV_MXBTMSE

C	  MXBTMSE is the maximum number of entries that can be set
C	  within a bitmap.  An entry is "set" if the bit has a value
C	  of 0.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXBTMSE.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXBTMSE = 500 
#else
	  PARAMETER ( MXBTMSE = 500 )
#endif

	END MODULE
