	MODULE MODV_MXNRV

C	  MXNRV is the maximum number of nodes in the jump/link table
C	  that can contain new 2-03 reference values.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXNRV.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXNRV = 15
#else
	  PARAMETER ( MXNRV = 15 )
#endif

	END MODULE
