	MODULE MODV_MXMTBB

C	  MXMTBB is the maximum number of entries in the master
C	  BUFR Table B.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXMTBB.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXMTBB = 2500
#else
	  PARAMETER ( MXMTBB = 4000 )
#endif

	END MODULE
