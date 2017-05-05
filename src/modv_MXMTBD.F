	MODULE MODV_MXMTBD

C	  MXMTBD is the maximum number of entries in the master
C	  BUFR Table D.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXMTBD.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXMTBD = 800
#else
	  PARAMETER ( MXMTBD = 1000 )
#endif

	END MODULE
