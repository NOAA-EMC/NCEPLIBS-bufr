	MODULE MODV_MAXTBD

C	  MAXTBD is the maximum number of entries in the internal BUFR
C	  Table D for each BUFR file that is connected to the BUFRLIB
C	  software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MAXTBD.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MAXTBD = 500
#else
	  PARAMETER ( MAXTBD = 500 )
#endif

	END MODULE
