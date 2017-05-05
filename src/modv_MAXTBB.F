	MODULE MODV_MAXTBB

C	  MAXTBB is the maximum number of entries in the internal BUFR
C	  Table B for each BUFR file that is connected to the BUFRLIB
C	  software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MAXTBB.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MAXTBB = 500
#else
	  PARAMETER ( MAXTBB = 500 )
#endif

	END MODULE
