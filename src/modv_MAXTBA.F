	MODULE MODV_MAXTBA

C	  MAXTBA is the maximum number of entries in the internal BUFR
C	  Table A for each BUFR file that is connected to the BUFRLIB
C	  software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MAXTBA.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MAXTBA = 150
#else
	  PARAMETER ( MAXTBA = 150 )
#endif

	END MODULE
