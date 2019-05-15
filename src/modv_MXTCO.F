	MODULE MODV_MXTCO

C	  MXTCO is the maximum number of Table C operator locations
C	  within the jump/link table which can be stored internally.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXTCO.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXTCO = 250
#else
	  PARAMETER ( MXTCO = 250 )
#endif

	END MODULE
