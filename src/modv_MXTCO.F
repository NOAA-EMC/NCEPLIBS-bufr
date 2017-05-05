	MODULE MODV_MXTCO

C	  MXTCO is the maximum number of Table C operators with an
C	  X value of 21 or greater that can appear in the definition
C	  of a Table A (subset) mnemonic

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXTCO.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXTCO = 30
#else
	  PARAMETER ( MXTCO = 30 )
#endif

	END MODULE
