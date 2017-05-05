	MODULE MODV_MXTAMC

C	  MXTAMC is the maximum number of Table A (subset) mnemonics
C	  in the jump/link table which contain at least one Table C
C	  operator with an X value of 21 or greater in their definition

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXTAMC.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXTAMC = 15
#else
	  PARAMETER ( MXTAMC = 15 )
#endif

	END MODULE
