	MODULE MODV_MAXJL

C	  MAXJL is the maximum number of entries in the internal
C	  jump/link table.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MAXJL.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MAXJL = 84000

#else

#  ifdef SUPERSIZE_BUILD
	  PARAMETER ( MAXJL = 128000 )
#  else
	  PARAMETER ( MAXJL = 96000 )
#  endif

#endif

	END MODULE
