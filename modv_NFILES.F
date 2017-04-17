	MODULE MODV_NFILES

C	  NFILES is the maximum number of BUFR files that can be
C	  connected to the BUFRLIB software (for reading or writing)
C	  at any one time.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for NFILES.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: NFILES = 32

#else

#  ifdef C32BITS_BUILD
	  PARAMETER ( NFILES = 10 )
#  else
	  PARAMETER ( NFILES = 32 )
#  endif

#endif

	END MODULE
