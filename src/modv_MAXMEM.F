	MODULE MODV_MAXMEM

C	  MAXMEM is the maximum number of bytes that can be used to
C	  store BUFR messages within internal memory.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MAXMEM.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MAXMEM = 50000000

#else

#  if defined SUPERSIZE_BUILD
	  PARAMETER ( MAXMEM = 75000000 )
#  elif defined C32BITS_BUILD
	  PARAMETER ( MAXMEM = 400000 )
#  else
	  PARAMETER ( MAXMEM = 50000000 )
#  endif

#endif

	END MODULE
