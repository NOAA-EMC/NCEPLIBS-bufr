	MODULE MODV_MAXMSG

C	  MAXMSG is the maximum number of BUFR messages that can be
C	  stored within internal memory.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MAXMSG.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MAXMSG = 200000

#else

#  ifdef C32BITS_BUILD
	  PARAMETER ( MAXMSG = 20000 )
#  else
	  PARAMETER ( MAXMSG = 200000 )
#  endif

#endif

	END MODULE
