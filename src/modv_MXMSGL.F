	MODULE MODV_MXMSGL

C	  MXMSGL is the maximum length (in bytes) of a BUFR message that
C	  can be read or written by the BUFRLIB software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXMSGL.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXMSGL = 200000
	  INTEGER :: MXMSGLD4

#else

#  ifdef SUPERSIZE_BUILD
	  PARAMETER ( MXMSGL = 2500000 )
#  else
	  PARAMETER ( MXMSGL = 600000 )
#  endif
	  PARAMETER ( MXMSGLD4 = MXMSGL/4 )

#endif

	END MODULE
