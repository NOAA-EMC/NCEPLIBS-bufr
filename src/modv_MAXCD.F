	MODULE MODV_MAXCD

C	  MAXCD is the maximum number of child descriptors that can
C	  be included within the sequence definition of a Table D
C	  descriptor.

C	  This value must be identically defined in the C header
C	  file bufrlib.h

C	  Note that this value does *not* need to take into account
C	  the recursive resolution of any child descriptors which may
C	  themselves be Table D descriptors.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MAXCD.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MAXCD = 250
#else
	  PARAMETER ( MAXCD = 250 )
#endif

	END MODULE
