	MODULE MODV_MAXSS

C	  MAXSS is the maximum number of data values that can be read
C	  from or written into a subset by the BUFRLIB software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MAXSS.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MAXSS = 80000
#else
	  PARAMETER ( MAXSS = 120000 )
#endif

	END MODULE
