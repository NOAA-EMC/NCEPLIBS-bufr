	MODULE MODV_MXS01V

C	  MXS01V is the maximum number of default Section 0 or
C	  Section 1 values that can be overwritten within an output
C	  BUFR message by the BUFRLIB software.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXS01V.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXS01V = 10
#else
	  PARAMETER ( MXS01V = 10 )
#endif

	END MODULE
