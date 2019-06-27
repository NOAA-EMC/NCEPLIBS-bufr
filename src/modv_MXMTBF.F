	MODULE MODV_MXMTBF

C	  MXMTBF is the maximum number of entries in the master
C	  BUFR Code/Flag tables.

C	  Note that this maximum count includes all entries across
C	  all Code and Flag tables.  In other words, each defined
C	  code figure (within each individual Code table) or defined
C	  bit number (within each individual Flag table) is counted
C	  as a separate "entry" for the purposes of this parameter
C	  value.

#ifdef DYNAMIC_ALLOCATION

C	  Set a default value for MXMTBF.  This value will be used
C	  unless it is overridden by a subsequent user call to
C	  function ISETPRM.

	  INTEGER :: MXMTBF = 25000
#else
	  PARAMETER ( MXMTBF = 25000 )
#endif

	END MODULE
