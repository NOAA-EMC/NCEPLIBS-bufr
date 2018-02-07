
C	The following module is used to share information between
C	subroutine FDEBUFR and subroutine OPENBT, since the latter
C	is not called by the former but rather is called directly
C	from within the NCEP BUFRLIB.

	MODULE Share_Table_Info
	    CHARACTER*120	ctbldir
	    INTEGER		ltbd, ludx
	END MODULE
