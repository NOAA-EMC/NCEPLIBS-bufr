C-----------------------------------------------------------------------
C	Maximum number of entries in the internal string cache.

	PARAMETER ( MXS = 1000 )
C-----------------------------------------------------------------------
C	BUFRLIB "missing" value.  The default value for BMISS is set
C	within subroutine bfrini(), but it can be modified by the user
C	via a subsequent call to subroutine setbmiss().

	COMMON /BUFRBMISS/ BMISS
	REAL*8  BMISS
C-----------------------------------------------------------------------
