	SUBROUTINE GETLTDS ( iunrrs, rltds )

C*	determines rltds (# seconds elapsed since launch time)
C*	for the current subset

C*	iunrrs is input
C*	rltds is output

	REAL*8          rltds, dtims (6,2)

	CALL UFBREP ( iunrrs, dtims, 6, 2, ndtims,
     +	  'YEAR MNTH DAYS HOUR MINU SECO' )

	CALL CTIMDIF ( dtims(1,1), dtims(2,1), dtims(3,1),
     +		       dtims(4,1), dtims(5,1), dtims(6,1),
     +		       dtims(1,2), dtims(2,2), dtims(3,2),
     +		       dtims(4,2), dtims(5,2), dtims(6,2),
     +		       rltds )

	RETURN
	END
