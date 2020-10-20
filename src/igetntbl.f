C> @file
C> @author ATOR @date 2007-01-19
	
C> THIS FUNCTION GETS THE NEXT LINE FROM THE ASCII MASTER
C>   TABLE FILE SPECIFIED BY LUNT, IGNORING ANY BLANK LINES OR COMMENT
C>   LINES IN THE PROCESS.
C>
C> PROGRAM HISTORY LOG:
C> 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR
C>
C> USAGE:    IGETNTBL ( LUNT, LINE )
C>   INPUT ARGUMENT LIST:
C>     LUNT     - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE
C>                CONTAINING MASTER TABLE INFORMATION
C>
C>   OUTPUT ARGUMENT LIST:
C>     LINE     - CHARACTER*(*): NEXT NON-BLANK, NON-COMMENT LINE READ
C>                FROM LUNT
C>     IGETNTBL - INTEGER: RETURN CODE:
C>                       0 = normal return
C>                      -1 = end-of-file encountered while reading
C>                           from LUNT
C>                      -2 = I/O error encountered while reading
C>                           from LUNT
C> REMARKS:
C>    THIS ROUTINE CALLS:        None
C>    THIS ROUTINE IS CALLED BY: GETNTBE  GETTBH   SNTBDE   SNTBFE
C>                               Normally not called by any application
C>                               programs.
C>
	FUNCTION IGETNTBL ( LUNT, LINE )



	CHARACTER*(*)	LINE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

  10	READ ( LUNT, '(A)', END=100, ERR=200 ) LINE
	IF ( ( LINE .EQ. ' ' ) .OR. ( LINE(1:1) .EQ. '#' ) ) GOTO 10
	IF ( LINE(1:3) .EQ. 'END' ) GOTO 100

	IGETNTBL = 0
	RETURN

 100	IGETNTBL = -1
	RETURN

 200	IGETNTBL = -2
	RETURN

	END
