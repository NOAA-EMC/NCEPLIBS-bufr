C> @file
C> @author J @date 2016-05-27
	
C> THIS SUBROUTINE STORES INTERNAL INFORMATION IN
C>   MODULE BITMAPS IF THE INPUT ELEMENT IS PART OF A BITMAP.
C>
C> PROGRAM HISTORY LOG:
C> 2016-05-27  J. ATOR    -- ORIGINAL AUTHOR
C> 2019-05-22  J. ATOR    -- ADD CONFIRMATION CHECK
C>
C> USAGE:    CALL STRBTM ( N, LUN )
C>   INPUT ARGUMENT LIST:
C>     N        - INTEGER: SUBSET ELEMENT
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>
C>    THIS ROUTINE CALLS:        BORT     IBFMS    LSTJPB
C>    THIS ROUTINE IS CALLED BY: RCSTPL   RDCMPS
C>                               Normally not called by any application
C>                               programs.
C>
	SUBROUTINE STRBTM ( N, LUN )

        USE MODV_MXBTM
        USE MODV_MXBTMSE

	USE MODA_MSGCWD
	USE MODA_USRINT
	USE MODA_TABLES
	USE MODA_BITMAPS

        LOGICAL ISBTME

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	NODE = INV( N, LUN )

	IF ( TAG(NODE)(1:5) .EQ. 'DPRI ' ) THEN

C           Confirm that this is really an entry within a bitmap.
C           Although it is rare, it is possible for a DPRI element
C           to appear in a subset definition outside of a bitmap.

            ISBTME = .FALSE.
            IF ( NTAMC .GT. 0 ) THEN
                NODTAM = LSTJPB( NODE, LUN, 'SUB' )
                DO II = 1, NTAMC
                  IF ( NODTAM .EQ. INODTAMC(II) ) THEN
                    DO JJ = 1, NTCO(II)
                      IF ( ( INODTCO(II,JJ) .GE. INODE(LUN) ) .AND.
     .                     ( INODTCO(II,JJ) .LE. ISC(INODE(LUN)) ) .AND.
     .                     ( INODTCO(II,JJ) .LT. NODE ) ) THEN
                        IF ( CTCO(II,JJ) .EQ. '236000' ) THEN
                          ISBTME = .TRUE.
                        ELSE IF ( ( CTCO(II,JJ) .EQ. '235000' ) .OR.
     .                            ( CTCO(II,JJ) .EQ. '237255' ) ) THEN
                          ISBTME = .FALSE.
                        END IF
                      END IF
                    END DO
                  END IF
                END DO
            END IF
            IF ( .NOT. ISBTME ) THEN
              LINBTM = .FALSE.
              RETURN
	    ELSE IF ( .NOT. LINBTM ) THEN

C		This is the start of a new bitmap.

		IF ( NBTM .GE. MXBTM ) GOTO 900
		NBTM = NBTM + 1
		ISTBTM(NBTM) = N
		ISZBTM(NBTM) = 0
		NBTMSE(NBTM) = 0
		LINBTM = .TRUE.
	    END IF
	    ISZBTM(NBTM) = ISZBTM(NBTM) + 1
	    IF ( IBFMS(VAL(N,LUN)) .EQ. 0 ) THEN

C		This is a "set" (value=0) entry in the bitmap.

		IF ( NBTMSE(NBTM) .GE. MXBTMSE ) GOTO 901
		NBTMSE(NBTM) = NBTMSE(NBTM) + 1
		IBTMSE(NBTM,NBTMSE(NBTM)) = ISZBTM(NBTM)
	    END IF
	ELSE IF ( ITP(NODE) .GT. 1 ) THEN
	    LINBTM = .FALSE.
	END IF

	RETURN
900	CALL BORT('BUFRLIB: STRBTM - MXBTM OVERFLOW')
901	CALL BORT('BUFRLIB: STRBTM - MXBTMSE OVERFLOW')
	END
