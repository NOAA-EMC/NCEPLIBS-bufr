	SUBROUTINE STRBTM ( N, LUN )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    STRBTM
C   PRGMMR: J. ATOR          ORG: NCEP       DATE: 2016-05-27
C
C ABSTRACT: THIS SUBROUTINE STORES INTERNAL INFORMATION IN
C   MODULE BITMAPS IF THE INPUT ELEMENT IS PART OF A BITMAP.
C
C PROGRAM HISTORY LOG:
C 2016-05-27  J. ATOR    -- ORIGINAL AUTHOR
C 2019-05-22  J. ATOR    -- ADD CONFIRMATION CHECK
C
C USAGE:    CALL STRBTM ( N, LUN )
C   INPUT ARGUMENT LIST:
C     N        - INTEGER: SUBSET ELEMENT
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C   -----------------------------------------------------------------
C
C    THE FOLLOWING VALUES ARE STORED WITHIN MODULE BITMAPS BY THIS
C    SUBROUTINE AND BY SUBPROGRAMS IGETRFEL, MAKESTAB AND TABSUB:
C
C	NBTM = number of stored bitmaps for the current subset (up to
C	       a maximum of MXBTM)
C
C	NBTMSE(I=1,NBTM) = number of "set" entries (i.e. set to a value
C			   of 0) in the bitmap
C
C	LINBTM = logical variable set to .TRUE. if a bitmap is currently
C		 being read for the current subset
C		 
C	ISTBTM(I=1,NBTM) = ordinal position in subset corresponding to
C		 first entry of bitmap
C
C	ISZBTM(I=1,NBTM) = size of bitmap (i.e. total number of entries,
C			   whether "set" or not)
C
C	IBTMSE(I=1,NBTM, J=1,NBTMSE(I)) =
C		ordinal positions in bitmap of bits that were "set";
C		these ordinal positions can range in value from 1 to
C		ISZBTM(I)
C
C	LSTNOD = last jump/link table entry that was processed by
C		 function IGETRFEL and whose corresponding value
C		 type was either numeric or CCITT IA5
C
C	LSTNODCT = current count of consecutive occurrences of LSTNOD
C
C	NTAMC = number of Table A mnemonics in jump/link table (up to a
C		maximum of MXTAMC) which contain at least one Table C
C		operator with an X value of 21 or greater in their
C		definition; only Table C operators with an X value of 21
C		or greater are tracked in this module, since all others
C		(e.g. 2-01, 2-02, 2-07) are automatically processed
C		within subroutines TABSUB and TABENT
C
C	INODTAMC(I=1,NTAMC) = location of Table A mnemonic within
C			      jump/link table
C
C	NTCO(I=1,NTAMC) = number of Table C operators (with an X value
C			  of 21 or greater) within the definition of the
C			  given Table A mnemonic
C
C	CTCO(I=1,NTAMC, J=1,NTCO(I)) = Table C operator
C
C	INODTCO(I=1,NTAMC, J=1,NTCO(I)) =
C		location of Table C operator within jump/link table
C
C   -----------------------------------------------------------------
C
C    THIS ROUTINE CALLS:        BORT     IBFMS    LSTJPB
C    THIS ROUTINE IS CALLED BY: RCSTPL   RDCMPS
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$
	
	USE MODA_MSGCWD
	USE MODA_USRINT
	USE MODA_TABLES
	USE MODA_BITMAPS

	INCLUDE 'bufrlib.prm'

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
