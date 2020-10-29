C> @file
C> @author J @date 2014-10-02
	
C> GIVEN A TABLE B MNEMONIC, THIS SUBROUTINE RETURNS THE
C>   SCALE FACTOR, REFERENCE VALUE AND BIT WIDTH CORRESPONDING TO THE
C>   (NNEMO)th OCCURRENCE OF THAT MNEMONIC WITHIN A SUBSET
C>   DEFINITION (COUNTING FROM THE BEGINNING OF THE OVERALL SUBSET
C>   DEFINITION), AND INCLUDING ACCOUNTING FOR ANY TABLE C OPERATORS
C>   (E.G. 2-01-YYY, 2-02-YYY, 2-03-YYY, 2-07-YYY) WHICH MAY BE IN
C>   EFFECT FOR THAT PARTICULAR OCCURRENCE OF THE MNEMONIC.  A SUBSET
C>   DEFINITION MUST ALREADY BE IN SCOPE, EITHER VIA A PREVIOUS CALL TO
C>   BUFR ARCHIVE LIBRARY SUBROUTINE READSB OR EQUIVALENT (FOR INPUT
C>   FILES) OR TO SUBROUTINE OPENMB OR EQUIVALENT (FOR OUTPUT FILES).
C>
C> PROGRAM HISTORY LOG:
C> 2014-10-02  J. ATOR    -- ORIGINAL VERSION
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL NEMSPECS (LUNIT, NEMO, NNEMO, NSCL, NREF, NBTS, IRET )
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>     NEMO     - CHARACTER*(*): TABLE B MNEMONIC
C>     NNEMO    - INTEGER: ORDINAL OCCURRENCE OF NEMO FOR WHICH
C>                INFORMATION IS TO BE RETURNED, COUNTING FROM THE
C>                BEGINNING OF THE OVERALL SUBSET DEFINITION
C>
C>   OUTPUT ARGUMENT LIST:
C>     NSCL     - INTEGER: SCALE FACTOR CORRESPONDING TO NEMO
C>     NREF     - INTEGER: REFERENCE VALUE CORRESPONDING TO NEMO
C>     NBTS     - INTEGER: BIT WIDTH CORRESPONDING TO NEMO
C>     IRET     - INTEGER: RETURN CODE
C>                   0 = NORMAL RETURN
C>                  -1 = REQUESTED MNEMONIC COULD NOT BE FOUND, OR SOME
C>                       OTHER ERROR OCCURRED
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        FSTAG    STATUS   STRSUC
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs
C>
	SUBROUTINE NEMSPECS ( LUNIT, NEMO, NNEMO,
     .			      NSCL, NREF, NBTS, IRET )



	USE MODA_USRINT
	USE MODA_MSGCWD
	USE MODA_TABLES
	USE MODA_NRV203

	INCLUDE 'bufrlib.inc'

	CHARACTER*10  TAGN

	CHARACTER*(*) NEMO

C----------------------------------------------------------------------
C----------------------------------------------------------------------

	IRET = -1

C	Get LUN from LUNIT.

	CALL STATUS( LUNIT, LUN, IL, IM )
	IF ( IL .EQ. 0 ) RETURN
	IF ( INODE(LUN) .NE. INV(1,LUN) ) RETURN

C	Starting from the beginning of the subset, locate the (NNEMO)th
C	occurrence of NEMO.

	CALL FSTAG( LUN, NEMO, NNEMO, 1, NIDX, IERFST )
	IF ( IERFST .NE. 0 ) RETURN

C	Confirm that NEMO is a Table B mnemonic.

	NODE = INV(NIDX,LUN)
	IF ( ( TYP(NODE) .NE. 'NUM' ) .AND. ( TYP(NODE) .NE. 'CHR' ) )
     .	    RETURN

C	Get the scale factor, reference value and bit width, including
C	accounting for any Table C operators which may be in scope for
C	this particular occurrence of NEMO.

        IRET = 0

	NSCL = ISC(NODE)
	NBTS = IBT(NODE)
	NREF = IRF(NODE)

	IF ( NNRV .GT. 0 ) THEN

C	  There are nodes containing redefined reference values (from
C	  one or more 2-03-YYY operators) in the jump/link table, so we
C	  need to check if this node is one of them.

	  TAGN = ' '
	  CALL STRSUC( NEMO, TAGN, LTN ) 
	  IF ( ( LTN .LE. 0 ) .OR. ( LTN .GT. 8 ) ) RETURN

	  DO JJ = 1, NNRV
	    IF ( ( NODE .NE. INODNRV(JJ) ) .AND.
     .		( TAGN(1:8) .EQ. TAGNRV(JJ) ) .AND.
     .		( NODE .GE. ISNRV(JJ) ) .AND.
     .		( NODE .LE. IENRV(JJ) ) ) THEN
	      NREF = NRV(JJ)
	      RETURN
	    END IF
	  END DO

	END IF

	RETURN
	END
