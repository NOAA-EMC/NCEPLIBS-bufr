C> @file
C> @author J @date 2009-11-30
      
C> THIS FUNCTION DETERMINES WHETHER LOGICAL UNIT IOLUN(LUN) IS
C>   SHARING INTERNAL TABLE INFORMATION WITH LOGICAL UNIT IOLUN(LUD).
C>   NOTE THAT TWO LOGICAL UNITS CAN HAVE THE SAME INTERNAL TABLE
C>   INFORMATION WITHOUT ACTUALLY SHARING IT.
C>
C> PROGRAM HISTORY LOG:
C> 2009-11-30  J. ATOR    -- ORIGINAL AUTHOR
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    ISHRDX (LUD, LUN)
C>   INPUT ARGUMENT LIST:
C>     LUD      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>                FOR FIRST LOGICAL UNIT
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>                FOR SECOND LOGICAL UNIT
C>
C>   OUTPUT ARGUMENT LIST:
C>     ISHRDX   - INTEGER: RETURN CODE INDICATING WHETHER IOLUN(LUN)
C>                IS SHARING TABLE INFORMATION WITH IOLUN(LUD):
C>                  0 - NO
C>                  1 - YES
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        None
C>    THIS ROUTINE IS CALLED BY: ICMPDX  MAKESTAB
C>                               Normally not called by any application
C>                               programs.
C>
      INTEGER FUNCTION ISHRDX(LUD,LUN)



      USE MODA_TABABD

      INCLUDE 'bufrlib.inc'

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Note that, for any I/O stream index value LUx, the MTAB(*,LUx)
C     array contains pointer indices into the internal jump/link table
C     for each of the Table A mnemonics that is currently defined for
C     that LUx value.  Thus, if all of these indices are identical for
C     two different LUx values, then the associated logical units are
C     sharing table information.

      IF ( ( NTBA(LUD) .GE. 1 ) .AND.
     +	    ( NTBA(LUD) .EQ. NTBA(LUN) ) ) THEN
	II = 1
	ISHRDX = 1
	DO WHILE ( ( II .LE. NTBA(LUD) ) .AND. ( ISHRDX .EQ. 1 ) )
	  IF ( ( MTAB(II,LUD) .NE. 0 ) .AND.
     +		( MTAB(II,LUD) .EQ. MTAB(II,LUN) ) ) THEN
	    II = II + 1
	  ELSE
	    ISHRDX = 0
	  ENDIF
	ENDDO
      ELSE
	ISHRDX = 0
      ENDIF

      RETURN
      END
