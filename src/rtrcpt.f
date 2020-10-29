C> @file
C> @author ATOR @date 2009-03-23
      
C> THIS SUBROUTINE RETURNS THE TANK RECEIPT TIME STORED WITHIN
C>   SECTION 1 OF THE BUFR MESSAGE OPEN FOR INPUT VIA A PREVIOUS CALL TO
C>   BUFR ARCHIVE LIBRARY SUBROUTINE READMG, READMM OR EQUIVALENT.
C>
C> PROGRAM HISTORY LOG:
C> 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C> 2013-10-07  J. ATOR    -- MODIFIED TO CALL RTRCPTB
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL RTRCPT (LUNIT,IYR,IMO,IDY,IHR,IMI,IRET) 
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>
C>   OUTPUT ARGUMENT LIST:
C>     IYR      - INTEGER: TANK RECEIPT YEAR
C>     IMO      - INTEGER: TANK RECEIPT MONTH
C>     IDY      - INTEGER: TANK RECEIPT DAY
C>     IHR      - INTEGER: TANK RECEIPT HOUR
C>     IMI      - INTEGER: TANK RECEIPT MINUTE
C>     IRET     - INTEGER: RETURN CODE:
C>                       0 = normal return
C>                      -1 = no tank receipt time was present within the
C>                           BUFR message currently open for input
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     RTRCPTB   STATUS
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
      SUBROUTINE RTRCPT(LUNIT,IYR,IMO,IDY,IHR,IMI,IRET)



      USE MODA_BITBUF

      INCLUDE 'bufrlib.inc'

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check the file status.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C     Unpack the tank receipt time.

      CALL RTRCPTB(MBAY(1,LUN),IYR,IMO,IDY,IHR,IMI,IRET)
	
C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: RTRCPT - INPUT BUFR FILE IS CLOSED; IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: RTRCPT - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT; IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: RTRCPT - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE; NONE ARE')
      END
