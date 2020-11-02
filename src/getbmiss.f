C> @file
C> @author WOOLLEN @date 2012-09-15
      
C> GETBMISS RETURNS THE CURRENT VALUE OF "BMISS" WHICH DENOTES
C>           MISSING VALUES BOTH FOR READING FROM BUFR FILES AND FOR
C>           WRITING TO BUFR FILES.  THIS MISSING VALUE IS SET TO A
C>           DEFAULT VALUE OF 10E10 IN SUBROUTINE BFRINI, BUT APPLICATION
C>           PROGRAMS MAY SET IT TO A DIFFERENT VALUE VIA A CALL TO
C>           SUBROUTINE SETBMISS.
C>
C> PROGRAM HISTORY LOG:
C> 2012-10-05  J. ATOR -- ORIGINAL AUTHOR
C>
C> USAGE:    GETBMISS()
C>
C>   INPUT ARGUMENTS:
C>
C>   OUTPUT ARGUMENTS:
C>     GETBMISS - REAL*8: CURRENT VALUE OF BUFR ARCHIVE LIBRARY MISSING
C>                VALUE "BMISS"
C>
C> REMARKS:
C>    THIS ROUTINE CALLS: 	OPENBF
C>
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
      REAL*8 FUNCTION GETBMISS()



      INCLUDE 'bufrlib.inc'

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------

      CALL OPENBF(0,'FIRST',0)

      GETBMISS = BMISS

      RETURN
      END
