C> @file
C> @author WOOLLEN @date 2016-06-27
      
C> IGETMXBY RETURNS THE CURRENT VALUE OF MAXBYT, WHICH IS THE
C>           MAXIMUM LENGTH OF A BUFRLIB MESSAGE THAT CAN BE WRITTEN
C>           TO AN OUTPUT STREAM.  THIS VALUE IS SET TO A DEFAULT VALUE
C>           OF MIN(10000,MXMSGL) IN SUBROUTINE BFRINI, BUT APPLICATION
C>           PROGRAMS MAY SET IT TO A DIFFERENT VALUE VIA A CALL TO
C>           SUBROUTINE MAXOUT.
C>
C> PROGRAM HISTORY LOG:
C> 2016-06-27  J. ATOR -- ORIGINAL AUTHOR
C>
C> USAGE:    IGETMXBY()
C>
C>   INPUT ARGUMENTS:
C>
C>   OUTPUT ARGUMENTS:
C>     IGETMXBY - INTEGER: CURRENT VALUE OF MAXBYT = MAXIMUM LENGTH OF
C>                A BUFRLIB MESSAGE THAT CAN BE WRITTEN TO OUTPUT
C>
C> REMARKS:
C>    THIS ROUTINE CALLS: 	OPENBF
C>
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
      INTEGER FUNCTION IGETMXBY()



      USE MODA_BITBUF

      INCLUDE 'bufrlib.inc'

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------

      CALL OPENBF(0,'FIRST',0)

      IGETMXBY = MAXBYT

      RETURN
      END
