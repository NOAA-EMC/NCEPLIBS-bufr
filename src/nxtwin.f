C> @file
C> @brief Computes the start and end indices of the next window.      
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced cray "abort" with bort().
C> 1999-11-18 | J. Woollen | Increased the number of open bufr files to 32.
C> 2002-05-14 | J. Woollen | Removed old cray compiler directives.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation.
C> 2009-03-31 | J. Woollen | Added additional documentation.
C> 2009-05-07 | J. Ator    | Use lstjpb instead of lstrpc.
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C>
C> @author WOOLLEN @date 1994-01-06
      
C> Given indices within the internal jump/link table which
C> point to the start and end of an "rpc" window (i.e. iteration of
C> an 8-bit or 16-bit delayed replication sequence), this subroutine
C> computes the start and end indices of the next window.
C>
C> @note See getwin() for an explanation of "windows" within the
C> context of a bufr data subset.
C>
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C> @param[inout] IWIN - integer: in: starting index of current window iteration.
C> out: starting index of next window iteration.
C> @param[inout] JWIN - integer: ending index of current window iteration.
C> out: ending index of next window iteration.
C>
C> @author WOOLLEN @date 1994-01-06
      SUBROUTINE NXTWIN(LUN,IWIN,JWIN)

      USE MODA_USRINT

      CHARACTER*128 BORT_STR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(JWIN.EQ.NVAL(LUN)) THEN
         IWIN = 0
         GOTO 100
      ENDIF

C  FIND THE NEXT SEQUENTIAL WINDOW
C  -------------------------------

      NODE = INV(IWIN,LUN)
      IF(LSTJPB(NODE,LUN,'RPC').NE.NODE) GOTO 900
      IF(VAL(JWIN,LUN).EQ.0) THEN
         IWIN = 0
      ELSE
         IWIN = JWIN
         JWIN = IWIN+VAL(IWIN,LUN)
      ENDIF

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NXTWIN - LSTJPB FOR NODE",I6," '//
     . '(LSTJPB=",I5,") DOES NOT EQUAL VALUE OF NODE, NOT RPC (IWIN '//
     . '=",I8,")")') NODE,LSTJPB(NODE,LUN,'RPC'),IWIN
      CALL BORT(BORT_STR)
      END
