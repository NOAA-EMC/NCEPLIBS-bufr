C> @file
C> @brief Computes the start and end indices of the next window.
C>
C> @author WOOLLEN @date 1994-01-06

C> Given indices within the internal jump/link table which
C> point to the start and end of an "rpc" window (which is an iteration of
C> an 8-bit or 16-bit delayed replication sequence), this subroutine
C> computes the start and end indices of the next window.
C>
C> @note See getwin() for an explanation of "windows" within the
C> context of a bufr data subset.
C>
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C> @param[inout] IWIN - integer:
C>  - on input, contains starting index of current window iteration.
C>  - on output, contains starting index of next window iteration.
C> @param[inout] JWIN - integer:
C>  - on input, contains ending index of current window iteration.
C>  - on output, contains ending index of next window iteration.
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
