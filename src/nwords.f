C> @file
C> @author WOOLLEN @date 1996-10-09
      
C> THIS FUNCTION ADDS UP THE COMPLETE LENGTH OF THE DELAYED
C>   REPLICATION SEQUENCE BEGINNING AT INDEX N OF THE DATA SUBSET.
C>
C> PROGRAM HISTORY LOG:
C> 1996-10-09  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C>                           INCREASED FROM 15000 TO 16000 (WAS IN
C>                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C>                           WRF; ADDED DOCUMENTATION (INCLUDING
C>                           HISTORY) (INCOMPLETE)
C> 2009-03-31  J. WOOLLEN -- ADDED DOCUMENTATION
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    NWORDS (N, LUN)
C>   INPUT ARGUMENT LIST:
C>     N        - INTEGER: INDEX TO START OF DELAYED REPLICATION SEQUENCE
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>
C>   OUTPUT ARGUMENT LIST:
C>     NWORDS   - INTEGER: COMPLETE LENGTH OF DELAYED REPLICATION
C>                SEQUENCE WITHIN DATA SUBSET
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        None
C>    THIS ROUTINE IS CALLED BY: INVMRG
C>                               Normally not called by any application
C>                               programs.
C>
      FUNCTION NWORDS(N,LUN)



      USE MODA_USRINT

      INCLUDE 'bufrlib.inc'

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NWORDS = 0

      DO K=1,NINT(VAL(N,LUN))
      NWORDS = NWORDS + NINT(VAL(NWORDS+N+1,LUN))
      ENDDO

      RETURN
      END
