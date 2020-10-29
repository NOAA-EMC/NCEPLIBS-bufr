C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE COPIES A SUBSET FROM ONE MESSAGE BUFFER
C>   (ARRAY MBAY IN MODULE BITBUF) TO ANOTHER AND/OR RESETS THE
C>   POINTERS.  IF THE SUBSET WILL NOT FIT INTO THE OUTPUT MESSAGE, OR
C>   IF THE SUBSET BYTE COUNT EXCEEDS 65530 (SUFFICIENTLY CLOSE TO THE
C>   16-BIT BYTE COUNTER UPPER LIMIT OF 65535), THEN THAT MESSAGE IS
C>   FLUSHED TO LUNIT AND A NEW ONE IS CREATED IN ORDER TO HOLD THE
C>   COPIED SUBSET.  ANY SUBSET WITH BYTE COUNT > 65530 WILL BE WRITTEN
C>   INTO ITS OWN ONE-SUBSET MESSAGE.  IF THE SUBSET TO BE COPIED IS
C>   LARGER THAN THE MAXIMUM MESSAGE LENGTH, THEN A CALL IS ISSUED TO
C>   BUFR ARCHIVE LIBRARY SUBROUTINE BORT.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           10,000 TO 20,000 BYTES
C> 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY
C> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> 2009-03-23  J. ATOR    -- USE MSGFULL
C> 2014-10-27  J. WOOLLEN -- ACCOUNT FOR SUBSETS WITH BYTE COUNT > 65530
C>                           (THESE MUST BE WRITTEN INTO THEIR OWN
C>                           ONE-SUBSET MESSAGE)
C> 2014-10-27  D. KEYSER  -- FOR CASE ABOVE, DO NOT WRITE "CURRENT"
C>                           MESSAGE IF IT CONTAINS ZERO SUBSETS
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C> 2015-09-24  D. STOKES  -- FIX MISSING DECLARATION OF COMMON /QUIET/
C>
C> USAGE:    CALL CPYUPD (LUNIT, LIN, LUN, IBYT)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>     LIN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>                FOR INPUT MESSAGE LOCATION
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>                FOR OUTPUT MESSAGE LOCATION
C>     IBYT     - INTEGER: NUMBER OF BYTES OCCUPIED BY THIS SUBSET
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     ERRWRT   IUPB     MSGFULL
C>                               MSGINI   MSGWRT   MVB      PKB
C>    THIS ROUTINE IS CALLED BY: COPYSB
C>                               Normally not called by any application
C>                               programs.
C>
      SUBROUTINE CPYUPD(LUNIT,LIN,LUN,IBYT)



      USE MODA_MSGCWD
      USE MODA_BITBUF

      INCLUDE 'bufrlib.inc'

      COMMON /MSGPTR/ NBY0,NBY1,NBY2,NBY3,NBY4,NBY5

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR, ERRSTR

      LOGICAL MSGFULL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK WHETHER THE NEW SUBSET SHOULD BE WRITTEN INTO THE CURRENTLY
C  OPEN MESSAGE
C  -----------------------------------------------------------------

      IF(MSGFULL(MBYT(LUN),IBYT,MAXBYT)
     .      .OR.
     .  ((IBYT.GT.65530).AND.(NSUB(LUN).GT.0))) THEN
c       NO it should not, either because:
c        1) it doesn't fit,
c                -- OR --
c        2) it has byte count > 65530 (sufficiently close to the
c           upper limit for the 16 bit byte counter placed at the
c           beginning of each subset), AND the current message has
c           at least one subset in it,
c       SO write the current message out and create a new one to
c       hold the current subset
         CALL MSGWRT(LUNIT,MBAY(1,LUN),MBYT(LUN))
         CALL MSGINI(LUN)
      ENDIF

      IF(MSGFULL(MBYT(LUN),IBYT,MAXBYT)) GOTO 900

C  TRANSFER SUBSET FROM ONE MESSAGE TO THE OTHER
C  ---------------------------------------------

C     Note that we want to append the data for this subset to the end
C     of Section 4, but the value in MBYT(LUN) already includes the
C     length of Section 5 (i.e. 4 bytes).  Therefore, we need to begin
C     writing at the point 3 bytes prior to the byte currently pointed
C     to by MBYT(LUN).

      CALL MVB(MBAY(1,LIN),MBYT(LIN)+1,MBAY(1,LUN),MBYT(LUN)-3,IBYT)

C  UPDATE THE SUBSET AND BYTE COUNTERS
C  --------------------------------------

      MBYT(LUN)   = MBYT(LUN)   + IBYT
      NSUB(LUN)   = NSUB(LUN)   + 1

      LBIT = (NBY0+NBY1+NBY2+4)*8
      CALL PKB(NSUB(LUN),16,MBAY(1,LUN),LBIT)

      LBYT = NBY0+NBY1+NBY2+NBY3
      NBYT = IUPB(MBAY(1,LUN),LBYT+1,24)
      LBIT = LBYT*8
      CALL PKB(NBYT+IBYT,24,MBAY(1,LUN),LBIT)

C  IF THE SUBSET BYTE COUNT IS > 65530, THEN GIVE IT ITS OWN ONE-SUBSET
C  MESSAGE (CANNOT HAVE ANY OTHER SUBSETS IN THIS MESSAGE BECAUSE THEIR
C  BEGINNING WOULD BE BEYOND THE UPPER LIMIT OF 65535 IN THE 16-BIT
C  BYTE COUNTER, MEANING THEY COULD NOT BE LOCATED!)
C  --------------------------------------------------------------------

      IF(IBYT.GT.65530) THEN
         IF(IPRT.GE.1) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I7,A,A)')
     . 'BUFRLIB: CPYUPD - SUBSET HAS BYTE COUNT = ',IBYT,' > UPPER ',
     . 'LIMIT OF 65535'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('>>>>>>>WILL BE COPIED INTO ITS OWN MESSAGE<<<<<<<<')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         CALL MSGWRT(LUNIT,MBAY(1,LUN),MBYT(LUN))
         CALL MSGINI(LUN)
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: CPYUPD - THE LENGTH OF THIS SUBSET '//
     . 'EXCEEDS THE MAXIMUM MESSAGE LENGTH (",I6,")")') MAXBYT
      CALL BORT(BORT_STR)
      END
