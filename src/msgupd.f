      SUBROUTINE MSGUPD(LUNIT,LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MSGUPD
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE PACKS UP THE CURRENT SUBSET WITHIN MEMORY
C  (ARRAY IBAY IN MODULE BITBUF) AND THEN TRIES TO ADD IT TO
C  THE BUFR MESSAGE THAT IS CURRENTLY OPEN WITHIN MEMORY FOR LUNIT
C  (ARRAY MBAY IN MODULE BITBUF).  IF THE SUBSET WILL NOT FIT
C  INTO THE CURRENTLY OPEN MESSAGE, OR IF THE SUBSET BYTE COUNT EXCEEDS
C  65530 (SUFFICIENTLY CLOSE TO THE 16-BIT BYTE COUNTER UPPER LIMIT OF
C  65535), THEN THAT MESSAGE IS FLUSHED TO LUNIT AND A NEW ONE IS
C  CREATED IN ORDER TO HOLD THE CURRENT SUBSET.  ANY SUBSET WITH BYTE
C  COUNT > 65530 WILL BE WRITTEN INTO ITS OWN ONE-SUBSET MESSAGE.
C  IF THE CURRENT SUBSET IS LARGER THAN THE MAXIMUM MESSAGE LENGTH,
C  THEN THE SUBSET IS DISCARDED AND A DIAGNOSTIC IS PRINTED.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 1998-12-14  J. WOOLLEN -- NO LONGER CALLS BORT IF A SUBSET IS LARGER
C                           THAN A MESSAGE, JUST DISCARDS THE SUBSET
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2009-03-23  J. ATOR    -- USE MSGFULL AND ERRWRT
C 2014-10-20  J. WOOLLEN -- ACCOUNT FOR SUBSETS WITH BYTE COUNT > 65530
C                           (THESE MUST BE WRITTEN INTO THEIR OWN
C                           ONE-SUBSET MESSAGE)
C 2014-10-20  D. KEYSER  -- FOR CASE ABOVE, DO NOT WRITE "CURRENT"
C                           MESSAGE IF IT CONTAINS ZERO SUBSETS
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C
C USAGE:    CALL MSGUPD (LUNIT, LUN)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT)
C
C REMARKS:
C    THIS ROUTINE CALLS:        ERRWRT   IUPB     MSGFULL  MSGINI
C                               MSGWRT   MVB      PAD      PKB
C                               USRTPL   WRITLC
C    THIS ROUTINE IS CALLED BY: WRITSA   WRITSB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_H4WLC

      INCLUDE 'bufrlib.prm'

      COMMON /MSGPTR/ NBY0,NBY1,NBY2,NBY3,NBY4,NBY5
      COMMON /QUIET / IPRT

      LOGICAL MSGFULL

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  PAD THE SUBSET BUFFER
C  ---------------------

      CALL PAD(IBAY,IBIT,IBYT,8)

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

C  SET A BYTE COUNT AND TRANSFER THE SUBSET BUFFER INTO THE MESSAGE
C  ----------------------------------------------------------------

      LBIT = 0
      CALL PKB(IBYT,16,IBAY,LBIT)

C     Note that we want to append the data for this subset to the end
C     of Section 4, but the value in MBYT(LUN) already includes the
C     length of Section 5 (i.e. 4 bytes).  Therefore, we need to begin
C     writing at the point 3 bytes prior to the byte currently pointed
C     to by MBYT(LUN).

      CALL MVB(IBAY,1,MBAY(1,LUN),MBYT(LUN)-3,IBYT)

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

C  IF ANY LONG CHARACTER STRINGS ARE BEING HELD INTERNALLY FOR STORAGE
C  INTO THIS SUBSET, STORE THEM NOW.
C  -------------------------------------------------------------------

      IF(NH4WLC.GT.0) THEN
        DO II = 1, NH4WLC
          CALL WRITLC(LUH4WLC(II),CHH4WLC(II),STH4WLC(II))
        ENDDO
        NH4WLC = 0
      ENDIF

C  IF THE SUBSET BYTE COUNT IS > 65530, THEN GIVE IT ITS OWN ONE-SUBSET
C  MESSAGE (CANNOT HAVE ANY OTHER SUBSETS IN THIS MESSAGE BECAUSE THEIR
C  BEGINNING WOULD BE BEYOND THE UPPER LIMIT OF 65535 IN THE 16-BIT
C  BYTE COUNTER, MEANING THEY COULD NOT BE LOCATED!)
C  --------------------------------------------------------------------

      IF(IBYT.GT.65530) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,I7,A,A)')
     . 'BUFRLIB: MSGUPD - SUBSET HAS BYTE COUNT = ',IBYT,' > UPPER ',
     . 'LIMIT OF 65535'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('>>>>>>>WILL BE WRITTEN INTO ITS OWN MESSAGE<<<<<<<<')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         CALL MSGWRT(LUNIT,MBAY(1,LUN),MBYT(LUN))
         CALL MSGINI(LUN)
      ENDIF

C  RESET THE USER ARRAYS AND EXIT NORMALLY
C  ---------------------------------------

      CALL USRTPL(LUN,1,1)
      GOTO 100

C  ON ENCOUTERING OVERLARGE SUBSETS, EXIT GRACEFULLY (SUBSET DISCARDED)
C  --------------------------------------------------------------------

900   IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I7,A)')
     . 'BUFRLIB: MSGUPD - SUBSET LONGER THAN ANY POSSIBLE MESSAGE ',
     . '{MAXIMUM MESSAGE LENGTH = ', MAXBYT, '}'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('>>>>>>>OVERLARGE SUBSET DISCARDED FROM FILE<<<<<<<<')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
