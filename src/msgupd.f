C> @file
C> @brief Write an uncompressed BUFR data subset.
C>
C> @author Woollen @date 1994-01-06

C> This subroutine packs up the current subset within memory
C> (array ibay in module bitbuf) and then tries to add it to
C> the BUFR message that is currently open within memory for LUNIT
C> (array mbay in module bitbuf). If the subset will not fit
C> into the currently open message, or if the subset byte count exceeds
C> 65530 (sufficiently close to the 16-bit byte counter upper limit of
C> 65535), then that message is flushed to LUNIT and a new one is
C> created in order to hold the current subset. Any subset with byte
C> count > 65530 will be written into its own one-subset message.
C> if the current subset is larger than the maximum message length,
C> then the subset is discarded and a diagnostic is printed.
C>
C> @param[in] LUNIT - integer: fortran logical unit number for BUFR file.
C> @param[in] LUN - integer: I/O stream index into internal memory arrays
C> (associated with file connected to logical unit LUNIT).
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE MSGUPD(LUNIT,LUN)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_H4WLC

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

      IF(MSGFULL(MBYT(LUN),IBYT,MAXBYT)) THEN
c       This is an overlarge subset that won't fit in any message
c       given the current value of MAXBYT, so discard the subset
c       and exit gracefully.
        IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        WRITE ( UNIT=ERRSTR, FMT='(A,A,I7,A)')
     .   'BUFRLIB: MSGUPD - SUBSET LONGER THAN ANY POSSIBLE MESSAGE ',
     .   '{MAXIMUM MESSAGE LENGTH = ', MAXBYT, '}'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('>>>>>>>OVERLARGE SUBSET DISCARDED FROM FILE<<<<<<<<')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
        ENDIF
        GOTO 100
      ENDIF

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
         IF(IPRT.GE.1) THEN
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

100   CALL USRTPL(LUN,1,1)

C  EXIT
C  ----

      RETURN
      END
