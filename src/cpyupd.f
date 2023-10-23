C> @file
C> @brief Copy a BUFR data subset.
C>
C> @author Woollen @date 1994-01-06

C> Copy a BUFR data subset from one unit
C> to another within internal memory and reset the pointers.
C> If the subset will not fit into the output message, or
C> if the subset byte count exceeds 65530 (sufficiently close to the
C> 16-bit byte counter upper limit of 65535), then that message is
C> flushed to lunit and a new one is created in order to hold the
C> copied subset. Any subset with byte count > 65530 will be written
C> into its own one-subset message. If the subset to be copied is
C> larger than the maximum message length, then a call is issued to
C> subroutine bort().
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file
C>                    associated with output unit.
C> @param[in] LIN - integer: I/O stream index into internal memory arrays
C>                  for input unit
C> @param[in] LUN - integer: I/O stream index into internal memory arrays
C>                  for output unit.
C> @param[in] IBYT - integer: length (in bytes) of data subset
C>
C> @author Woollen @date 1994-01-06

      SUBROUTINE CPYUPD(LUNIT,LIN,LUN,IBYT)

      USE MODA_MSGCWD
      USE MODA_BITBUF

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
