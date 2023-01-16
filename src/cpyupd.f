C> @file
C> @brief Copy a subset from one message buffer
C> (array mbay in module bitbuf) to another and/or resets the
C> pointers. 
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced call to cray library routine "abort" with call to bort().
C> 1999-11-18 | J. Woollen | Increased num open files from 10 to 32 (necessary for mpi).
C> 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes.
C> 2002-05-14 | J. Woollen | Removed old cray compiler directives.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | Unified/portable for wrf; documentation; more complete diagnostic when routine terminates abnormally.
C> 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes.
C> 2009-03-23 | J. Ator    | Use msgfull.
C> 2014-10-27 | J. Woollen | Account for subsets with byte count > 65530 (these must be written into their own one-subset message).
C> 2014-10-27 | D. Keyser  | For case above, do not write "current" message if it contains zero subsets.
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C> 2015-09-24 | D. Stokes  | Fix missing declaration OF COMMON QUIET.
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine copies a subset from one message buffer
C> (array mbay in module bitbuf) to another and/or resets the
C> pointers. If the subset will not fit into the output message, or
C> if the subset byte count exceeds 65530 (sufficiently close to the
C> 16-bit byte counter upper limit of 65535), then that message is
C> flushed to lunit and a new one is created in order to hold the
C> copied subset. Any subset with byte count > 65530 will be written
C> into its own one-subset message. If the subset to be copied is
C> larger than the maximum message length, then a call is issued to
C> subroutine bort().
C>
C> @param[in] LUNIT - integer: fortran logical unit number for bufr file.
C> @param[in] LIN - integer: i/o stream index into internal memory arrays
C> for input message location.
C> @param[in] LUN - integer: i/o stream index into internal memory arrays
C> for output message location.
C> @param[in] IBYT - integer: number of bytes occupied by this subset.
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
