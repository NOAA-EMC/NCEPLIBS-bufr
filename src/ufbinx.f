C> @file
C> @brief Either open a bufr file connected to
C> logical unit lunit for input operations (if it is not already
C> opened as such), or saves its position and rewinds it to the first
C> data message.
C>
C> ### Program History Log
C> Date | Programmer | Comments 
C> -----|------------|----------
C> 2003-11-04 | J. Woollen | original author (was in verification version but may have been in the production version at one time and then removed)
C> 2003-11-04 | D. Keyser  | unified/portable for wrf; added documentation; outputs more complete diagnostic info
C> 2004-08-09 | J. Ator    | maximum message length increased from 20,000 to 50,000 bytes
C> 2009-03-23 | J. Ator    | modify logic to handle bufr table messages encountered anywhere in the file (and not just at the beginning!)
C> 2012-09-15 | J. Woollen | modified for c/i/o/bufr interface use 'inx' argument to openbf
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C> 2022-10-04 | J. Ator    | added 8-byte wrapper
C>
C> @author Woollen @date 2003-11-04
      
C> This subroutine either opens a bufr file connected to
C> logical unit lunit for input operations (if it is not already
C> opened as such), or saves its position and rewinds it to the first
C> data message (if bufr file already opened), then (via a call to
C> bufr archive library subroutine ufbint) reads specified values from
C> internal subset arrays associated with a particular subset from a
C> particular bufr message in a message buffer. The particular subset
C> and bufr message are based based on the subset number in the
C> message and the message number in the bufr file. Finally, this
C> subroutine either closes the bufr file in lunit (if is was opened
C> here) or restores it to its previous read/write status and position
C> (if it was not opened here). See ufbint for more information on
C> the reading of values out of a bufr message subset. @note The
C> message number here does not include the dictionary messages at the
C> beginning of the file.
C>
C> @param[in] LUNIT - integer: fortran logical unit number for bufr file.
C> @param[in] IMSG - integer: pointer to bufr message number to read in bufr file.
C> @param[in] ISUB - integer: pointer to subset number to read in bufr message.
C> @param[out] USR - real*8: (i1,i2) starting address of data values read
C> from data subset.
C> @param[in] I1 - integer: length of first dimension of usr (must be at
C> least as large as the number of blank-separated
C> mnemonics in str).
C> @param[in] I2 - integer: length of second dimension of usr.
C> @param[out] IRET - integer: number of "levels" of data values read from
C> data subset (must be no larger than i2).
C> @param[in] STR - character*(*): string of blank-separated table b.
C> mnemonics in one-to-one correspondence with first
C> dimension of usr {this can also be a single table d
C> (sequence) mnemonic with either 8- or 16-bit delayed
C> replication (see remarks 1 in ufbint docblock)}.
C>
C> @author Woollen @date 2003-11-04
      RECURSIVE SUBROUTINE UFBINX(LUNIT,IMSG,ISUB,USR,I1,I2,IRET,STR)

      USE MODV_IM8B

      USE MODA_MSGCWD
      USE MODA_BITBUF

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET
      LOGICAL       OPENIT
      REAL*8        USR(I1,I2)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(IMSG,MY_IMSG,1)
         CALL X84(ISUB,MY_ISUB,1)
         CALL X84(I1,MY_I1,1)
         CALL X84(I2,MY_I2,1)
         CALL UFBINX(MY_LUNIT,MY_IMSG,MY_ISUB,USR,MY_I1,MY_I2,IRET,STR)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL STATUS(LUNIT,LUN,IL,IM)
      OPENIT = IL.EQ.0

      IF(OPENIT) THEN

C  OPEN BUFR FILE CONNECTED TO UNIT LUNIT IF IT IS NOT ALREADY OPEN
C  ----------------------------------------------------------------

         CALL OPENBF(LUNIT,'INX',LUNIT)
      ELSE

C  IF BUFR FILE ALREADY OPENED, SAVE POSITION & REWIND TO FIRST DATA MSG
C  ---------------------------------------------------------------------

         CALL REWNBF(LUNIT,0)
      ENDIF

C  SKIP TO MESSAGE # IMSG
C  ----------------------

C     Note that we need to use subroutine READMG to actually read in all
C     of the messages (including the first (IMSG-1) messages!), just in
C     case there are any embedded dictionary messages in the file.

      DO I=1,IMSG
         CALL READMG(LUNIT,SUBSET,JDATE,JRET)
         IF(JRET.LT.0) GOTO 901
      ENDDO

C  POSITION AT SUBSET # ISUB
C  -------------------------

      DO I=1,ISUB-1
         IF(NSUB(LUN).GT.MSUB(LUN)) GOTO 902
         IBIT = MBYT(LUN)*8
         CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
         MBYT(LUN) = MBYT(LUN) + NBYT
         NSUB(LUN) = NSUB(LUN) + 1
      ENDDO

      CALL READSB(LUNIT,JRET)
      IF(JRET.NE.0) GOTO 902

      CALL UFBINT(LUNIT,USR,I1,I2,IRET,STR)

      IF(OPENIT) THEN

C  CLOSE BUFR FILE IF IT WAS OPENED HERE
C  -------------------------------------

         CALL CLOSBF(LUNIT)
      ELSE


C  RESTORE BUFR FILE TO PREV. STATUS & POSITION IF NOT ORIG. OPENED HERE
C  ---------------------------------------------------------------------

         CALL REWNBF(LUNIT,1)
      ENDIF

C  EXITS
C  -----

      RETURN
901   WRITE(BORT_STR,'("BUFRLIB: UFBINX - HIT END OF FILE BEFORE '//
     . 'READING REQUESTED MESSAGE NO.",I5," IN BUFR FILE CONNECTED TO'//
     . ' UNIT",I4)')  IMSG,LUNIT
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: UFBINX - ALL SUBSETS READ BEFORE '//
     . 'READING REQ. SUBSET NO.",I3," IN REQ. MSG NO.",I5," IN BUFR '//
     . 'FILE CONNECTED TO UNIT",I4)') ISUB,IMSG,LUNIT
      CALL BORT(BORT_STR)
      END
