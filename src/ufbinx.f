C> @file
C> @brief Read one or more data values from a data subset.
C>
C> @author Woollen @date 2003-11-04

C> Read one or more data values from a data subset.
C>
C> If logical unit LUNIT has already been opened for input operations
C> via a previous call to subroutine openbf(), then this subroutine
C> will save the current file position, rewind the file to the
C> beginning, reposition the file to a specified data subset
C> within a specified message, read one or more specified data values
C> from that data subset via an internal call to ufbint(), and then
C> restore the file to its previous position.
C>
C> Otherwise, if logical unit LUNIT has not already been opened for
C> input operations via a previous call to subroutine openbf(),
C> then this subroutine will open it via an internal call to
C> subroutine openbf(), position the file to a specified data subset
C> within a specified message, read one or more specified data values
C> from that data subset via an internal call to ufbint(), and then
C> close the file via an internal call to subroutine closbf().
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file.
C> @param[in] IMSG  - integer: Number of BUFR message to be read from
C> the BUFR file, counting from the beginning of the file, but <b>not</b>
C> counting any DX BUFR table messages which may be present in the file
C> @param[in] ISUB  - integer: Number of data subset to be read from
C> read from the (IMSG)th BUFR message, counting from the beginning of
C> the message
C> @param[out] USR - real*8(*,*): Data values
C> @param[in] I1 - integer: First dimension of USR as allocated within the
C> calling program
C> @param[in] I2 - integer: Second dimension of USR as allocated within the
C> calling program
C> @param[out] IRET - integer: Number of replications of STR that were read
C> from the data subset
C> @param[in] STR - character*(*): string of blank-separated Table B
C> mnemonics in one-to-one correspondence with the number of data values
C> that will be read from the data subset into the first dimension of USR
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
