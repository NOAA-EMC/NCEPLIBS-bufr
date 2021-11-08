C> @file
C> @brief Read the next data subset from a BUFR message.
      
C> This subroutine reads the next data subset from a BUFR
C> message into internal arrays.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR file
C> @param[out] IRET   -- integer: return code
C>                           - 0 = new BUFR data subset was successfully
C>                                 read into internal arrays
C>                           - -1 = there are no more BUFR data subsets in
C>                                 the BUFR message
C>
C> <p>Logical unit LUNIT should have already been opened for
C> input operations via a previous call to subroutine openbf(), and a
C> BUFR message should have already been read into internal arrays via
C> a previous call to one of the 
C> [message-reading subroutines](@ref hierarchy).
C>
C> <p>Whenever this subroutine returns with IRET = 0, this indicates
C> that a new BUFR data subset (i.e. report) was successfully read into
C> internal arrays within the BUFRLIB software, and from where it can
C> then be easily manipulated or further parsed via calls to any of the
C> [values-reading subroutines](@ref hierarchy).
C> Otherwise, if the subroutine returns with IRET = -1, then this
C> indicates that there are no more data subsets available within the
C> current message, and therefore that a new call needs to be made to
C> one of the [message-reading subroutines](@ref hierarchy) in order
C> to read in the next message from logical unit LUNIT.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2000-09-19 | J. Woollen | Added call to new routine rdcmps() allowing subsets to also be decoded from compressed BUFR messages; maximum length increased from 10,000 to 20,000 bytes |
C> | 2002-05-14 | J. Woollen | Corrected error relating to certain foreign file types; removed old Cray compiler directives |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
C> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C>
      SUBROUTINE READSB(LUNIT,IRET)

      USE MODA_MSGCWD
      USE MODA_UNPTYP
      USE MODA_BITBUF
      USE MODA_BITMAPS

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) THEN
         IRET = -1
         GOTO 100
      ENDIF

C  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE
C  ---------------------------------------------

      IF(NSUB(LUN).EQ.MSUB(LUN)) THEN
         IRET = -1
         GOTO 100
      ELSE
         NSUB(LUN) = NSUB(LUN) + 1
      ENDIF

C  READ THE NEXT SUBSET AND RESET THE POINTERS
C  -------------------------------------------

      NBTM = 0
      LSTNOD = 0
      LSTNODCT = 0
      LINBTM = .FALSE.

      IF(MSGUNP(LUN).EQ.0) THEN
         IBIT = MBYT(LUN)*8
         CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
         CALL RDTREE(LUN,IER)
         IF(IER.NE.0) THEN
           IRET = -1
           GOTO 100
         ENDIF
         MBYT(LUN) = MBYT(LUN) + NBYT
      ELSEIF(MSGUNP(LUN).EQ.1) THEN
c  .... message with "standard" Section 3
         IBIT = MBYT(LUN)
         CALL RDTREE(LUN,IER)
         IF(IER.NE.0) THEN
           IRET = -1
           GOTO 100
         ENDIF
         MBYT(LUN) = IBIT
      ELSEIF(MSGUNP(LUN).EQ.2) THEN
c  .... compressed message
         CALL RDCMPS(LUN)
      ELSE
         GOTO 902
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: READSB - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: READSB - INPUT BUFR FILE IS OPEN FOR OUTPUT'//
     . ', IT MUST BE OPEN FOR INPUT')
902   WRITE(BORT_STR,'("BUFRLIB: READSB - MESSAGE UNPACK TYPE",I3,"IS'//
     . ' NOT RECOGNIZED")') MSGUNP
      CALL BORT(BORT_STR)
      END
