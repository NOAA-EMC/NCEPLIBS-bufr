C> @file
C> @brief Read the next data subset from a BUFR message.
C>
C> @author J. Woollen @date 1994-01-06

C> Read the next data subset from a BUFR message.
C>
C> Logical unit LUNIT should have already been opened for
C> input operations via a previous call to subroutine openbf(), and a
C> BUFR message should have already been read into internal arrays via
C> a previous call to one of the
C> [message-reading subroutines](@ref hierarchy).
C>
C> Whenever this subroutine returns with IRET = 0, this indicates that a
C> new BUFR data subset (i.e. report) was successfully read into internal
C> arrays within the BUFRLIB software, and from where it can be
C> manipulated or further parsed via calls to any of the [values-reading
C> subroutines](@ref hierarchy).
C>
C> If the subroutine returns an IRET of -1 there are no more data
C> subsets available within the current message; a new call needs to
C> be made to one of the [message-reading subroutines](@ref
C> hierarchy) in order to read in the next message from logical unit
C> LUNIT.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file.
C> @param[out] IRET - integer: return code:
C> - 0 new BUFR data subset was successfully read into internal arrays.
C> - -1 there are no more BUFR data subsets in the BUFR message.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE READSB(LUNIT,IRET)

      USE MODA_MSGCWD
      USE MODA_UNPTYP
      USE MODA_BITBUF
      USE MODA_BITMAPS
      USE MODV_IM8B

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL READSB(MY_LUNIT,IRET)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

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
