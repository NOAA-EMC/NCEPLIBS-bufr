C> @file
C> @brief Read the next data subset from a BUFR message.
      
C> This subroutine reads the next data subset from a BUFR
C> message into internal arrays.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT    - integer: Fortran logical unit number for BUFR file
C> @param[out] IRET    - integer: return code
C>                           - 0 = new BUFR data subset was successfully
C>                                 read into internal arrays
C>                           - -1 = there are no more BUFR data subsets in
C>                                 the BUFR message
C>
C> <p>Logical unit LUNIT should have already been opened for
C> input operations via a previous call to subroutine openbf(), and a
C> BUFR message should have already been read into internal arrays via
C> a previous call to subroutine readmg(), readerme() or equivalent. 
C>
C> <p>Whenever this subroutine returns with IRET = 0, this indicates
C> that a new BUFR data subset (i.e. report) was successfully read into
C> internal arrays within the BUFRLIB software, and from where it can
C> then be easily manipulated or further parsed via calls to subroutines
C> ufbint(), ufbrep(), ufbseq(), etc.  Otherwise, if the subroutine
C> returns with IRET = -1, then this indicates that there are no more
C> data subsets available within the current message, and therefore that
C> a new call needs to be made to subroutine readmg(), readerme() or
C> equivalent in order to read in the next message from logical unit
C> LUNIT.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine
C>                           "ABORT" with call to new internal BUFRLIB
C>                           routine "BORT"
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                           opened at one time increased from 10 to 32
C>                           (necessary in order to process multiple
C>                           BUFR files under the MPI)
C> - 2000-09-19  J. Woollen -- Added call to new routine rdcmps() allowing
C>                           subsets to also be decoded from compressed
C>                           BUFR messages; maximum length increased
C>                           from 10,000 to 20,000 bytes
C> - 2002-05-14  J. Woollen -- Corrected error relating to certain
C>                          foreign file types; removed old Cray
C>                          compiler directives
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                           documentation; outputs more complete
C>                           diagnostic info when routine terminates
C>                           abnormally
C> - 2004-08-09  J. Ator    -- Maximum message length increased from
C>                           20,000 to 50,000 bytes
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
C> <b>This routine calls:</b>  bort()   rdcmps() rdtree()  status()
C>                             upb()
C>
C> <b>This routine is called by:</b>
C>                     copysb()   ireadsb()  rdmems()   readns()
C>                     ufbinx()   ufbpos()
C>                     <br>Also called by application programs.
C>
      SUBROUTINE READSB(LUNIT,IRET)

      USE MODA_MSGCWD
      USE MODA_UNPTYP
      USE MODA_BITBUF
      USE MODA_BITMAPS

      INCLUDE 'bufrlib.inc'

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
