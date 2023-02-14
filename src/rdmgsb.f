C> @file
C> @brief Read a specified data subset from a BUFR file
C>
C> @author J. Woollen @date 2003-11-04

C> This subroutine provides a handy way to combine the functionality
C> of subroutines openbf(), readmg(), and readsb() within a single
C> subroutine call.
C>
C> @author J. Woollen @date 2003-11-04
C>
C> @author J. Woollen @date 2003-11-04
C>
C> @param[in] LUNIT  -- integer: Fortran logical unit number for
C>                      BUFR file
C> @param[in] IMSG   -- integer: Number of BUFR message to be
C>                      read from the BUFR file, counting from the
C>                      beginning of the file, but <b>not</b>
C>                      counting any DX BUFR table messages which
C>                      may be present in the file
C> @param[in] ISUB   -- integer: Number of data subset to be
C>                      read from the (IMSG)th BUFR message,
C>                      counting from the beginning of the message
C>
C> <p>Logical unit LUNIT should not have already been opened via a
C> previous call to subroutine openbf()
C>
C> <p>Whenever this subroutine returns successfully, this indicates
C> that a new data subset was successfully read into internal arrays
C> within the BUFRLIB software, and that subsequent calls can
C> immediately be made to any of the various
C> [values-reading subroutines](@ref hierarchy).
C>
C> <p>Note that the value specified for IMSG should not include any
C> DX BUFR table messages which may be present in the file.
C> In other words, a value of 12 for IMSG means to read the 12th
C> message which contains actual report data.
C>
C> @author J. Woollen @date 2003-11-04
      RECURSIVE SUBROUTINE RDMGSB(LUNIT,IMSG,ISUB)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODV_IM8B

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------
      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(IMSG,MY_IMSG,1)
         CALL X84(ISUB,MY_ISUB,1)
         CALL RDMGSB(MY_LUNIT,MY_IMSG,MY_ISUB)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  OPEN THE FILE AND SKIP TO MESSAGE # IMSG
C  ----------------------------------------

      CALL OPENBF(LUNIT,'IN',LUNIT)
      CALL STATUS(LUNIT,LUN,IL,IM)

C     Note that we need to use subroutine READMG to actually read in all
C     of the messages (including the first (IMSG-1) messages!), just in
C     case there are any embedded dictionary messages in the file.

      DO I=1,IMSG
        CALL READMG(LUNIT,SUBSET,JDATE,IRET)
        IF(IRET.LT.0) GOTO 901
      ENDDO

C  POSITION AT SUBSET # ISUB
C  -------------------------

      DO I=1,ISUB
        CALL READSB(LUNIT,IRET)
        IF(IRET.LT.0) GOTO 902
      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: RDMGSB - ERROR READING MESSAGE '//
     . '(RECORD) NUMBER",I5," IN INPUT BUFR FILE CONNECTED TO UNIT",'//
     . 'I4)')  I,LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: RDMGSB - HIT END OF FILE BEFORE '//
     . 'READING REQUESTED MESSAGE NO.",I5," IN BUFR FILE CONNECTED TO'//
     . ' UNIT",I4)')  IMSG,LUNIT
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: RDMGSB - ALL SUBSETS READ BEFORE '//
     . 'READING REQ. SUBSET NO.",I3," IN REQ. MSG NO.",I5," IN BUFR '//
     . 'FILE CONNECTED TO UNIT",I4)') ISUB,IMSG,LUNIT
      CALL BORT(BORT_STR)
      END
