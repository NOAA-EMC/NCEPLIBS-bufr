C> @file
C> @brief Update file status in library internals.
C>
C> @author J. Woollen @date 1994-01-06

C> Update file status in library internals.
C>
C> This subroutine can be used to connect or disconnect a specified
C> Fortran logical unit number to/from the NCEPLIBS-bufr software, and it
C> can also be used to set or reset the internal message status
C> associated with that logical unit number.
C>
C> @note Before this subroutine is called to connect any LUNIT to the
C> software, a previous call should have been made to subroutine
C> status() to confirm that internal space is available to connect
C> the associated file, as well as to obtain an LUN value to use
C> in connecting it. Once a file is connected, the corresponding
C> LUNIT and LUN values remain linked to each other for as
C> long as the file is connected to the software.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file
C> @param[in] LUN - integer: file ID associated with LUNIT
C> @param[in] IL - integer: File status update option
C>  - 0 Disconnect LUNIT from the software
C>  - 1 Connect LUNIT to the software for output operations
C>      (i.e. writing/encoding BUFR), if not already connected
C>  - -1 Connect LUNIT to the software for input operations
C>      (i.e. reading/decoding BUFR), if not already connected
C> @param[in] IM - integer: Message status update option, indicating
C> whether a message is currently open within the internal arrays for LUNIT
C>  - 0 No
C>  - 1 Yes
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE WTSTAT(LUNIT,LUN,IL,IM)

      use moda_stbfr

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK ON THE ARGUMENTS
C  ----------------------

      IF(LUNIT.LE.0)            GOTO 900
      IF(LUN  .LE.0)            GOTO 901
      IF(IL.LT.-1 .OR. IL.GT.1) GOTO 902
      IF(IM.LT. 0 .OR. IM.GT.1) GOTO 903

C  CHECK ON LUNIT-LUN COMBINATION
C  ------------------------------

      IF(ABS(IOLUN(LUN)).NE.LUNIT) THEN
         IF(IOLUN(LUN).NE.0) GOTO 905
      ENDIF

C  RESET THE FILE STATUSES
C  -----------------------

      IF(IL.NE.0) THEN
         IOLUN(LUN) = SIGN(LUNIT,IL)
         IOMSG(LUN) = IM
      ELSE
         IOLUN(LUN) = 0
         IOMSG(LUN) = 0
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID UNIT NUMBER PASSED '//
     . ' INTO FIRST ARGUMENT (INPUT) (=",I3,")")') LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID FILE ID '//
     . 'PASSED INTO SECOND ARGUMENT (INPUT) (=",I3,")")') LUN
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID LOGICAL UNIT STATUS'//
     . ' INDICATOR PASSED INTO THIRD ARGUMENT (INPUT) (=",I4,")")') IL
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID BUFR MESSAGE STATUS'//
     . ' INDICATOR PASSED INTO FOURTH ARGUMENT (INPUT) (=",I4,")")') IM
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - ATTEMPTING TO REDEFINE '//
     . 'EXISTING FILE UNIT (LOGICAL UNIT NUMBER ",I3,")")') IOLUN(LUN)
      CALL BORT(BORT_STR)
      END
