C> @file
C> @brief Update the status of a system file with respect to the
C> BUFRLIB software.

C> This subroutine can be used to connect or disconnect a specified
C> Fortran logical unit number to/from the BUFRLIB software, and it
C> can also be used to set or reset the internal message status
C> associated with that logical unit number.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in]  LUNIT    - integer: Fortran logical unit number for
C>                        BUFR file
C> @param[in]  LUN      - integer: Internal I/O stream index associated
C>                        with LUNIT
C> @param[in]  IL       - integer: File status update option
C>                        - 0 = Disconnect LUNIT from the software
C>                        - 1 = Connect LUNIT to the software for
C>                              output operations
C>                              (i.e. writing/encoding BUFR),
C>                              if not already connected
C>                        - -1 = Connect LUNIT to the software for
C>                               input operations
C>                               (i.e. reading/decoding BUFR),
C>                               if not already connected
C> @param[in]  IM       - integer: Message status update option, to
C>                        indicate whether a message is now open
C>                        within the internal arrays for LUNIT
C>                        - 0 = No
C>                        - 1 = Yes
C>
C> <p>Before this subroutine is called to connect any LUNIT to the
C> software, a previous call should have been made to subroutine
C> status() to confirm that internal space is available to connect
C> the associated file, as well as to obtain an LUN value to use
C> in connecting it.  Once a file is connected, the corresponding
C> LUNIT and LUN values remain linked to each other for as
C> long as the file is connected to the software.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to 32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2003-11-04  J. Ator    -- Corrected a typo in test for IM validity;
C>                             added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation; outputs more complete
C>                             diagnostic info when routine terminates
C>                             abnormally
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
      SUBROUTINE WTSTAT(LUNIT,LUN,IL,IM)

      USE MODA_STBFR

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
901   WRITE(BORT_STR,'("BUFRLIB: WTSTAT - INVALID I/O STREAM INDEX '//
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
