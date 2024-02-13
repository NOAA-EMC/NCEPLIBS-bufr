C> @file
C> @brief Get the event program code associated with a Table D mnemonic
C> from an NCEP prepbufr file
C>
C> @author J. Woollen @date 1994-01-06

C> Given a mnemonic associated with a category 63 Table D
C> descriptor from an NCEP prepbufr file, return the
C> corresponding event program code.
C>
C> The event program code is equivalent to the Y value of the
C> category 63 (i.e. X=63) Table D descriptor.  Knowledge of this value
C> is especially useful for application programs which are writing data
C> events to NCEP prepbufr files.
C>
C> Logical unit LUNIT should have already been opened via a previous
C> call to subroutine openbf().
C>
C> This subroutine is the logical inverse of subroutine ufbqcp().
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                     NCEP prepbufr file
C> @param[in] NEMO  -- character*(*): Mnemonic associated with a
C>                     category 63 (i.e. X=63) Table D descriptor
C> @param[out] IQCD -- integer: Y value of descriptor associated
C>                     with NEMO
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBQCD(LUNIT,NEMO,IQCD)

      use modv_vars, only: im8b

      CHARACTER*(*) NEMO
      CHARACTER*128 BORT_STR
      CHARACTER*6   FXY,ADN30
      CHARACTER*1   TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL UFBQCD(MY_LUNIT,NEMO,IQCD)
         CALL X48(IQCD,IQCD,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

      CALL NEMTAB(LUN,NEMO,IDN,TAB,IRET)
      IF(TAB.NE.'D') GOTO 901

      FXY = ADN30(IDN,6)
      IF(FXY(2:3).NE.'63') GOTO 902
      READ(FXY(4:6),'(I3)') IQCD

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: UFBQCD - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   WRITE(BORT_STR,'("BUFRLIB: UFBQCD - INPUT MNEMONIC ",A," NOT '//
     . 'DEFINED AS A SEQUENCE DESCRIPTOR IN BUFR TABLE")') NEMO
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: UFBQCD - BUFR TABLE SEQ. DESCRIPTOR '//
     . 'ASSOC. WITH INPUT MNEMONIC ",A," HAS INVALID CATEGORY ",A," -'//
     . ' CATEGORY MUST BE 63")') NEMO,FXY(2:3)
      CALL BORT(BORT_STR)
      END
