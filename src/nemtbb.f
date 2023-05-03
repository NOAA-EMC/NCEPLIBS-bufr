C> @file
C> @brief Get information about a Table B descriptor
C>
C> @author J. Woollen @date 1994-01-06

C> Get information about a Table B descriptor.
C>
C> This subroutine returns information about a Table B descriptor
C> from the internal DX BUFR tables.
C>
C> @param[in] LUN - integer: File ID.
C> @param[in] ITAB - integer: Positional index of descriptor within
C> internal Table B.
C> @param[out] UNIT - character*24: Units of descriptor.
C> @param[out] ISCL - integer: Scale factor of descriptor.
C> @param[out] IREF - integer: Reference value of descriptor.
C> @param[out] IBIT - integer: Bit width of descriptor.
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE NEMTBB(LUN,ITAB,UNIT,ISCL,IREF,IBIT)

      USE MODA_TABABD

      CHARACTER*128 BORT_STR
      CHARACTER*24  UNIT
      CHARACTER*8   NEMO
      REAL*8        MXR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      MXR = 1E11-1

      IF(ITAB.LE.0 .OR. ITAB.GT.NTBB(LUN)) GOTO 900

C  PULL OUT TABLE B INFORMATION
C  ----------------------------

      IDN  = IDNB(ITAB,LUN)
      NEMO = TABB(ITAB,LUN)( 7:14)
      UNIT = TABB(ITAB,LUN)(71:94)
      CALL STRNUM(TABB(ITAB,LUN)( 95: 98),ISCL,IERNS)
      CALL STRNUM(TABB(ITAB,LUN)( 99:109),IREF,IERNS)
      CALL STRNUM(TABB(ITAB,LUN)(110:112),IBIT,IERNS)

C  CHECK TABLE B CONTENTS
C  ----------------------

      IF(IDN.LT.IFXY('000000')) GOTO 901
      IF(IDN.GT.IFXY('063255')) GOTO 901

      IF(ISCL.LT.-999 .OR. ISCL.GT.999) GOTO 902
      IF(IREF.LE.-MXR .OR. IREF.GE.MXR) GOTO 903
      IF(IBIT.LE.0) GOTO 904
      IF(UNIT(1:5).NE.'CCITT' .AND. IBIT.GT.32      ) GOTO 904
      IF(UNIT(1:5).EQ.'CCITT' .AND. MOD(IBIT,8).NE.0) GOTO 905

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - ITAB (",I7,") NOT FOUND IN '//
     . 'TABLE B")') ITAB
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - INTEGER REPRESENTATION OF '//
     . 'DESCRIPTOR FOR TABLE B MNEMONIC ",A," (",I7,") IS OUTSIDE '//
     . 'RANGE 0-16383 (16383 -> 0-63-255)")') NEMO,IDN
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - SCALE VALUE FOR TABLE B '//
     .'MNEMONIC ",A," (",I7,") IS OUTSIDE RANGE -999 TO 999")')
     . NEMO,ISCL
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - REFERENCE VALUE FOR TABLE B'//
     .' MNEMONIC ",A," (",I7,") IS OUTSIDE RANGE +/- 1E11-1")')
     . NEMO,IREF
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - BIT WIDTH FOR NON-CHARACTER'//
     . ' TABLE B MNEMONIC ",A," (",I7,") IS > 32")') NEMO,IBIT
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: NEMTBB - BIT WIDTH FOR CHARACTER '//
     . 'TABLE B MNEMONIC ",A," (",I7,") IS NOT A MULTIPLE OF 8")')
     . NEMO,IBIT
      CALL BORT(BORT_STR)
      END
