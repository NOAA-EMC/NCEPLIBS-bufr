C> @file
C> @brief Confirm that a mnemonic and FXY value haven't already been defined.
C>
C> @author J. Woollen @date 1994-01-06

C> Check a mnemonic and FXY value pair that were read
C> from a user-supplied BUFR DX dictionary table in character format,
C> in order to confirm that neither value has already been
C> defined within internal BUFR table B or D (in module @ref moda_tababd) for
C> the given LUN. If either value has already been defined for this
C> LUN, then an appropriate call is made to subroutine bort().
C>
C> @param[in] NEMO - character*8: Mnemonic.
C> @param[in] NUMB - character*6: FXY value associated with NEMO.
C> @param[in] LUN - integer: File ID.
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE NENUBD(NEMO,NUMB,LUN)

      use moda_tababd

      CHARACTER*128 BORT_STR
      CHARACTER*8   NEMO
      CHARACTER*6   NUMB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  CHECK TABLE B AND D
C  -------------------

      DO N=1,NTBB(LUN)
      IF(NUMB.EQ.TABB(N,LUN)(1: 6)) GOTO 900
      IF(NEMO.EQ.TABB(N,LUN)(7:14)) GOTO 901
      ENDDO

      DO N=1,NTBD(LUN)
      IF(NUMB.EQ.TABD(N,LUN)(1: 6)) GOTO 902
      IF(NEMO.EQ.TABD(N,LUN)(7:14)) GOTO 903
      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE B FXY VALUE (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE B MNEMONIC (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE D FXY VALUE (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE D MNEMONIC (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO
      CALL BORT(BORT_STR)
      END
