C> @file
C> @brief Determine important information about the local machine.
C>
C> @author J. Woollen @author J. Ator @date 1994-01-06

C> Determine important information about the local machine.
C>
C> This subroutine figures out some important information about the
C> local machine on which the NCEPLIBS-bufr software is being run,
C> including the native endianness and the number of bytes in an integer.
C>
C> This subroutine isn't normally called directly by any
C> application program, because it's automatically called internally
C> from within subroutine openbf() during the first time that subroutine
C> is called by any application program.  It's also called as needed
C> from within several other subroutines, but it always keeps track of
C> its results as well as whether it has already been called during
C> the life of an application program, and that way if it does end
C> up being called more than once, it will just quietly return without
C> having to recompute all of its results from the first call.
C>
C> @author J. Woollen @author J. Ator @date 1994-01-06

      SUBROUTINE WRDLEN

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)
      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8   CINT,DINT,CVSTR
      CHARACTER*6   CNDIAN
      EQUIVALENCE   (CINT,INT)
      EQUIVALENCE   (DINT,JNT)
      LOGICAL       PRINT

      DATA IFIRST/0/

      SAVE IFIRST

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     HAS THIS SUBROUTINE ALREADY BEEN CALLED?

      IF(IFIRST.EQ.0) THEN

C        NO, SO CHECK WHETHER DIAGNOSTIC INFORMATION SHOULD BE PRINTED
C        AND THEN PROCEED THROUGH THE REST OF THE SUBROUTINE.

         PRINT = IPRT.GE.1
         IFIRST = 1
      ELSE

C        YES, SO THERE IS NO NEED TO PROCEED ANY FURTHER.

         RETURN
      ENDIF

C  COUNT THE BITS IN A WORD - MAX 64 ALLOWED
C  -----------------------------------------

      INT = 1
      DO I=1,65
      INT = ISHFT(INT,1)
      IF(INT.EQ.0) GOTO 10
      ENDDO
   10 IF(I.GE.65)       GOTO 900
      IF(MOD(I,8).NE.0) GOTO 901

C  NBITW is no. of bits in a word, NBYTW is no. of bytes in a word
C  ---------------------------------------------------------------

      NBITW = I
      NBYTW = I/8

C  INDEX THE BYTE STORAGE ORDER -  HIGH BYTE TO LOW BYTE
C  -----------------------------------------------------

      JNT = 0

      DO I = 1,8
         IORD(I) = 9999
      ENDDO

      DO I=1,NBYTW
         INT = ISHFT(1,(NBYTW-I)*8)
         DO J=1,NBYTW
            IF(CINT(J:J).NE.DINT(J:J)) GOTO 20
         ENDDO
c  .... DK: Can the below ever happen since upper loop bounds is NBYTW?
   20    IF(J.GT.NBYTW) GOTO 902
         IORD(I) = J
      ENDDO

C  SHOW SOME RESULTS
C  -----------------

      IF(PRINT) THEN
         CALL BVERS(CVSTR)
#ifdef BIG_ENDIAN
         CNDIAN = '  BIG '
#else
         CNDIAN = 'LITTLE'
#endif
      ERRSTR = '=============== ' //
     . 'WELCOME TO THE BUFR ARCHIVE LIBRARY' // ' =============='
      CALL ERRWRT(ERRSTR)
      WRITE (  UNIT=ERRSTR, FMT='(A,I2)' )
     . ' MACHINE CHARACTERISTICS: NUMBER OF BYTES PER WORD =', NBYTW
      CALL ERRWRT(ERRSTR)
      WRITE (  UNIT=ERRSTR, FMT='(A,I3)' )
     . '                          NUMBER OF BITS PER WORD =', NBITW
      CALL ERRWRT(ERRSTR)
      ERRSTR = '                          BYTE ORDER IS ' // CNDIAN //
     .  ' ENDIAN'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(ERRSTR)
      ERRSTR = '====================== VERSION: ' // CVSTR //
     .  '=========================='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

      RETURN
  900 WRITE(BORT_STR,'("BUFRLIB: WRDLEN - MACHINE WORD LENGTH IS '//
     . 'LIMITED TO 64 BITS (THIS MACHINE APPARENTLY HAS",I4," BIT '//
     . 'WORDS!)")') I
      CALL BORT(BORT_STR)
  901 WRITE(BORT_STR,'("BUFRLIB: WRDLEN - MACHINE WORD LENGTH (",I4,"'//
     . ') IS NOT A MULTIPLE OF 8 (THIS MACHINE HAS WORDS NOT ON WHOLE'//
     . ' BYTE BOUNDARIES!)")') I
      CALL BORT(BORT_STR)
  902 WRITE(BORT_STR,'("BUFRLIB: WRDLEN - BYTE ORDER CHECKING MISTAKE'//
     . ', LOOP INDEX J (HERE =",I3,") IS .GT. NO. OF BYTES PER WORD '//
     . 'ON THIS MACHINE (",I3,")")') J,NBYTW
      CALL BORT(BORT_STR)
      END
