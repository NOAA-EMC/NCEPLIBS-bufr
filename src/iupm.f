C> @file
C> @brief Decode an integer value from a character string.
C>
C> @author J. Woollen @date 1994-01-06

C> Decode an integer value from a character string.
C>
C> This function decodes an integer value from within a
C> specified number of bits of a character string, starting
C> with the first bit of the first byte of the string.
C>
C> This function is the logical inverse of subroutine ipkm().
C>
C> @param[in] CBAY  - character*(*): String.
C> @param[in] NBITS - integer: Number of bits from CBAY to be decoded.
C> @returns iupm - integer: Decoded value.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE FUNCTION IUPM(CBAY,NBITS) RESULT(IRET)

      USE MODV_IM8B

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      CHARACTER*128 BORT_STR
      CHARACTER*8   CBAY
      CHARACTER*8   CINT
      DIMENSION     INT(2)
      EQUIVALENCE   (CINT,INT)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(NBITS,MY_NBITS,1)
         IRET = IUPM(CBAY,MY_NBITS)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = 0
      IF(NBITS.GT.NBITW) GOTO 900
      CINT = CBAY
      INT(1) = IREV(INT(1))
      IRET = ISHFT(INT(1),NBITS-NBITW)

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: IUPM - NUMBER OF BITS BEING UNPACKED'//
     . ', NBITS (",I4,"), IS > THE INTEGER WORD LENGTH ON THIS '//
     . 'MACHINE, NBITW (",I3,")")') NBITS,NBITW
      CALL BORT(BORT_STR)
      END
