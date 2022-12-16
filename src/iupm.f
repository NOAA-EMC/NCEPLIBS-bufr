C> @file
C> @brief Decode an integer value from a character string.

C> This function decodes an integer value from within a
C> specified number of bits of a character string, starting
C> with the first bit of the first byte of the string.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] CBAY   -- character*(*): String
C> @param[in] NBITS  -- integer: Number of bits from CBAY to be decoded
C> @returns iupm     -- integer: Decoded value
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 2003-11-04 | J. Woollen | Modified to be endian-independent |
C> | 2003-11-04 | J. Ator    | Added documentation |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

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
