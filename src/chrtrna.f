C> @file
C> @brief Copy a specified number of characters from an array into
C> a string.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 2003-11-04 | J. Ator    | Added documentation.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine copies a specified number of characters from an
C> array of characters into a character string.
C>
C> <p>The characters in the input array CHR are assumed to be ASCII,
C> so for cases where the native machine is EBCDIC, an
C> ASCII-to-EBCDIC translation is done on the final character string
C> before it is output as STR.
C>
C> @remarks
C> - The determination as to whether the native machine is ASCII or
C> EBCDIC is made via an internal call to subroutine wrdlen().
C>
C>
C> @param[in] CHR - character(*): Array of characters in ASCII.
C> @param[in] N - integer: Number of characters to be copied
C> from CHR, starting from the beginning of the array.
C> @param[out] STR- character*(*): Character string in ASCII or
C> EBCDIC, depending on native machine.
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE CHRTRNA(STR,CHR,N)

      COMMON /CHARAC/ IASCII,IATOE(0:255),IETOA(0:255)

      CHARACTER*(*) STR
      CHARACTER*1   CHR(N)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Loop on N characters of CHR

      DO I=1,N
      STR(I:I) = CHR(I)

C     If this is an EBCDIC machine, then translate the character
C     from ASCII -> EBCDIC.

      IF(IASCII.EQ.0) CALL IPKM(STR(I:I),1,IATOE(IUPM(STR(I:I),8)))
      ENDDO
      RETURN
      END
