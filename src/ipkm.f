C> @file
C> @brief Encode an integer value within a character string.
C>
C> @author J. Woollen @date 1994-01-06

C> Encode an integer value within a specified
C> number of bytes of a character string, up to a maximum of 4
C> bytes.
C>
C> This subroutine is the logical inverse of subroutine iupm().
C>
C> @param[in] N - integer: Value to be encoded.
C> @param[in] NBYT - integer: Number of bytes of CBAY (up to a
C> maximum of 4) within which to encode N.
C> @param[out] CBAY - character*(*): String of length NBYT bytes
C> containing encoded integer N.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE IPKM(CBAY,NBYT,N)

      use modv_vars, only: im8b, nbytw

      CHARACTER*128 BORT_STR
      CHARACTER*8   CBAY,CINT
      EQUIVALENCE   (CINT,INT)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
          IM8B=.FALSE.

          CALL X84(N,MY_N,1)
          CALL X84(NBYT,MY_NBYT,1)
          CALL IPKM(CBAY,MY_NBYT,MY_N)

          IM8B=.TRUE.
          RETURN
      ENDIF

      IF(NBYT.GT.NBYTW) GOTO 900

C     Note that the widths of input variable N and local variable INT
C     will both be equal to the default size of an integer (= NBYTW),
C     since they aren't specifically declared otherwise.

      INT = IREV(ISHFT(N,(NBYTW-NBYT)*8))
      DO I=1,NBYT
          CBAY(I:I) = CINT(I:I)
      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: IPKM - NUMBER OF BYTES BEING PACKED '//
     . ', NBYT (",I4,"), IS > THE INTEGER WORD LENGTH ON THIS '//
     . 'MACHINE, NBYTW (",I3,")")') NBYT,NBYTW
      CALL BORT(BORT_STR)
      END
