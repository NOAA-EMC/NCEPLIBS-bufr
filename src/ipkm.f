C> @file
C> @brief Encode an integer value within a character string.

C> This subroutine encodes an integer value within a specified
C> number of bytes of a character string, up to a maximum of 8
C> bytes.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] N       - integer: Value to be encoded
C> @param[in] NBYT    - integer: Number of bytes of CBAY (up to a
C>                      maximum of 8) within which to encode N
C> @param[out] CBAY   - character*(*): String of length NBYT bytes
C>                      containing encoded integer N
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 2003-11-04  J. Woollen -- Modified to be endian-independent
C> - 2003-11-04  J. Ator    -- Added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation; outputs more complete
C>                             diagnostic info when routine terminates
C>                             abnormally
C>
      SUBROUTINE IPKM(CBAY,NBYT,N)

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      CHARACTER*128 BORT_STR
      CHARACTER*8   CBAY,CINT
      EQUIVALENCE   (CINT,INT)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

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
