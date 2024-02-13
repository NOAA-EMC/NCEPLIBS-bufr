C> @file
C> @brief Decode a character string from an integer array.
C>
C> @author J. Woollen @date 1994-01-06

C> Decode a character string from within a specified
C> number of bytes of an integer array, starting at the bit immediately
C> after a specified bit within the array.
C>
C> @remarks
C> - This subroutine is the logical inverse of subroutine pkc().
C> - On input, there is no requirement that IBIT must point to the first
C>   bit of a byte within IBAY.  In other words, the NCHR characters to
C>   be decoded do not necessarily need to be aligned on byte boundaries
C>   within IBAY.
C>
C> @param[out] CHR - character*(*): Decoded string.
C> @param[in] NCHR - integer: Number of bytes of IBAY from within
C> which to decode CHR (i.e. the number of characters in CHR).
C> @param[in] IBAY - integer(*): Array from which to decode CHR.
C> @param[inout] IBIT - integer: Bit pointer within IBAY
C> - On input, IBIT points to the bit within IBAY after which to begin
C> decoding CHR.
C> - On output, IBIT points to the last bit of IBAY which was decoded.
C> @param[in] CNVNULL - logical: .true. if null characters in IBAY
C> should be converted to blanks within CHR; .false. otherwise
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE UPC(CHR,NCHR,IBAY,IBIT,CNVNULL)

      use modv_vars, only: nbytw, iordle, iordbe

      CHARACTER*(*) CHR
      CHARACTER*8   CVAL
      DIMENSION     IBAY(*),IVAL(2)
      EQUIVALENCE   (CVAL,IVAL)

      LOGICAL CNVNULL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Set LB to point to the "low-order" (i.e. least significant) byte
C     within a machine word.

#ifdef BIG_ENDIAN
      LB = IORDBE(NBYTW)
#else
      LB = IORDLE(NBYTW)
#endif

      CVAL = ' '

      NUMCHR = MIN(NCHR,LEN(CHR))
      DO I=1,NUMCHR
        CALL UPB(IVAL(1),8,IBAY,IBIT)
        IF((IVAL(1).EQ.0).AND.(CNVNULL)) THEN
          CHR(I:I) = ' '
        ELSE
          CHR(I:I) = CVAL(LB:LB)
        ENDIF
      ENDDO

      RETURN
      END
