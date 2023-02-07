C> @file
C> @brief Decode a character string from an integer array.
C>
C> ### Program History Log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 1994-01-06 | J. Woollen | original author
C> 2003-11-04 | J. Ator    | added documentation
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | unified/portable for wrf; added history documentation
C> 2009-03-23 | J. Ator    | treat null characters as blanks; prevent overflow of chr
C> 2014-11-19 | J. Ator    | add cnvnull argument
C>
C> @author Woollen @date 1994-01-06

C> This subroutine decodes a character string from within a specified
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
C> @param[out] CHR - character*(*): Decoded string
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
C> @author Woollen @date 1994-01-06
      SUBROUTINE UPC(CHR,NCHR,IBAY,IBIT,CNVNULL)

      COMMON /CHARAC/ IASCII,IATOE(0:255),IETOA(0:255)
      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      CHARACTER*(*) CHR
      CHARACTER*8   CVAL
      DIMENSION     IBAY(*),IVAL(2)
      EQUIVALENCE   (CVAL,IVAL)

      LOGICAL CNVNULL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      LB = IORD(NBYTW)
      CVAL = ' '

      NUMCHR = MIN(NCHR,LEN(CHR))
      DO I=1,NUMCHR
        CALL UPB(IVAL(1),8,IBAY,IBIT)
        IF((IVAL(1).EQ.0).AND.(CNVNULL)) THEN
          CHR(I:I) = ' '
        ELSE
          CHR(I:I) = CVAL(LB:LB)
        ENDIF
        IF(IASCII.EQ.0) CALL IPKM(CHR(I:I),1,IATOE(IUPM(CHR(I:I),8)))
      ENDDO

      RETURN
      END
