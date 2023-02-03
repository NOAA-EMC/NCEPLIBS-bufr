C> @file
C> @brief Encode a character string within an integer array.

C> This subroutine encodes a character string within a specified
C> number of bytes of an integer array, starting at the bit
C> immediately after a specified bit within the array.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] CHR     -- character*(*): String to be encoded
C> @param[in] NCHR    -- integer: Number of bytes of IBAY within
C>                       which to encode CHR (i.e. the number of
C>                       characters in CHR)
C> @param[out] IBAY   -- integer(*): Array containing encoded CHR
C> @param[in,out] IBIT -- integer: Bit pointer within IBAY
C>                        - On input, IBIT points to the bit within
C>                          IBAY after which to begin encoding CHR.
C>                        - On output, IBIT points to the last bit
C>                          of IBAY which contains the encoded CHR.
C>
C> @remarks
C> - This subroutine is the logical inverse of subroutine upc().
C> - On input, there is no requirement that IBIT must point to the first
C>   bit of a byte within IBAY.  Correspondingly, on output there is no
C>   guarantee that the NCHR characters of CHR will be aligned on byte
C>   boundaries when encoded within IBAY.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 2003-11-04 | J. Woollen | Modified to be endian-independent |
C> | 2003-11-04 | J. Ator    | Added documentation |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally; use bort2() instead of bort() |
C> | 2004-08-18 | J. Ator    | Modified to be compatible with writlc() |
C>
      SUBROUTINE PKC(CHR,NCHR,IBAY,IBIT)

      COMMON /CHARAC/ IASCII,IATOE(0:255),IETOA(0:255)
      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      CHARACTER*(*) CHR
      CHARACTER*1   CVAL(8)
      DIMENSION     IBAY(*),IVAL(2)
      EQUIVALENCE   (CVAL,IVAL)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      LB = IORD(NBYTW)

C     LB now points to the "low-order" (i.e. least significant) byte
C     within a machine word.

      IVAL(1) = 0
      NBIT = 8

      DO I=1,NCHR
      IF(I.LE.LEN(CHR)) THEN
         CVAL(LB) = CHR(I:I)
      ELSE
         CVAL(LB) = ' '
      ENDIF

C     If the machine is EBCDIC, then translate character CVAL(LB) from
C     EBCDIC to ASCII.

      IF(IASCII.EQ.0) CALL IPKM(CVAL(LB),1,IETOA(IUPM(CVAL(LB),8)))

      NWD  = IBIT/NBITW + 1
      NBT  = MOD(IBIT,NBITW)
      INT = ISHFT(IVAL(1),NBITW-NBIT)
      INT = ISHFT(INT,-NBT)
      MSK = ISHFT(  -1,NBITW-NBIT)
      MSK = ISHFT(MSK,-NBT)
      IBAY(NWD) = IREV(IOR(IAND(IREV(IBAY(NWD)),NOT(MSK)),INT))
      IF(NBT+NBIT.GT.NBITW) THEN

C        This character will not fit within the current word (i.e.
C        array member) of IBAY, because there are less than 8 bits of
C        space left.  Store as many bits as will fit within the current
C        word and then store the remaining bits within the next word.

         INT = ISHFT(IVAL(1),2*NBITW-(NBT+NBIT))
         MSK = ISHFT(  -1,2*NBITW-(NBT+NBIT))
         IBAY(NWD+1) = IREV(IOR(IAND(IREV(IBAY(NWD+1)),NOT(MSK)),INT))
      ENDIF
      IBIT = IBIT + NBIT
      ENDDO

C  EXITS
C  -----

      RETURN
      END
