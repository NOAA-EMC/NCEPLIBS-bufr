C> @file
C> @brief Encode a character string within an integer array.
C>
C> @author J. Woollen @date 1994-01-06

C> Encode a character string within a specified
C> number of bytes of an integer array, starting at the bit
C> immediately after a specified bit within the array.
C>
C> @remarks
C> - This subroutine is the logical inverse of subroutine upc().
C> - On input, there is no requirement that IBIT must point to the first
C>   bit of a byte within IBAY.  Correspondingly, on output there is no
C>   guarantee that the NCHR characters of CHR will be aligned on byte
C>   boundaries when encoded within IBAY.
C>
C> @param[in] CHR - character*(*): String to be encoded.
C> @param[in] NCHR - integer: Number of bytes of IBAY within
C> which to encode CHR (i.e. the number of characters in CHR).
C> @param[out] IBAY - integer(*): Array containing encoded CHR.
C> @param[in,out] IBIT - integer: Bit pointer within IBAY
C>  - On input, IBIT points to the bit within IBAY after which
C> to begin encoding CHR.
C>  - On output, IBIT points to the last bit of IBAY which
C> contains the encoded CHR.
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE PKC(CHR,NCHR,IBAY,IBIT)

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
