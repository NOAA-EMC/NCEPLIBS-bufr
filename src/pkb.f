C> @file
C> @brief Encode an integer value within an integer array.
C>
C> @author J. Woollen @date 1994-01-06

C> Encode an integer value within a specified
C> number of bits of an integer array, starting at the bit
C> immediately after a specified bit within the array.
C>
C> @param[in] NVAL    -- integer: Value to be encoded
C> @param[in] NBITS   -- integer: Number of bits of IBAY within
C>                       which to encode NVAL
C> @param[out] IBAY   -- integer(*): Array containing encoded NVAL
C> @param[in,out] IBIT -- integer: Bit pointer within IBAY
C>                        - On input, IBIT points to the bit within
C>                          IBAY after which to begin encoding NVAL.
C>                        - On output, IBIT points to the last bit
C>                          of IBAY which contains the encoded NVAL.
C>
C> @remarks
C> - This subroutine is the logical inverse of subroutine upb().
C> - This subroutine will not work properly if NBITS is greater than 32.
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE PKB(NVAL,NBITS,IBAY,IBIT)

      use modv_vars, only: nbitw

      DIMENSION IBAY(*)

      CHARACTER*156 BORT_STR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(NBITS.GT.NBITW) GOTO 900

      NWD  = IBIT/NBITW + 1
      NBT  = MOD(IBIT,NBITW)
      IVAL = NVAL
      IF(ISHFT(IVAL,-NBITS).GT.0) IVAL = -1
      INT = ISHFT(IVAL,NBITW-NBITS)
      INT = ISHFT(INT,-NBT)
      MSK = ISHFT(  -1,NBITW-NBITS)
      MSK = ISHFT(MSK,-NBT)
      IBAY(NWD) = IREV(IOR(IAND(IREV(IBAY(NWD)),NOT(MSK)),INT))
      IF(NBT+NBITS.GT.NBITW) THEN

C        There are less than NBITS bits remaining within the current
C        word (i.e. array member) of IBAY, so store as many bits as
C        will fit within the current word and then store the remaining
C        bits within the next word.

         INT = ISHFT(IVAL,2*NBITW-(NBT+NBITS))
         MSK = ISHFT(  -1,2*NBITW-(NBT+NBITS))
         IBAY(NWD+1) = IREV(IOR(IAND(IREV(IBAY(NWD+1)),NOT(MSK)),INT))
      ENDIF

      IBIT = IBIT + NBITS

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: PKB - NUMBER OF BITS BEING PACKED '//
     . ', NBITS (",I4,"), IS > THE INTEGER WORD LENGTH ON THIS '//
     . 'MACHINE, NBITW (",I3,")")')
     . NBITS,NBITW
      CALL BORT(BORT_STR)
      END
