C> @file
C> @brief Decode an integer value from an integer array.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine decodes an integer value from within a specified
C> number of bits of an integer array, starting at the bit
C> immediately after a specified bit within the array.
C>
C> It is similar to subroutine upb(), except that here IBIT is
C> only an input argument, and the overall order of the arguments
C> is different.
C>
C> @param[in] IBAY  -- integer(*): Array containing encoded value
C> @param[in] IBIT  -- integer: Bit within IBAY after which to begin
C>                     decoding NVAL
C> @param[in] NBITS -- integer: Number of bits to be decoded
C> @param[out] NVAL -- integer: Decoded value
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE UPBB(NVAL,NBITS,IBIT,IBAY)

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

      DIMENSION IBAY(*)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  IF NBITS=0, THEN JUST SET NVAL=0 AND RETURN
C  -------------------------------------------

      IF(NBITS.EQ.0)THEN
        NVAL=0
        GOTO 100
      ENDIF

      NWD = IBIT/NBITW + 1
      NBT = MOD(IBIT,NBITW)
      INT = ISHFT(IREV(IBAY(NWD)),NBT)
      INT = ISHFT(INT,NBITS-NBITW)
      LBT = NBT+NBITS
      IF(LBT.GT.NBITW) THEN
         JNT = IREV(IBAY(NWD+1))
         INT = IOR(INT,ISHFT(JNT,LBT-2*NBITW))
      ENDIF
      NVAL = INT

C  EXIT
C  ----

100   RETURN
      END
