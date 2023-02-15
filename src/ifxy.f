C> @file
C> @brief Convert an FXY value from its six character representation
C> to its bit-wise (integer) representation
C>
C> @author J. Woollen @date 1994-01-06

C> This function converts an FXY value from its 6 character
C> representation to its bit-wise (integer) representation.
C>
C> @param[in] ADSC -- character*6: FXY value
C> @returns ifxy -- integer: Bit-wise representation of FXY value
C>
C> @remarks
C> Per the [official WMO BUFR regulations](@ref manual), an FXY value
C> can be represented as a bit-wise integer in 16 bits, ordered from
C> left (most significant) to right (least significant), and where the
C> F value occupies the first 2 bits, the X value occupies the next 6
C> bits, and the Y value occupies the last 8 bits.
C>
C>     For example, if ADSC = '063022'
C>
C>        F |     X     |      Y
C>        0 |     63    |      22
C>       0 0 1 1 1 1 1 1 0 0 0 1 0 1 1 0
C>
C>     then the corresponding ifxy
C>
C>       = ( 2**13 + 2**12 + 2**11 + 2**10 + 2**9 + 2**8 +
C>           2**4 + 2**2 + 2**1 )
C>
C>       = 16150
C>
C> @author J. Woollen @date 1994-01-06
      FUNCTION IFXY(ADSC)

      CHARACTER*6 ADSC

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      READ(ADSC,'(I1,I2,I3)') IF,IX,IY
      IFXY = IF*2**14 + IX*2**8 + IY
      RETURN
      END
