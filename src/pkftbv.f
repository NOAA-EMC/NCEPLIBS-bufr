C> @file
C> @brief Determine the numerical data value equivalent to the
C> setting of a specified bit within a flag table.

C> This function computes the numerical value equivalent to the
C> setting of bit #IBIT within a flag table of NBITS bits.
C>
C> <p>If the computation fails for any reason, then the function
C> returns the current placeholder value for "missing" data.
C>
C> @author J. Ator
C> @date 2005-11-29
C>
C> @param[in] NBITS  - integer: Total number of bits in flag table
C> @param[in] IBIT   - integer: Number of bit to be set
C>
C> @returns pkftbv   - real*8: Value equivalent to the setting of
C>                     bit #IBIT within a flag table of NBITS bits
C>
C> @remarks
C> - This function is the logical inverse of subroutine upftbv().
C> - According to the WMO standard, bits within a bit field are
C> numbered from left to right, so bit #1 is always the high-order
C> (i.e. most significant) bit in any bit field.
C>
C> <b>Program history log:</b>
C> - 2005-11-29  J. Ator    -- Original version
C>
      REAL*8 FUNCTION PKFTBV(NBITS,IBIT)

      USE MODV_BMISS

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF((NBITS.LE.0).OR.(IBIT.LE.0).OR.(IBIT.GT.NBITS)) THEN
          PKFTBV = BMISS
      ELSE
          PKFTBV = (2.)**(NBITS-IBIT)
      ENDIF

      RETURN
      END
