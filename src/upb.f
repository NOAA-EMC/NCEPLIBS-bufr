C> @file
C> @brief Decode an integer value from an integer array.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine decodes an integer value from within a specified
C> number of bits of an integer array, starting at the bit
C> immediately after a specified bit within the array.
C>
C> <p>It is similar to subroutine upbb(), except that here IBIT is
C> both an input and an output argument, and the overall order
C> of the arguments is different.
C>
C> @param[in] IBAY    -- integer(*): Array containing encoded value
C> @param[in,out] IBIT -- integer: Bit pointer within IBAY
C>                        - On input, IBIT points to the bit within
C>                          IBAY after which to begin decoding NVAL.
C>                        - On output, IBIT points to the last bit
C>                          of IBAY which contained the decoded NVAL.
C> @param[in] NBITS   -- integer: Number of bits to be decoded
C> @param[out] NVAL   -- integer: Decoded value
C>
C> @remarks
C> - This subroutine is the logical inverse of subroutine pkb().
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE UPB(NVAL,NBITS,IBAY,IBIT)

      DIMENSION IBAY(*)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      CALL UPBB(NVAL,NBITS,IBIT,IBAY)

      IBIT = IBIT+NBITS

      RETURN
      END
