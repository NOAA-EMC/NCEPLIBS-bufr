C> @file
C> @brief Decode an 8-byte integer value from an integer array.
C>
C> @author J. Woollen @date 2022-05-06

C> This subroutine decodes an 8-byte integer value from within a
C> specified number of bits of an integer array, starting at the bit
C> immediately after a specified bit within the array.
C>
C> It is similar to subroutine upb8(), except that here IBIT is
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
C> @param[out] NVAL   -- integer*8: Decoded value
C>
C> @remarks
C> - This subroutine is the logical inverse of subroutine pkb8().
C>
C> @author J. Woollen @date 2022-05-06
      subroutine up8(nval,nbits,ibay,ibit)

      common /hrdwrd/ nbytw,nbitw,iord(8)

      integer(8) :: nval
      integer(4) :: nbits,ibit,ibay(*)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      call upb8(nval,nbits,ibit,ibay)
      ibit = ibit+nbits

      end subroutine
