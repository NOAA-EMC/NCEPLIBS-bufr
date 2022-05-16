C> @file
C> @brief Encode an 8 byte integer value within an integer array.

C> This subroutine encodes an 8 byte integer value within a specified
C> number of bits of an integer array, starting at the bit
C> immediately after a specified bit within the array.
C>
C> @author J. Woollen
C> @date 2022-05-06
C>
C> @param[in] NVAL    -- 8 byte integer: Value to be encoded
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
C> - This subroutine will not work properly if NBITS is <0 or >64.   
C> - an internal call to subroutine wrdlen().  
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2022-05-06 | J. Woollen | Original author |
C>
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      subroutine pkb8(nval,nbits,ibay,ibit)

      common /hrdwrd/ nbytw,nbitw,iord(8)

      integer(8) :: nval
      integer(4) :: nbits,ibit,ibay(*)

      integer(8) :: nval8
      integer(4) :: nval4
      integer(4) :: nvals(2)

      equivalence (nval8,nvals)

      if(nbits<0 ) call bort('bufrlib: pkb8 - nbits < zero !!!!!')
      if(nbits>64) call bort('bufrlib: pkb8 - nbits > 64   !!!!!')

      if(nbitw==32) then
         nval8=nval
         nval4=nvals(2); call pkb(nval4,max(nbits-nbitw,0),ibay,ibit)
         nval4=nvals(1); call pkb(nval4,min(nbits,nbitw  ),ibay,ibit)
      else
         call pkb(nval,nbits,ibay,ibit)
      endif

      end subroutine


