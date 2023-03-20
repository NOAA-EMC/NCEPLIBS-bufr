C> @file
C> @brief Decode an 8-byte integer value from an integer array.
C>
C> @author J. Woollen @date 2022-05-06

C> This subroutine decodes an 8-byte integer value from within a
C> specified number of bits of an integer array, starting at the bit
C> immediately after a specified bit within the array.
C>
C> It is similar to subroutine up8(), except that here IBIT is
C> only an input argument, and the overall order of the arguments
C> is different.
C>
C> This subroutine will not work properly if NBITS is less than 0 or
C> greater than 64, as determined via an internal call to subroutine
C> wrdlen().
C>
C> @param[in] IBAY - integer(*): Array containing encoded value.
C> @param[in] IBIT - integer: Bit within IBAY after which to begin
C> decoding NVAL.
C> @param[in] NBITS - integer: Number of bits to be decoded.
C> @param[out] NVAL - integer*8: Decoded value.
C>
C> @author J. Woollen @date 2022-05-06
      subroutine upb8(nval,nbits,ibit,ibay)

      common /hrdwrd/ nbytw,nbitw,iord(8)

      integer(8) :: nval
      integer(4) :: nbits,ibit,ibay(*)

      integer(4) :: nvals(2)
      integer(8) :: nval8
      equivalence (nval8,nvals)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      if(nbits<0 ) call bort('BUFRLIB: UPB8 - nbits < zero !!!!!')
      if(nbits>64) nval=0
      if(nbits>64) return

      if(nbitw==32) then
         jbit=ibit; nvals=0
         call upb(nvals(2),max(nbits-nbitw,0),ibay,jbit)
         call upb(nvals(1),min(nbitw,nbits  ),ibay,jbit)
         nval=nval8
      else
         call upbb(nval,nbits,ibit,ibay)
      endif

      end subroutine
