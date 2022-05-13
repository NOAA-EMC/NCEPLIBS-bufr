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

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      subroutine up8(nval,nbits,ibay,ibit)

      common /hrdwrd/ nbytw,nbitw,iord(8)

      integer(8) :: nval
      integer(4) :: nbits,ibit,ibay(*)

      call upb8(nval,nbits,ibit,ibay)
      ibit = ibit+nbits

      end subroutine

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      subroutine upb8(nval8,nbits,ibit,ibay)

      common /hrdwrd/ nbytw,nbitw,iord(8)

      integer(8) :: nval8
      integer(4) :: nbits,ibit,ibay(*)

      integer(4) :: nval1
      integer(4) :: nval2
      integer(8) :: nval3

      if(nbits<0 ) call bort('BUFRLIB: UPB8 - nbits < zero !!!!!') 
      if(nbits>64) nval=0
      if(nbits>64) return

      if(nbitw==32) then
         jbit=ibit
         call upb(nval1,max(nbits-nbitw,0),ibay,jbit)
         call upb(nval2,min(nbitw,nbits  ),ibay,jbit)
         nval3 = nval2
         nval8 = nval1
         nval8 = ior(nval8,nval3)
      else
         call upbb(nval8,nbits,ibit,ibay)
      endif

      end subroutine 

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      subroutine upb8p(nval8,nbits,ibit,ibay)

      common /hrdwrd/ nbytw,nbitw,iord(8)

      integer(8) :: nval8
      integer(4) :: nbits,ibit,ibay(*)

      integer(4) :: nval1
      integer(4) :: nval2
      integer(8) :: nval3


      if(nbits<0) call bort('BUFRLIB: UPB8 - nbits < zero !!!!!')
      if(nbits>2*nbitw) nval=0
      if(nbits>2*nbitw) return

      jbit=ibit
      call upb(nval1,max(nbits-nbitw,0),ibay,jbit)
      call bits(nval1,32); print*,nval1

      call upb(nval2,min(nbitw,nbits  ),ibay,jbit)
      call bits(nval2,32); print*,nval2

      nval3=nval2
      call bits(nval3,64); print*,nval3

      nval8 = nval1
      call bits(nval8,64); print*,nval8

      nval8 = ior(nval8,nval3)
      call bits(nval8,64); print*,nval8

      end subroutine

!----------------------------------------------------------------------
!----------------------------------------------------------------------

        subroutine bits(nval,nbits)

        integer(8) nval

        do i=1,nbits
        write(6,'(i1)',advance='no') ibits(nval,i,1)
        enddo; print*

        end subroutine

!----------------------------------------------------------------------
!----------------------------------------------------------------------
