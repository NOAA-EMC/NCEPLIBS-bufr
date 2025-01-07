!> @file
!> @brief  Produce an inventory of subsets in a bufr file.
!>
!> @author J Woollen @date 1994

!> Usage: binv \<bufrfile\> will print bufrfile inventory by message type.
!>
!> @return 0 for success, error message otherwise.
!>
!> @author J Woollen @date 1994
program binv

  parameter (maxsub=100)

  character*255 file
  character*8   subset
  character*8   sub(maxsub)
  integer*8     ninv(3,maxsub)
  real*8        xsub, xmsg
  logical       exist

  data lunbf  /20/

  !-----------------------------------------------------------------------
  nmbyt(lunit)= iupvs01(lunit,'LENM')
  !-----------------------------------------------------------------------

  !  get filename

  narg=command_argument_count()
  if(narg/=1) then
     print *,'Usage: binv <bufrfile> will print bufrfile inventory by message type'
     stop 2
  endif

  call get_command_argument(1,file)
  file = trim(file)//char(0)
  inquire(file=file,exist=exist)
  if (.not.exist) then
     print *,trim(file)//' does not exist'
     stop 3
  endif
  open(lunbf,file=file,form='unformatted')

  ninv = 0
  nsub = 0


  !  Compute a message inventory by subsets
  !  --------------------------------------

  call openbf(lunbf,'IN',lunbf)
  do while(ireadmg(lunbf,subset,idate)==0)
     isub = 0
     do i=1,nsub
        if(subset==sub(i)) isub = i
     enddo
     if(isub==0) then
        if(nsub+1>maxsub) call bort('NSUB TOO BIG')
        sub(nsub+1) = subset
        nsub = nsub+1
        isub = nsub
     endif
     ninv(1,isub) = ninv(1,isub)+1
     ninv(2,isub) = ninv(2,isub)+nmsub(lunbf)
     ninv(3,isub) = ninv(3,isub)+nmbyt(lunbf)
  enddo

  !  Print the inventory
  !  -------------------

  print*
  print'(a4,6x,2(a10,4x),a11)','type','messages','subsets','bytes'
  print*
  do j=1,nsub
     xmsg = ninv(1,j)
     xsub = ninv(2,j)
     print'(a8,2x,2(i10,4x),i11,4x,f8.2)',sub(j),(ninv(i,j),i=1,3),xsub/xmsg
     if(j>1) then
        ninv(1,1) = ninv(1,1)+ninv(1,j)
        ninv(2,1) = ninv(2,1)+ninv(2,j)
        ninv(3,1) = ninv(3,1)+ninv(3,j)
     endif
  enddo

  print'(a8,2x,2(i10,4x),i11,4x)','TOTAL   ',(ninv(i,1),i=1,3)
  print*

  stop
end program binv
