!> @file
!> @brief Split a BUFR file into separate BUFR files by subset type
!> @author WOOLLEN
!> @date 2000-01-01

!> Read BUFR file messages, collating them into output files by message type/subtype (eg NC001002, aka subset type)
!> This is the opposite of combfr.f, which concatenates BUFR messages from listed input files

program split_by_subset

  implicit none

  integer, parameter :: maxsub = 100
  integer, parameter :: lunit = 20
  real(8), parameter :: bmiss = 10e10

  character(len=255) :: finput
  character(len=8) :: subset, sub(maxsub)
  integer(4), dimension(3, maxsub) :: ninv
  integer(4) :: idate
  integer(4) :: ireadmg, nmsub, iupvs01 !> functions from bufr library
  integer :: nsub, isub, ii, jj
  logical :: file_exists
  integer, dimension(maxsub) :: lsubunit

  !> get input filename from stdin

  if(iargc()==1) then
     call getarg(1, finput)
     inquire(file=trim(adjustl(finput)), exist=file_exists)
     if (file_exists) then
        open(lunit, file=trim(adjustl(finput)), form='unformatted')
        call openbf(lunit,'IN',lunit)
     else
        call bort('File ' // trim(adjustl(finput)) // ' does not exist')       
     endif
  else
     call bort('Usage: "split_by_subset bufrfile" will split a BUFR file into subsets')
  endif

  !> initialize counters
  ninv = 0
  nsub = 0
  lsubunit = 50

  !> loop over the messages in the bufr file one-by-one
  ireadmg_loop: do while(ireadmg(lunit, subset, idate) == 0)

    !> if already encountered a subset type, re-use index
    isub = 0
    do ii = 1, nsub
      if (subset == sub(ii)) then
        isub = ii
        continue
      endif
    enddo

    !> If new subset type is found, register it
    new_subset: if (isub == 0) then
      !> if too many subset types in a file, abort
      if (nsub+1 > maxsub) call bort('nsub too big')
      sub(nsub+1) = subset
      isub = nsub+1
      lsubunit(isub) = 50+isub  !> Store lunits of output subset files

      !> open an output file for this subset type
      open(lsubunit(isub), file=trim(adjustl(subset)), form='unformatted')
      call openbf(lsubunit(isub), 'OUT', lunit)
      if (nsub == 0) call maxout(20000)
      nsub = nsub+1
    endif new_subset

    !> update the inventory counts
    ninv(1, isub) = ninv(1, isub) + 1
    ninv(2, isub) = ninv(2, isub) + nmsub(lunit)
    ninv(3, isub) = ninv(3, isub) + iupvs01(lunit, 'LENM')

    !> copy the subset type message into the output file for this type
    call copymg(lunit, lsubunit(isub))

  enddo ireadmg_loop

  !> close the output files and flush the buffers
  do ii = 1,nsub
    call closbf(lsubunit(50+ii))
  enddo

  !> print the inventory
  write(6,'(a)') repeat('#',48)
  write(6,101) 'TYPE', 'MESSAGES', 'SUBSETS', 'BYTES'
  write(6,'(a)') repeat('#',48)
  do ii=1,nsub
    write(6,102) sub(ii), (ninv(jj,ii),jj=1,3)
    if (ii > 1) then
       ninv(1,1) = ninv(1,1)+ninv(1,ii)
       ninv(2,1) = ninv(2,1)+ninv(2,ii)
       ninv(3,1) = ninv(3,1)+ninv(3,ii)
    endif
  enddo
  write(6,'(a)') repeat('-',48)
  write(6,102) 'TOTAL', (ninv(jj,1),jj=1,3)
  write(6,'(a)') repeat('#',48)

101 format(a8,2x,3(a10,4x))
102 format(a8,2x,3(i10,4x))

  stop
end program split_by_subset

