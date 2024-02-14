!> @file
!> @brief Read BUFR file containing embedded DX BUFR tables,
!> and print the tables to stdout.
!>
!> @author Woollen @date 2000-01-01

!> Read BUFR file containing embedded DX BUFR tables,
!> and print the tables to stdout.
!>
!> @return 0 for success, error code otherwise.
!>
!> @author Woollen @date 2000-01-01
program gettab

  implicit none

  character(len=255) :: file        !> name of filename to read
  integer, parameter :: lunit = 20
  logical :: exist

! get the filename to open and read

  call get_command_argument(1,file); file=trim(adjustl(file))
  if (file == '') then
    print *,'Usage: gettab <bufrfile> will print the internal BUFR table'
    call exit(2)
  else
    inquire(file=file,exist=exist)
    if (.not.exist) then
      print *,trim(file)//' does not exist'
      call exit(3)
    endif
  endif

! open the file and dump the bufr table to standard outout

  open(lunit,file=file,status='old', form='unformatted')
  call openbf(lunit, 'IN', lunit)
  call dxdump(lunit, 6)
  call closbf(lunit)
  close(lunit)

  stop
end program gettab

