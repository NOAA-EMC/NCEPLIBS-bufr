!> @file
!> @brief Read BUFR file containing embedded DX BUFR tables,
!> and print the tables to stdout
!> @author WOOLLEN @date 2000-01-01

program gettab

  implicit none

  character(len=255) :: file        !> name of filename to read
  integer, parameter :: lunit = 20
  logical :: exist

! get the filename to open and read

  call getarg(1,file); file=trim(adjustl(file)) 
  if (file == '') call bort('Usage: "gettab bufrfile" will print the internal BUFR table')
  inquire(file=file,exist=exist)
  if (.not.exist) call bort(trim(file)//' does not exist') 

! open the file and dump the bufr table to standard outout

  open(lunit,file=file,status='old', form='unformatted')
  call openbf(lunit, 'IN', lunit)
  call dxdump(lunit, 6)
  call closbf(lunit)
  close(lunit)

  stop
end program gettab

