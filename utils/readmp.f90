!> @file
!> @brief Read BUFR file containing embedded DX BUFR tables,
!> and print each report one at a time

  program readmp

  implicit none

  character(255)     :: file        !> name of filename to read
  character(8)       :: subset      
  character(1)       :: go          
  integer, parameter :: lunit = 20
  integer            :: idate,ireadmg,ireadsb,i4dy
  logical            :: exist

! get the filename to open and read

  call getarg(1,file); file=trim(adjustl(file)) 
  if (file == '') call bort('Usage: "gettab bufrfile" will print the internal BUFR table')
  inquire(file=file,exist=exist)
  if (.not.exist) call bort(file//' does not exist') 
  call getarg(2,go); go=trim(adjustl(go)) ! this for testing !
  open(lunit,file=file,form='unformatted')

! open the file to bufr and dump the subsets to standard outout one at a time

  call openbf(lunit,'IN',lunit)
  do while(ireadmg(lunit,subset,idate).eq.0)
  do while(ireadsb(lunit).eq.0)
  print*,'message date=',i4dy(idate)
  call ufdump(lunit,6)
  if(go.ne.'q') read(5,'(a)') go
  if(go.eq.'q') stop
  enddo
  enddo

  end program readmp 
