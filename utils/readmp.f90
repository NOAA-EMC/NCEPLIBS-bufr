!> @file
!> @brief Read BUFR file containing embedded DX BUFR tables,
!> and print each report one at a time

  program readmp

  implicit none

  character(255)     :: file        !> name of filename to read
  character(8)       :: subset      
  character(1)       :: go          
  integer, parameter :: lunit = 20
  integer            :: idate,ireadmg,ireadsb,i4dy,lundx
  logical            :: exist

! get the filename to open and read

  call getarg(1,file); file=trim(adjustl(file)) 
  if (file == '') call bort('Usage: "readmp bufrfile" will (uf)dump BUFR reports one at a time')
  inquire(file=file,exist=exist)
  if (.not.exist) call bort(trim(file)//' does not exist') 
  open(lunit,file=file,form='unformatted')

  lundx=lunit

  if(iargc()==2) then
     call getarg(2,file); file=trim(adjustl(file))
     inquire(file=file,exist=exist)
     if (exist) then
        open(21,file=file)
        lundx=21
     else
        go='q' 
     endif
  endif

! open the file to bufr and dump the subsets to standard outout one at a time

  call openbf(lunit,'IN',lundx)
  do while(ireadmg(lunit,subset,idate).eq.0)
  do while(ireadsb(lunit).eq.0)
  print*,'message date=',i4dy(idate)
  call ufdump(lunit,6)
  if(go.ne.'q') read(5,'(a)') go
  if(go.eq.'q') stop
  enddo
  enddo

  end program readmp 
