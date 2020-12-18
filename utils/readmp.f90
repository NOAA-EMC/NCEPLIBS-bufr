!> @file
!> @brief Read BUFR file containing embedded DX tables, and print
!> each report one at a time

program readmp

implicit none

character(len=255) :: finput
character(len=8) :: subset
character(len=1) :: go
integer(4) :: ireadmg, ireadsb, idate, i4dy
integer, parameter :: lunit = 20
logical :: file_exists

call getarg(1, finput)
if (trim(adjustl(finput)) == '') finput = 'fort.20'
inquire(file=trim(adjustl(finput)), exist=file_exists)
if (.not. file_exists) then
  write(6,'(3(A,X))') "File", trim(adjustl(finput)), 'does not exist, ABORT!'
  stop
endif

open(lunit,file=trim(adjustl(finput)), status='old', form='unformatted')
call openbf(lunit,'IN',lunit)

do while(ireadmg(lunit,subset,idate) == 0)
  do while(ireadsb(lunit) == 0)
    write(6,'(A)') repeat('#',80)
    write(6,'(A,X,I10)') 'message date =', i4dy(idate)
    call ufdump(lunit,6)
    write(6,'(A)') repeat('#',80)
    read(5,'(a)') go
    if(go == 'q') then
      call closbf(lunit)
      close(lunit)
      stop
    endif
  enddo
enddo

call closbf(lunit)
close(lunit)

stop
end
