subroutine fortran_open(filename, lunit, format, position, iret)
  implicit none
  character*(*), intent(in) :: filename, format, position
  integer, intent(in)  :: lunit
  integer, intent(out) :: iret
  open(lunit, file=trim(filename), form=trim(format), &
       position=trim(position), iostat=iret)
  return
end
