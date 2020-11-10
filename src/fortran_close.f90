subroutine fortran_close(lunit, iret)
  implicit none
  integer, intent(in)  :: lunit
  integer, intent(out) :: iret
  close(lunit, iostat=iret)
  return
end
