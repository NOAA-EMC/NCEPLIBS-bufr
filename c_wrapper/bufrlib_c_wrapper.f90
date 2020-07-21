 
module bufrlib_c_wrapper
  use iso_c_binding

contains

  function c_to_f_string(c_str) result(str)
    implicit none

    character(kind=c_char,len=1), intent(in) :: c_str(*)
    character(len=:), allocatable :: str
    integer nchars

    nchars = 1
    do while (c_str(nchars) .ne. c_null_char)
       nchars = nchars + 1
    end do

    nchars = nchars - 1
    allocate(character(len=nchars) :: str)
    str = transfer(c_str(1:nchars), str)

  end function c_to_f_string


  ! function f_to_c_string(f_str) result(c_str)
  !   implicit none

  !   character(len=*), target, intent(in) :: f_str(*)

  !   character(len=len(f_str)), pointer :: f_str_ptr(:)
  !   type(c_ptr) :: c_str
  !   integer :: str_len

  !   f_str_ptr => f_str(1)
  !   str_len = len_trim(f_str_ptr)
  !   f_str_ptr(str_len+1:str_len+1) = c_null_char
  !   c_str = c_loc(f_str_ptr)
  ! end function


  subroutine open_fortran_file(unit, filepath) bind(C, name='open_fortran_file')
    implicit none

    integer(c_int), value, intent(in) :: unit
    character(kind=c_char, len=1) :: filepath

    open(unit, FILE=c_to_f_string(filepath))
  end subroutine open_fortran_file


  subroutine close_fortran_file(unit) bind(C, name='close_fortran_file')
    implicit none

    integer(c_int), value, intent(in) :: unit

    close(UNIT=unit)
  end SUBROUTINE close_fortran_file


  subroutine c_openbf(bufr_unit, cio, table_file_id) bind(C, name='open_bufr') 
    implicit none

    integer(c_int), value, intent(in) :: bufr_unit
    character(kind=c_char, len=1), intent(in) :: cio
    integer(c_int), value, intent(in) :: table_file_id

    call OPENBF(bufr_unit, c_to_f_string(cio), table_file_id)

  end subroutine c_openbf


  subroutine c_closebf(bufr_unit) bind(C, name='close_bufr') 
    implicit none

    integer(c_int), value, intent(in) :: bufr_unit

    call CLOSBF(bufr_unit)

  end subroutine c_closebf


  function c_readmg(bufr_unit, subset, iddate) result(ires) bind(C, name='read_next_msg')
    implicit none

    integer(c_int), value, intent(in) :: bufr_unit
    type(c_ptr), intent(out) :: subset
    integer(c_int), intent(out) :: iddate
    
    integer(c_int) :: ires
    integer :: subset_len
    integer :: IREADMG

    character(len=25), pointer :: subset_ptr
    character(len=25), target :: f_subset
    ires = IREADMG(bufr_unit, f_subset, iddate)

    !add the null character
    subset_ptr => f_subset
    subset_len = len_trim(subset_ptr)
    subset_ptr(subset_len+1:subset_len+1) = c_null_char
    subset = c_loc(subset_ptr)

    print *, subset_len
    
  end function c_readmg


  function c_readsb(bufr_unit) result(ires) bind(C, name='read_next_subset')
    implicit none

    integer(c_int), value, intent(in) :: bufr_unit

    integer(c_int) :: ires
    integer :: IREADSB
    
    ires = IREADSB(bufr_unit)

  end function c_readsb


  subroutine c_ufbint(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbint')
    implicit none

    integer(c_int), value, intent(in) :: bufr_unit
    type(c_ptr), intent(inout) ::  c_data
    integer(c_int), intent(inout) :: dim_1, dim_2
    integer(c_int), intent(out) :: iret
    character(kind=c_char, len=1), intent(in) :: table_b_mnemonic

    real, pointer :: f_data
    call c_f_pointer(c_data, f_data)

    call UFBINT(bufr_unit, f_data, dim_1, dim_2, iret, c_to_f_string(table_b_mnemonic))

  end subroutine c_ufbint


  subroutine c_ufbrep(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbrep')
    implicit none

    integer(c_int), value, intent(in) :: bufr_unit
    type(c_ptr), intent(out) :: c_data
    integer(c_int), intent(inout) :: dim_1, dim_2
    integer(c_int), intent(out) :: iret
    character(kind=c_char, len=1), intent(in) :: table_b_mnemonic

    real, pointer :: f_data
    call c_f_pointer(c_data, f_data)

    call UFBREP(bufr_unit, f_data, dim_1, dim_2, iret, c_to_f_string(table_b_mnemonic))

  end subroutine c_ufbrep

end module