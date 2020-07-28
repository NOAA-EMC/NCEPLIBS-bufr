 
module bufr_c_interface_mod
  use iso_c_binding
  implicit none

  private
  public :: open_c
  public :: close_c
  public :: openbf_c
  public :: ireadmg_c
  public :: ireadsb_c
  public :: ufbint_c
  public :: ufbrep_c
  public :: dxdump_c

contains

  !Private
  function c_f_string(c_str) result(f_str)
    character(kind=c_char,len=1), intent(in) :: c_str(*)
    character(len=:), allocatable :: f_str
    integer nchars

    nchars = 1
    do while (c_str(nchars) .ne. c_null_char)
       nchars = nchars + 1
    end do
    nchars = nchars - 1

    allocate(character(len=nchars) :: f_str)
    f_str = transfer(c_str(1:nchars), f_str)
  end function c_f_string


  subroutine copy_f_c_str(f_str, c_str, c_str_len)
    character(len=*), target, intent(in) :: f_str
    character(kind=c_char, len=1), intent(inout) :: c_str(*)
    integer, intent(in) :: c_str_len
    integer :: max_str_len

    if (c_str_len .ne. 0) then
      max_str_len = min(c_str_len - 1, len_trim(f_str))
      c_str(1)(1:max_str_len) = f_str(1:max_str_len)
      c_str(1)(max_str_len:max_str_len) = c_null_char
    end if
  end subroutine copy_f_c_str

  !Public
  subroutine open_c(unit, filepath) bind(C, name='open_f')
    integer(c_int), value, intent(in) :: unit
    character(kind=c_char, len=1) :: filepath

    open(unit, file=c_f_string(filepath))
  end subroutine open_c


  subroutine close_c(unit) bind(C, name='close_f')
    integer(c_int), value, intent(in) :: unit

    close(unit=unit)
  end subroutine close_c


  subroutine openbf_c(bufr_unit, cio, table_file_id) bind(C, name='openbf_f') 
    integer(c_int), value, intent(in) :: bufr_unit
    character(kind=c_char, len=1), intent(in) :: cio
    integer(c_int), value, intent(in) :: table_file_id

    call openbf(bufr_unit, c_f_string(cio), table_file_id)
  end subroutine openbf_c


  subroutine closbf_c(bufr_unit) bind(C, name='closbf_f') 
    integer(c_int), value, intent(in) :: bufr_unit

    call closbf(bufr_unit)
  end subroutine closbf_c


  function ireadmg_c(bufr_unit, c_subset, iddate, subset_str_len) result(ires) bind(C, name='ireadmg_f')
    integer(c_int), value, intent(in) :: bufr_unit
    character(kind=c_char, len=1), intent(inout) :: c_subset(*)
    integer(c_int), intent(out) :: iddate
    integer(c_int), value, intent(in) :: subset_str_len
    integer(c_int) :: ires
    integer :: subset_len
    character(len=25) :: f_subset
    integer :: ireadmg

    ires = ireadmg(bufr_unit, f_subset, iddate)
    call copy_f_c_str(f_subset, c_subset, int(subset_str_len))
  end function ireadmg_c


  function ireadsb_c(bufr_unit) result(ires) bind(C, name='ireadsb_f')
    integer(c_int), value, intent(in) :: bufr_unit
    integer(c_int) :: ires
    integer :: ireadsb
    
    ires = ireadsb(bufr_unit)
  end function ireadsb_c


  subroutine ufbint_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbint_f')
    integer(c_int), value, intent(in) :: bufr_unit
    type(c_ptr), intent(inout) ::  c_data
    integer(c_int), value, intent(in) :: dim_1, dim_2
    integer(c_int), intent(out) :: iret
    character(kind=c_char, len=1), intent(in) :: table_b_mnemonic
    real, pointer :: f_data

    call c_f_pointer(c_data, f_data)
    call ufbint(bufr_unit, f_data, dim_1, dim_2, iret, c_f_string(table_b_mnemonic))
  end subroutine ufbint_c


  subroutine ufbrep_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbrep_f')
    integer(c_int), value, intent(in) :: bufr_unit
    type(c_ptr), intent(inout) :: c_data
    integer(c_int), value, intent(in) :: dim_1, dim_2
    integer(c_int), intent(out) :: iret
    character(kind=c_char, len=1), intent(in) :: table_b_mnemonic
    real, pointer :: f_data

    call c_f_pointer(c_data, f_data)
    call ufbrep(bufr_unit, f_data, dim_1, dim_2, iret, c_f_string(table_b_mnemonic))
  end subroutine ufbrep_c


  subroutine dxdump_c(bufr_unit, table_unit) bind(C, name='dxdump_f')
    integer(c_int), value, intent(in) :: bufr_unit
    integer(c_int), value, intent(in) :: table_unit

    call dxdump(bufr_unit, table_unit)
  end subroutine dxdump_c

end module bufr_c_interface_mod
