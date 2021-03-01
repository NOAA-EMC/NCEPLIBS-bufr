!> @file
!> @authors Ronald Mclaren
!> @date 2020-07-29
!>
!> @brief This module contains functions which map relevant fortran library
!>        functions so they can be called from c and c++. The signatures of
!>        the public functions match their fortran equivalents (please see
!>        their documentation if you have questions).
!>
module bufr_c_interface_mod

  use iso_c_binding

  implicit none

  private
  public :: open_c, close_c
  public :: openbf_c, closbf_c
  public :: exitbufr_c
  public :: ireadmg_c
  public :: ireadsb_c
  public :: ufbint_c
  public :: ufbrep_c
  public :: mtinfo_c

contains

!Private

!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief This function turns a c string into a fortran string.
!>
!> @param[in] c_str - c_char: pointer to a \0 (null) terminated c string
!> @param[out] f_str - character(:): fortran string
!>
function c_f_string(c_str) result(f_str)
  character(kind=c_char,len=1), intent(in) :: c_str(*)
  character(len=:), allocatable :: f_str
  integer :: nchars

  nchars = 1
  do while (c_str(nchars) /= c_null_char)
      nchars = nchars + 1
  end do
  nchars = nchars - 1

  allocate(character(len=nchars) :: f_str)
  f_str = transfer(c_str(1:nchars), f_str)
end function c_f_string


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief This subroutine copies a fortran string into a c string buffer.
!>
!> @param[in] f_str - character(*): fortran string to be copied
!> @param[inout] c_str - c_char: c pointer to the target buffer
!> @param[in] c_str_len - integer: length of the c target buffer
!>
subroutine copy_f_c_str(f_str, c_str, c_str_len)
  character(len=*), target, intent(in) :: f_str
  character(kind=c_char, len=1), intent(inout) :: c_str(*)
  integer, intent(in) :: c_str_len
  integer :: max_str_len

  if (c_str_len /= 0) then
    max_str_len = min(c_str_len - 1, len_trim(f_str))
    c_str(1)(1:max_str_len) = f_str(1:max_str_len)
    c_str(1)(max_str_len:max_str_len) = c_null_char
  end if
end subroutine copy_f_c_str

!Public

!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps fortran "open" statement so we can open a Fortran file
!>.       from a C program.
!>
!> @param[in] lunit - c_int: the integer to use as the fortran file unit
!> @param[in] filepath - c_char: path to the file we want to open.
!>
subroutine open_c(lunit, filepath) bind(C, name='open_f')
  integer(c_int), value, intent(in) :: lunit
  character(kind=c_char, len=1) :: filepath

  open(lunit, file=c_f_string(filepath))
end subroutine open_c


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps fortran "close" statement so we can close a Fortran file
!>.       from a C program.
!>
!> @param[in] lunit - c_int: the integer to use as the fortran file unit
!>
subroutine close_c(lunit) bind(C, name='close_f')
  integer(c_int), value, intent(in) :: lunit

  close(unit=lunit)
end subroutine close_c


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps NCEPLIB-bufr "openbf" subroutine.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number
!> @param[in] cio - c_char: cio string
!> @param[in] table_file_id - c_int: table_file unit number
!>
subroutine openbf_c(bufr_unit, cio, table_file_id) bind(C, name='openbf_f')
  integer(c_int), value, intent(in) :: bufr_unit
  character(kind=c_char, len=1), intent(in) :: cio
  integer(c_int), value, intent(in) :: table_file_id

  call openbf(bufr_unit, c_f_string(cio), table_file_id)
end subroutine openbf_c


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps NCEPLIB-bufr "closbf" subroutine.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to close
!>
subroutine closbf_c(bufr_unit) bind(C, name='closbf_f')
  integer(c_int), value, intent(in) :: bufr_unit

  call closbf(bufr_unit)
end subroutine closbf_c


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps NCEPLIB-bufr "exitbufr" subroutine. Closes
!>        all open file units used by NCEPLIB-bufr.
!>
subroutine exitbufr_c() bind(C, name='exitbufr_f')
  call exitbufr()
end subroutine exitbufr_c


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps NCEPLIB-bufr "ireadmg" subroutine.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to read from
!> @param[inout] c_subset - c_char: the subset string
!> @param[out] iddate - c_int: datetime of message
!> @param[in] subset_str_len - c_int: length of the subset string
!>
function ireadmg_c(bufr_unit, c_subset, iddate, subset_str_len) result(ires) bind(C, name='ireadmg_f')
  integer(c_int), value, intent(in) :: bufr_unit
  character(kind=c_char, len=1), intent(inout) :: c_subset(*)
  integer(c_int), intent(out) :: iddate
  integer(c_int), value, intent(in) :: subset_str_len
  integer(c_int) :: ires
  character(len=25) :: f_subset
  integer :: ireadmg

  ires = ireadmg(bufr_unit, f_subset, iddate)
  call copy_f_c_str(f_subset, c_subset, int(subset_str_len))
end function ireadmg_c


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps NCEPLIB-bufr "ireadsb" function.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to read from
!>
function ireadsb_c(bufr_unit) result(ires) bind(C, name='ireadsb_f')
  integer(c_int), value, intent(in) :: bufr_unit
  integer(c_int) :: ires
  integer :: ireadsb

  ires = ireadsb(bufr_unit)
end function ireadsb_c


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps NCEPLIB-bufr "ufbint" function.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to read from
!> @param[inout] c_data - c_ptr: c style pointer to a pre-allocated buffer
!> @param[in] dim_1, dim_2 - c_int: dimensionality of data to read or write
!> @param[out] iret - c_int: return value, length of data read
!> @param[in] table_b_mnemonic - c_char: string of mnemonics
!>
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


!> @author Ronald McLaren
!> @date 2020-07-29
!>
!> @brief Wraps NCEPLIB-bufr "ufbrep" function.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to read from
!> @param[inout] c_data - c_ptr: c style pointer to a pre-allocated buffer
!> @param[in] dim_1, dim_2 - c_int: dimensionality of data to read or write
!> @param[out] iret - c_int: return value, length of data read
!> @param[in] table_b_mnemonic - c_char: string of mnemonics
!>
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


!> @author Ronald McLaren
!> @date 2021-02-24
!>
!> @brief Wraps NCEPLIB-bufr "mtinfo" function.
!>
!> @param[in] path - c_char: the path where the WMO tables are stored
!> @param[in] file_unit_1 - c_int: number to use for first file unit
!> @param[in] file_unit_2 - c_int: number to use for second file unit
!>
subroutine mtinfo_c(path, file_unit_1, file_unit_2) bind(C, name='mtinfo_f')
  character(kind=c_char, len=1), intent(in) :: path
  integer(c_int), value, intent(in) :: file_unit_1
  integer(c_int), value, intent(in) :: file_unit_2

  call mtinfo(c_f_string(path), file_unit_1, file_unit_2)
end subroutine mtinfo_c

end module bufr_c_interface_mod
