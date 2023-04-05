!> @file
!> @brief Enable a number of Fortran BUFRLIB subprograms to be called
!> via wrapper functions from C and C++ application programs.
!>
!> @author Ronald Mclaren @date 2020-07-29

!> This module contains functions which wrap Fortran BUFRLIB subprograms
!> and variables so they can be called from within C and C++ application
!> programs. The signatures of the public functions match their Fortran
!> equivalents, as shown within the documentation for each of the
!> individual functions.
!>
!> Local copies of some Fortran variables are stored as allocatable
!> objects, especially isc, link, jmpb, tag and typ. It's the
!> application program's responsibility to call delete_table_data_f in
!> order to properly delete these variables.
!>
!> @author Ronald Mclaren @date 2020-07-29
module bufr_c2f_interface

  use iso_c_binding

  implicit none

  private
  public :: open_c, close_c
  public :: openbf_c, closbf_c
  public :: datelen_c
  public :: exitbufr_c
  public :: ireadmg_c
  public :: ireadsb_c
  public :: ufbint_c
  public :: ufbrep_c
  public :: mtinfo_c
  public :: status_c
  public :: nemdefs_c
  public :: nemspecs_c
  public :: nemtab_c
  public :: nemtbb_c
  public :: get_isc_c
  public :: get_link_c
  public :: get_itp_c
  public :: get_typ_c
  public :: get_tag_c
  public :: get_jmpb_c
  public :: get_inode_c
  public :: get_nval_c
  public :: get_val_c
  public :: get_inv_c
  public :: get_irf_c
  public :: delete_table_data_c

  integer, allocatable, target, save :: isc_f(:)
  integer, allocatable, target, save :: link_f(:)
  integer, allocatable, target, save :: itp_f(:)
  integer, allocatable, target, save :: jmpb_f(:)
  integer, allocatable, target, save :: irf_f(:)
  character(len=10), allocatable, target, save :: tag_f(:)
  character(len=3), allocatable, target, save :: typ_f(:)

contains


!> This function turns a c string into a fortran string.
!>
!> @param[in] c_str - c_char: pointer to a \0 (null) terminated c string
!> @param[out] f_str - character(:): fortran string
!>
!> @author Ronald McLaren @date 2020-07-29
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


!> This subroutine copies a fortran string into a c string buffer.
!>
!> @param[in] f_str - character(*): fortran string to be copied
!> @param[inout] c_str - c_char: c pointer to the target buffer
!> @param[in] c_str_len - integer: length of the c target buffer
!>
!> @author Ronald McLaren @date 2020-07-29
subroutine copy_f_c_str(f_str, c_str, c_str_len)
  character(len=*), target, intent(in) :: f_str
  character(kind=c_char, len=1), intent(inout) :: c_str(*)
  integer, intent(in) :: c_str_len
  integer :: max_str_len

  if (c_str_len /= 0) then
    max_str_len = c_str_len
    c_str(1)(1:max_str_len) = f_str(1:max_str_len)
    c_str(1)(max_str_len:max_str_len) = c_null_char
  end if
end subroutine copy_f_c_str


!> Wraps fortran "open" statement so we can open a Fortran file
!> from a C program.
!>
!> @param[in] lunit - c_int: the integer to use as the fortran file unit
!> @param[in] filepath - c_char: path to the file we want to open.
!>
!> @author Ronald McLaren @date 2020-07-29
subroutine open_c(lunit, filepath) bind(C, name='open_f')
  integer(c_int), value, intent(in) :: lunit
  character(kind=c_char, len=1) :: filepath

  open(lunit, file=c_f_string(filepath))
end subroutine open_c


!> Wraps fortran "close" statement so we can close a Fortran file
!> from a C program.
!>
!> @param[in] lunit - c_int: the integer to use as the fortran file unit
!>
!> @author Ronald McLaren @date 2020-07-29
subroutine close_c(lunit) bind(C, name='close_f')
  integer(c_int), value, intent(in) :: lunit

  close(unit=lunit)
end subroutine close_c


!> Wraps BUFRLIB openbf() subroutine.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number
!> @param[in] cio - c_char: cio string
!> @param[in] table_file_id - c_int: table_file unit number
!>
!> @author Ronald McLaren @date 2020-07-29
subroutine openbf_c(bufr_unit, cio, table_file_id) bind(C, name='openbf_f')
  integer(c_int), value, intent(in) :: bufr_unit
  character(kind=c_char, len=1), intent(in) :: cio
  integer(c_int), value, intent(in) :: table_file_id

  call openbf(bufr_unit, c_f_string(cio), table_file_id)
end subroutine openbf_c


!> Wraps BUFRLIB datelen() subroutine.
!>
!> @param[in] len - c_int: the length of the date string. Possible values are:
!>                         8 for YYMMDDHH, 10 for YYYYMMDDHH.
!>
!> @author Ronald McLaren @date 2023-04-05
subroutine datelen_c(len) bind(C, name='datelen_f')
  integer(c_int), value, intent(in) :: len

  call datelen(len)
end subroutine datelen_c


!> Wraps BUFRLIB closbf() subroutine.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to close
!>
!> @author Ronald McLaren @date 2020-07-29
subroutine closbf_c(bufr_unit) bind(C, name='closbf_f')
  integer(c_int), value, intent(in) :: bufr_unit

  call closbf(bufr_unit)
end subroutine closbf_c


!> Wraps BUFRLIB exitbufr() subroutine. Closes
!> all open file units used by BUFRLIB.
!>
!> @author Ronald McLaren @date 2020-07-29
subroutine exitbufr_c() bind(C, name='exitbufr_f')
  call exitbufr()
end subroutine exitbufr_c


!> Wraps BUFRLIB ireadmg() function.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to read from
!> @param[out] c_subset - c_char: the subset string
!> @param[out] iddate - c_int: datetime of message
!> @param[in] subset_str_len - c_int: length of the subset string
!>
!> @return ireadmg_c - c_int: return code
!>  - 0 new BUFR message was successfully read into internal arrays.
!>  - -1 there are no more BUFR messages in bufr_unit.
!>
!> @author Ronald McLaren @date 2020-07-29
function ireadmg_c(bufr_unit, c_subset, iddate, subset_str_len) result(ires) bind(C, name='ireadmg_f')
  integer(c_int), value, intent(in) :: bufr_unit
  character(kind=c_char, len=1), intent(out) :: c_subset(*)
  integer(c_int), intent(out) :: iddate
  integer(c_int), value, intent(in) :: subset_str_len
  integer(c_int) :: ires
  character(len=25) :: f_subset
  integer :: ireadmg

  ires = ireadmg(bufr_unit, f_subset, iddate)

  if (ires == 0) then
    call copy_f_c_str(f_subset, c_subset, int(subset_str_len))
  end if
end function ireadmg_c


!> Wraps BUFRLIB ireadsb() function.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to read from
!>
!> @return ireadsb_c - c_int: return code
!>  - 0 new BUFR data subset was successfully read into internal arrays.
!>  - -1 there are no more BUFR data subsets in the BUFR message associated with bufr_unit
!>
!> @author Ronald McLaren @date 2020-07-29
function ireadsb_c(bufr_unit) result(ires) bind(C, name='ireadsb_f')
  integer(c_int), value, intent(in) :: bufr_unit
  integer(c_int) :: ires
  integer :: ireadsb

  ires = ireadsb(bufr_unit)
end function ireadsb_c


!> Wraps BUFRLIB ufbint() subroutine.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to read from
!> @param[inout] c_data - c_ptr: c style pointer to a pre-allocated buffer
!> @param[in] dim_1, dim_2 - c_int: dimensionality of data to read or write
!> @param[out] iret - c_int: return value, length of data read
!> @param[in] table_b_mnemonic - c_char: string of mnemonics
!>
!> @author Ronald McLaren @date 2020-07-29
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


!> Wraps BUFRLIB ufbrep() subroutine.
!>
!> @param[in] bufr_unit - c_int: the fortran file unit number to read from
!> @param[inout] c_data - c_ptr: c style pointer to a pre-allocated buffer
!> @param[in] dim_1, dim_2 - c_int: dimensionality of data to read or write
!> @param[out] iret - c_int: return value, length of data read
!> @param[in] table_b_mnemonic - c_char: string of mnemonics
!>
!> @author Ronald McLaren @date 2020-07-29
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


!> Wraps BUFRLIB mtinfo() subroutine.
!>
!> @param[in] path - c_char: the path where the WMO tables are stored
!> @param[in] file_unit_1 - c_int: number to use for first file unit
!> @param[in] file_unit_2 - c_int: number to use for second file unit
!>
!> @author Ronald McLaren @date 2020-07-29
subroutine mtinfo_c(path, file_unit_1, file_unit_2) bind(C, name='mtinfo_f')
  character(kind=c_char, len=1), intent(in) :: path
  integer(c_int), value, intent(in) :: file_unit_1
  integer(c_int), value, intent(in) :: file_unit_2

  call mtinfo(c_f_string(path), file_unit_1, file_unit_2)
end subroutine mtinfo_c


!> Wraps BUFRLIB status() subroutine.
!>
!> @param[in] file_unit - c_int: the fortran file unit number to read from
!> @param[out] lun - c_int: pointer for the file stream
!> @param[out] il - c_int: file status
!> @param[out] im - c_int: message status
!>
!> @author Ronald McLaren  @date 2022-03-23
subroutine status_c(file_unit, lun, il, im) bind(C, name='status_f')
  integer(c_int), value, intent(in) :: file_unit
  integer(c_int), intent(out) :: lun
  integer(c_int), intent(out) :: il
  integer(c_int), intent(out) :: im

  call status(file_unit, lun, il, im)
end subroutine status_c


!> Wraps BUFRLIB nemdefs() subroutine.
!>
!> @param[in] file_unit - c_int: Fortran file unit for the open file
!> @param[in] mnemonic - c_char: mnemonic
!> @param[out] unit_c - c_char: unit str
!> @param[in] unit_str_len - c_int: unit str length
!> @param[out] desc_c - c_char: description string
!> @param[in] desc_str_len - c_int: description str length
!> @param[out] iret - c_int: return value. 0 indicates success -1 indicates failure.
!>
!> @author Ronald McLaren @date 2022-08-08
subroutine nemdefs_c(file_unit, mnemonic, unit_c, unit_str_len, desc_c, desc_str_len, iret) &
        bind(C, name='nemdefs_f')
  integer(c_int), value, intent(in) :: file_unit
  character(kind=c_char,len=1), intent(in) :: mnemonic(*)
  character(kind=c_char, len=1), intent(out) :: unit_c(*)
  integer(c_int), value, intent(in) :: unit_str_len
  character(kind=c_char, len=1), intent(out) :: desc_c(*)
  integer(c_int), value, intent(in) :: desc_str_len
  integer(c_int), intent(out) :: iret

  character(len=24) :: unit_f
  character(len=55) :: desc_f

  ! Get the unit and description strings
  call nemdefs ( file_unit, c_f_string(mnemonic), desc_f, unit_f, iret)

  if (iret == 0) then
    ! Copy the unit fortran string into the resulting C style string.
    call copy_f_c_str(unit_f, unit_c, min(len(unit_f) + 1, unit_str_len))
    ! Copy the descriptor fortran string into the resulting C style string.
    call copy_f_c_str(desc_f, desc_c, min(len(desc_f) + 1, desc_str_len))
  end if
end subroutine nemdefs_c


!> Wraps BUFRLIB nemspecs() subroutine.
!>
!> @param[in] file_unit - c_int: Fortran file unit for the open file
!> @param[in] mnemonic - c_char: mnemonic
!> @param[in] mnemonic_idx - c_int: indicates specific mnemonic element (if repeated)
!> @param[out] scale - c_int: scale of element
!> @param[out] reference - c_int: reference of element
!> @param[out] bits - c_int: number of bits representing the element
!> @param[out] iret - c_int: return value. 0 indicates success -1 indicates failure.
!>
!> @author Ronald McLaren  @date 2022-08-08
subroutine nemspecs_c(file_unit, mnemonic, mnemonic_idx, scale, reference, bits, iret) &
        bind(C, name='nemspecs_f')
  integer(c_int), value, intent(in) :: file_unit
  character(kind=c_char,len=1), intent(in) :: mnemonic(*)
  integer(c_int), value, intent(in) ::mnemonic_idx
  integer(c_int), intent(out) :: scale
  integer(c_int), intent(out) :: reference
  integer(c_int), intent(out) :: bits
  integer(c_int), intent(out) :: iret

  ! Get the scale, reference and bits
  call nemspecs(file_unit, c_f_string(mnemonic), mnemonic_idx, scale, reference, bits, iret)
end subroutine nemspecs_c


!> Wraps BUFRLIB nemtab() subroutine.
!>
!> @param[in] bufr_unit - c_int: the bufr file pointer
!> @param[in] mnemonic - c_char: mnemonic
!> @param[out] descriptor - c_int: the binary descriptor for the mnemonic
!> @param[out] table_type - c_char: 'A', 'B', 'C', or 'D', depending on table type
!> @param[out] table_idx - c_int: the table index, or 0 if not found
!>
!> @author Ronald McLaren  @date 2022-08-16
subroutine nemtab_c(bufr_unit, mnemonic, descriptor, table_type, table_idx) &
        bind(C, name='nemtab_f')
  integer(c_int), value, intent(in) :: bufr_unit
  character(kind=c_char,len=1), intent(in) :: mnemonic(*)
  integer(c_int), intent(out) :: descriptor
  character(kind=c_char,len=1), intent(out) :: table_type(*)
  integer(c_int), intent(out) :: table_idx

  character(len=1) :: table_type_f

  call nemtab(bufr_unit, c_f_string(mnemonic), descriptor, table_type_f, table_idx)

  table_type(1)(1:1) = table_type_f(1:1)
end subroutine nemtab_c


!> Wraps BUFRLIB nemtbb() subroutine.
!>
!> @param[in] bufr_unit - c_int: the bufr file pointer
!> @param[in] table_idx - c_int: Table B index
!> @param[out] unit_str - c_char: unit str
!> @param[in] unit_str_len - c_int: unit str length
!> @param[out] scale - c_int: scale of element
!> @param[out] reference - c_int: reference of element
!> @param[out] bits - c_int: bits of element
!>
!> @author Ronald McLaren @date 2022-08-16
subroutine nemtbb_c(bufr_unit, table_idx, unit_str, unit_str_len, scale, reference, bits) &
        bind(C, name='nemtbb_f')
  integer(c_int), intent(in), value :: bufr_unit
  integer(c_int), intent(in), value :: table_idx
  character(kind=c_char,len=1), intent(out) :: unit_str(*)
  integer(c_int), intent(in), value :: unit_str_len
  integer(c_int), intent(out) :: scale
  integer(c_int), intent(out) :: reference
  integer(c_int), intent(out) :: bits

  character(len=24) :: unit_str_f

  ! Get the scale, reference and bits
  call nemtbb( bufr_unit, table_idx, unit_str_f, scale, reference, bits)
  call copy_f_c_str(unit_str_f, unit_str, min(len(unit_str_f) + 1, unit_str_len))
end subroutine nemtbb_c


!> Get copy of the moda_tables ISC array.
!>
!> @param[out] isc_ptr - c_ptr: c style pointer to the ISC array
!> @param[out] isc_size - c_int: size of the ISC array
!>
!>  @author Ronald McLaren  @date 2022-03-23
subroutine get_isc_c(isc_ptr, isc_size) bind(C, name='get_isc_f')
  use moda_tables
  type(c_ptr), intent(inout) :: isc_ptr
  integer(c_int), intent(out) :: isc_size

  allocate(isc_f(ntab))
  isc_f(1:ntab) = isc(1:ntab)
  isc_size = size(isc_f)
  isc_ptr = c_loc(isc_f(1))
end subroutine get_isc_c


!> Get copy of the moda_tables LINK array.
!>
!> @param[out] link_ptr - c_ptr: c style pointer to the LINK array
!> @param[out] link_size - c_int: size of the LINK array
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_link_c(link_ptr, link_size) bind(C, name='get_link_f')
  use moda_tables
  type(c_ptr), intent(inout) :: link_ptr
  integer(c_int), intent(out) :: link_size

  allocate(link_f(ntab))
  link_f(1:ntab) = link(1:ntab)
  link_size = size(link_f)
  link_ptr = c_loc(link_f(1))
end subroutine get_link_c


!> Get copy of the moda_tables ITP array.
!>
!> @param[out] itp_ptr - c_ptr: c style pointer to the ITP array
!> @param[out] itp_size - c_int: size of the ITP array
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_itp_c(itp_ptr, itp_size) bind(C, name='get_itp_f')
  use moda_tables
  type(c_ptr), intent(inout) :: itp_ptr
  integer(c_int), intent(out) :: itp_size

  allocate(itp_f(ntab))
  itp_f(1:ntab) = itp(1:ntab)
  itp_size = size(itp_f)
  itp_ptr = c_loc(itp_f(1))
end subroutine get_itp_c


!> Get copy of the moda_tables TYP array.
!>
!> @param[out] typ_ptr - c_ptr: c style pointer to the TYP array
!> @param[out] typ_len - c_int: size of each string within the TYP array
!> @param[out] mem_size - c_int: size of the TYP array
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_typ_c(typ_ptr, typ_len, mem_size) bind(C, name='get_typ_f')
  use moda_tables
  type(c_ptr), intent(inout) :: typ_ptr
  integer(c_int), intent(out) :: typ_len
  integer(c_int), intent(out) :: mem_size

  allocate(typ_f(ntab))
  typ_f(1:ntab) = typ(1:ntab)
  typ_len = len(typ(1))
  mem_size = size(typ_f)
  typ_ptr = c_loc(typ_f(1))
end subroutine get_typ_c


!> Get copy of the moda_tables TAG array.
!>
!> @param[out] tag_ptr - c_ptr: c style pointer to the TAG array
!> @param[out] tag_len - c_int: length of the tag string
!> @param[out] mem_size - c_int: size of TAG array
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_tag_c(tag_ptr, tag_len, mem_size) bind(C, name='get_tag_f')
  use moda_tables
  type(c_ptr), intent(inout) :: tag_ptr
  integer(c_int), intent(out) :: tag_len
  integer(c_int), intent(out) :: mem_size

  allocate(tag_f(ntab))
  tag_f(1:ntab) = tag(1:ntab)
  tag_len = len(tag(1))
  mem_size = size(tag_f)
  tag_ptr = c_loc(tag_f(1))
end subroutine get_tag_c


!> Get copy of the moda_tables JMPB array.
!>
!> @param[out] jmpb_ptr - c_ptr: c style pointer to the JMPB array
!> @param[out] jmpb_size - c_int: length of the array
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_jmpb_c(jmpb_ptr, jmpb_size) bind(C, name='get_jmpb_f')
  use moda_tables
  type(c_ptr), intent(inout) :: jmpb_ptr
  integer(c_int), intent(out) :: jmpb_size

  allocate(jmpb_f(ntab))
  jmpb_f(1:ntab) = jmpb(1:ntab)
  jmpb_size = size(jmpb_f)
  jmpb_ptr = c_loc(jmpb_f(1))
end subroutine get_jmpb_c


!> Get copy of the moda_tables IRF array.
!>
!> @param[out] irf_ptr - c_ptr: c style pointer to the IRF array
!> @param[out] irf_size - c_int: length of the array
!>
!> @author Ronald McLaren @date 2023-04-05
subroutine get_irf_c(irf_ptr, irf_size) bind(C, name='get_irf_f')
  use moda_tables
  type(c_ptr), intent(inout) :: irf_ptr
  integer(c_int), intent(out) :: irf_size

  allocate(irf_f(ntab))
  irf_f(1:ntab) = irf(1:ntab)
  irf_size = size(irf_f)
  irf_ptr = c_loc(irf_f(1))
end subroutine get_irf_c


!> Get the bufr node idx for the start node of the subset.
!>
!> @param[in] lun - c_int: pointer for the file stream
!> @param[out] start_node - c_int: the start node of the subset
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_inode_c(lun, start_node) bind(C, name='get_inode_f')
  use moda_msgcwd
  integer(c_int), value, intent(in) :: lun
  integer(c_int), intent(out) :: start_node

  start_node = inode(lun)
end subroutine get_inode_c


!> Get the number of values in the current subset
!>
!> @param[in] lun - c_int: pointer for the file stream
!> @param[out] num_nodes - c_int: number of values in the subset
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_nval_c(lun, num_nodes) bind(C, name='get_nval_f')
  use moda_usrint
  integer(c_int), value, intent(in) :: lun
  integer(c_int), intent(out) :: num_nodes

  num_nodes = nval(lun)
end subroutine get_nval_c


!> Get pointer to the moda_usrint VAL array.
!>
!> @param[in] lun - c_int: pointer for the file stream
!> @param[out] val_ptr - c_ptr: c style pointer to the VAL array
!> @param[out] val_size - c_int: length of the array
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_val_c(lun, val_ptr, val_size) bind(C, name='get_val_f')
  use moda_usrint
  integer(c_int), value, intent(in) :: lun
  type(c_ptr), intent(inout) :: val_ptr
  integer(c_int), intent(out) :: val_size

  val_size = size(val(:, lun))
  val_ptr = c_loc(val(1, lun))
end subroutine get_val_c


!> Get pointer to the moda_usrint INV array.
!>
!> @param[in] lun - c_int: pointer for the file stream
!> @param[out] inv_ptr - c_ptr: c style pointer to the INV array
!> @param[out] inv_size - c_int: length of the array
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine get_inv_c(lun, inv_ptr, inv_size) bind(C, name='get_inv_f')
  use moda_usrint
  integer(c_int), value, intent(in) :: lun
  type(c_ptr), intent(inout) :: inv_ptr
  integer(c_int), intent(out) :: inv_size

  inv_size = size(inv(:, lun))
  inv_ptr = c_loc(inv(1, lun))
end subroutine get_inv_c


!> Deletes the copies of the moda_tables arrays.
!>
!> @author Ronald McLaren @date 2022-03-23
subroutine delete_table_data_c() bind(C, name='delete_table_data_f')
  if (allocated(isc_f)) deallocate(isc_f)
  if (allocated(link_f)) deallocate(link_f)
  if (allocated(itp_f)) deallocate(itp_f)
  if (allocated(typ_f)) deallocate(typ_f)
  if (allocated(tag_f)) deallocate(tag_f)
  if (allocated(jmpb_f)) deallocate(jmpb_f)
  if (allocated(irf_f)) deallocate(irf_f)
end subroutine delete_table_data_c

end module bufr_c2f_interface
