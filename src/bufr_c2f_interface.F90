!> @file
!> @brief Enable a number of Fortran NCEPLIBS-bufr subprograms to be called
!> from within C.
!>
!> @author Ronald Mclaren @date 2020-07-29

!> Wrap Fortran NCEPLIBS-bufr subprograms and variables so they can be called
!> from within C.
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
  public :: open_c, close_c, openbf_c, closbf_c
  public :: exitbufr_c, bort_c
  public :: ireadmg_c, ireadsb_c, ireadns_c, openmb_c
  public :: ufbint_c, ufbrep_c, ufbseq_c
  public :: mtinfo_c, bvers_c, status_c, ibfms_c
  public :: get_isc_c, get_link_c, get_itp_c, get_typ_c, get_tag_c, get_jmpb_c
  public :: get_inode_c, get_nval_c, get_val_c, get_inv_c, get_irf_c, readlc_c
  public :: delete_table_data_c
  public :: iupbs01_c, iupb_c, imrkopr_c, istdesc_c, ifxy_c
  public :: igetntbi_c, igettdi_c, stntbi_c
  public :: igetprm_c, isetprm_c, maxout_c, igetmxby_c
  public :: elemdx_c, cadn30_c, strnum_c, wrdlen_c, uptdd_c, pktdd_c
  public :: nemdefs_c, nemspecs_c, nemtab_c, nemtbb_c, numtbd_c

  integer, allocatable, target, save :: isc_f(:), link_f(:), itp_f(:), jmpb_f(:), irf_f(:)
  character(len=10), allocatable, target, save :: tag_f(:)
  character(len=3), allocatable, target, save :: typ_f(:)

  contains

    !> Convert a C string into a Fortran string.
    !>
    !> @param c_str - Pointer to a null-terminated C string.
    !> @param f_str - Fortran string.
    !>
    !> Allocated arrays in Fortran are automatically deallocated
    !> once the array goes out of scope, so there's no need to
    !> deallocate f_str after it's used within the calling routine.
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

    !> Copy a Fortran string into a C string buffer.
    !>
    !> @param f_str - Fortran string to be copied.
    !> @param c_str - C pointer to the target buffer.
    !> @param c_str_len - Length of the C target buffer.
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

    !> Open a Fortran file from a C program.
    !>
    !> @param lunit - Fortran logical unit
    !> @param filepath - Path to the file we want to open.
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine open_c(lunit, filepath) bind(C, name='open_f')
      integer(c_int), value, intent(in) :: lunit
      character(kind=c_char, len=1) :: filepath

      open(lunit, file=c_f_string(filepath))
    end subroutine open_c

    !> Close a Fortran file from a C program.
    !>
    !> @param lunit - Fortran logical unit
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine close_c(lunit) bind(C, name='close_f')
      integer(c_int), value, intent(in) :: lunit

      close(unit=lunit)
    end subroutine close_c

    !> Connect a new file to the library, or initialize the
    !> library, or change verbosity associated with already-connected file.
    !>
    !> Wraps openbf() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number
    !> @param cio - cio string
    !> @param table_file_id - table_file unit number
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine openbf_c(bufr_unit, cio, table_file_id) bind(C, name='openbf_f')
      integer(c_int), value, intent(in) :: bufr_unit
      character(kind=c_char, len=1), intent(in) :: cio(*)
      integer(c_int), value, intent(in) :: table_file_id

      call openbf(bufr_unit, c_f_string(cio), table_file_id)
    end subroutine openbf_c

    !> Close a previously opened file and disconnect it from the library.
    !>
    !> Wraps closbf() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to close
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine closbf_c(bufr_unit) bind(C, name='closbf_f')
      integer(c_int), value, intent(in) :: bufr_unit

      call closbf(bufr_unit)
    end subroutine closbf_c

    !> Reset the library.
    !>
    !> Wraps exitbufr() subroutine.
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine exitbufr_c() bind(C, name='exitbufr_f')
      call exitbufr()
    end subroutine exitbufr_c

    !> Read the next message from a BUFR file.
    !>
    !> Wraps ireadmg() function.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_subset - Subset string
    !> @param iddate - Datetime of message
    !> @param subset_str_len - Length of the subset string
    !>
    !> @return ireadmg_c - Return code:
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

      ires = ireadmg(int(bufr_unit), f_subset, iddate)

      if (ires == 0) then
        call copy_f_c_str(f_subset, c_subset, int(subset_str_len))
      end if
    end function ireadmg_c

    !> Read the next data subset from a BUFR message.
    !> Wraps ireadsb() function.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !>
    !> @return ireadsb_c - Return code:
    !>  - 0 new BUFR data subset was successfully read into internal arrays.
    !>  - -1 there are no more BUFR data subsets in the BUFR message associated with bufr_unit
    !>
    !> @author Ronald McLaren @date 2020-07-29
    function ireadsb_c(bufr_unit) result(ires) bind(C, name='ireadsb_f')
      integer(c_int), value, intent(in) :: bufr_unit
      integer(c_int) :: ires
      integer :: ireadsb

      ires = ireadsb(int(bufr_unit))
    end function ireadsb_c

    !> Read/write one or more data values from/to a data subset.
    !>
    !> Wraps ufbint() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read or write
    !> @param iret - Return value, length of data read
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine ufbint_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbint_f')
      integer(c_int), value, intent(in) :: bufr_unit
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), value, intent(in) :: dim_1, dim_2
      integer(c_int), intent(out) :: iret
      character(kind=c_char, len=1), intent(in) :: table_b_mnemonic(*)
      real, pointer :: f_data

      call c_f_pointer(c_data, f_data)
      call ufbint(bufr_unit, f_data, dim_1, dim_2, iret, c_f_string(table_b_mnemonic))
    end subroutine ufbint_c

    !> Read/write one or more data values from/to a data subset.
    !>
    !> Wraps ufbrep() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read or write
    !> @param iret - Return value, length of data read
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine ufbrep_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbrep_f')
      integer(c_int), value, intent(in) :: bufr_unit
      type(c_ptr), intent(inout) :: c_data
      integer(c_int), value, intent(in) :: dim_1, dim_2
      integer(c_int), intent(out) :: iret
      character(kind=c_char, len=1), intent(in) :: table_b_mnemonic(*)
      real, pointer :: f_data

      call c_f_pointer(c_data, f_data)
      call ufbrep(bufr_unit, f_data, dim_1, dim_2, iret, c_f_string(table_b_mnemonic))
    end subroutine ufbrep_c

    !> Specify location of master BUFR tables on local file system.
    !>
    !> Wraps mtinfo() subroutine.
    !>
    !> @param path - Path where the WMO tables are stored
    !> @param file_unit_1 - Number to use for first logical unit
    !> @param file_unit_2 - Number to use for second logical unit
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine mtinfo_c(path, file_unit_1, file_unit_2) bind(C, name='mtinfo_f')
      character(kind=c_char, len=1), intent(in) :: path(*)
      integer(c_int), value, intent(in) :: file_unit_1
      integer(c_int), value, intent(in) :: file_unit_2

      call mtinfo(c_f_string(path), file_unit_1, file_unit_2)
    end subroutine mtinfo_c

    !> Check whether a file is connected to the library.
    !>
    !> Wraps status() subroutine.
    !>
    !> @param file_unit - Fortran logical unit number of file.
    !> @param lun - File ID.
    !> @param il - File status.
    !> @param im - Message status.
    !>
    !> @author Ronald McLaren  @date 2022-03-23
    subroutine status_c(file_unit, lun, il, im) bind(C, name='status_f')
      integer(c_int), value, intent(in) :: file_unit
      integer(c_int), intent(out) :: lun
      integer(c_int), intent(out) :: il
      integer(c_int), intent(out) :: im

      call status(file_unit, lun, il, im)
    end subroutine status_c

    !> Get the element name and units associated with a Table B mnemonic.
    !>
    !> Wraps nemdefs() subroutine.
    !>
    !> @param file_unit - Fortran logical unit for the open file.
    !> @param mnemonic - Mnemonic.
    !> @param unit_c - Unit string.
    !> @param unit_str_len - Unit string length.
    !> @param desc_c - Description string.
    !> @param desc_str_len - Description string length.
    !> @param iret - Return value. 0 indicates success -1 indicates failure.
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

      character(len=25) :: unit_f
      character(len=55) :: desc_f

      ! Get the unit and description strings
      call nemdefs ( file_unit, c_f_string(mnemonic), desc_f, unit_f, iret)

      if (iret == 0) then
        ! Copy the unit Fortran string into the resulting C-style string.
        call copy_f_c_str(unit_f, unit_c, min(len(unit_f), unit_str_len))
        ! Copy the descriptor Fortran string into the resulting C-style string.
        call copy_f_c_str(desc_f, desc_c, min(len(desc_f), desc_str_len))
      end if
    end subroutine nemdefs_c

    !> Get the scale factor, reference value and bit width associated with a specified occurrence of
    !> a Table B mnemonic.
    !>
    !> Wraps nemspecs() subroutine.
    !>
    !> @param file_unit - Fortran logical unit for the open file.
    !> @param mnemonic - Mnemonic.
    !> @param mnemonic_idx - Ordinal indicator of specific mnemonic element (if repeated).
    !> @param scale - Scale of element.
    !> @param reference - Reference value of element.
    !> @param bits - Number of bits representing the element.
    !> @param iret - Return value. 0 indicates success -1 indicates failure.
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

    !> Get information about a descriptor.
    !>
    !> Wraps nemtab() subroutine.
    !>
    !> @param lun - File ID.
    !> @param mnemonic - Mnemonic
    !> @param descriptor - The binary descriptor for the mnemonic
    !> @param table_type - Type of internal DX BUFR table ('B', 'C', or 'D').
    !> @param table_idx - The table index, or 0 if not found
    !>
    !> @author Ronald McLaren  @date 2022-08-16
    subroutine nemtab_c(lun, mnemonic, descriptor, table_type, table_idx) &
            bind(C, name='nemtab_f')
      integer(c_int), value, intent(in) :: lun
      character(kind=c_char,len=1), intent(in) :: mnemonic(*)
      integer(c_int), intent(out) :: descriptor
      character(kind=c_char,len=1), intent(out) :: table_type(*)
      integer(c_int), intent(out) :: table_idx

      character(len=1) :: table_type_f

      call nemtab(lun, c_f_string(mnemonic), descriptor, table_type_f, table_idx)

      table_type(1)(1:1) = table_type_f(1:1)
    end subroutine nemtab_c

    !> Get information about a Table B descriptor.
    !>
    !> Wraps nemtbb() subroutine.
    !>
    !> @param lun - File ID.
    !> @param table_idx - Table B index.
    !> @param unit_str - Unit string.
    !> @param unit_str_len - Unit string length.
    !> @param scale - Scale of element.
    !> @param reference - Reference value of element.
    !> @param bits - Number of bits representing the element.
    !>
    !> @author Ronald McLaren @date 2022-08-16
    subroutine nemtbb_c(lun, table_idx, unit_str, unit_str_len, scale, reference, bits) &
            bind(C, name='nemtbb_f')
      integer(c_int), intent(in), value :: lun
      integer(c_int), intent(in), value :: table_idx
      character(kind=c_char,len=1), intent(out) :: unit_str(*)
      integer(c_int), intent(in), value :: unit_str_len
      integer(c_int), intent(out) :: scale
      integer(c_int), intent(out) :: reference
      integer(c_int), intent(out) :: bits

      character(len=25) :: unit_str_f

      ! Get the scale, reference and bits
      call nemtbb( lun, table_idx, unit_str_f, scale, reference, bits)
      call copy_f_c_str(unit_str_f, unit_str, min(len(unit_str_f), unit_str_len))
    end subroutine nemtbb_c

    !> Get copy of the moda_tables ISC array.
    !>
    !> @param isc_ptr - C-style pointer to the ISC array
    !> @param isc_size - Size of the ISC array
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
    !> @param link_ptr - C-style pointer to the LINK array
    !> @param link_size - Size of the LINK array
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
    !> @param itp_ptr - C-style pointer to the ITP array
    !> @param itp_size - Size of the ITP array
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
    !> @param typ_ptr - C-style pointer to the TYP array
    !> @param typ_len - Size of each string within the TYP array
    !> @param mem_size - Size of the TYP array
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
    !> @param tag_ptr - C-style pointer to the TAG array
    !> @param tag_len - Length of the tag string
    !> @param mem_size - Size of TAG array
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
    !> @param jmpb_ptr - C-style pointer to the JMPB array
    !> @param jmpb_size - Length of the array
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
    !> @param irf_ptr - C-style pointer to the IRF array
    !> @param irf_size - Length of the array
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
    !> @param lun - File ID.
    !> @param start_node - The start node of the subset
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
    !> @param lun - File ID.
    !> @param num_nodes - number of values in the subset
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
    !> @param lun - File ID.
    !> @param val_ptr - C-style pointer to the VAL array
    !> @param val_size - Length of the array
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
    !> @param lun - File ID.
    !> @param inv_ptr - C-style pointer to the INV array
    !> @param inv_size - Length of the array
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

    !> Function used to get long strings from the BUFR file.
    !>
    !> @param lunit - Fortran logical unit.
    !> @param str_id - Mnemonic for the string for the source field plus the index number
    !>                 (ex: 'IDMN#2')
    !> @param output_str - The pre-allocated result string
    !> @param output_str_len - Size of the result string buffer
    !>
    !> @author Ronald McLaren @date 2023-07-03
    subroutine readlc_c(lunit, str_id, output_str, output_str_len) bind(C, name='readlc_f')
      use moda_rlccmn
      integer(c_int), value, intent(in) :: lunit
      character(kind=c_char, len=1), intent(in) :: str_id(*)
      character(kind=c_char, len=1), intent(out) :: output_str(*)
      integer(c_int), intent(in), value :: output_str_len

      character(len=120) :: output_str_f
      integer :: output_str_len_f

      call readlc(lunit, output_str_f, c_f_string(str_id))
      output_str_len_f = len(trim(output_str_f)) + 1  ! add 1 for the null terminator
      call copy_f_c_str(output_str_f, output_str, min(output_str_len_f, output_str_len))
    end subroutine readlc_c

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

    !> Read a data value from Section 0 or Section 1 of a BUFR message.
    !>
    !> Wraps iupbs01() function.
    !>
    !> @param bufr - BUFR message.
    !> @param mnemonic - Value to be read from Section 0 or Section 1
    !> of BUFR message.
    !>
    !> @return iupbs01_c - Value corresponding to mnemonic, or -1 if
    !> not found or other error occurred.
    !>
    !> @author J. Ator @date 2023-04-07
    function iupbs01_c(bufr, mnemonic) result(ires) bind(C, name='iupbs01_f')
      integer(c_int), intent(in) :: bufr(*)
      character(kind=c_char, len=1), intent(in) :: mnemonic(*)
      integer(c_int) :: ires
      integer :: iupbs01

      ires = iupbs01(bufr,c_f_string(mnemonic))
    end function iupbs01_c

    !> Get the current value of a parameter.
    !>
    !> Wraps igetprm() function.
    !>
    !> @param cprmnm - Parameter.
    !>
    !> @return igetprm_c - Value corresponding to cprmnm, or -1 if
    !> not found or other error occurred.
    !>
    !> @author J. Ator @date 2023-04-07
    function igetprm_c(cprmnm) result(ires) bind(C, name='igetprm_f')
      character(kind=c_char, len=1), intent(in) :: cprmnm(*)
      integer(c_int) :: ires
      integer :: igetprm

      ires = igetprm(c_f_string(cprmnm))
    end function igetprm_c

    !> Define a customized parameter value for dynamic allocation.
    !>
    !> Wraps isetprm() function.
    !>
    !> @param cprmnm - Parameter.
    !> @param ipval - Value to be set for cprmnm.
    !>
    !> @return isetprm_c - 0 if successful, or -1 if cprmnm unknown.
    !>
    !> @author J. Ator @date 2023-04-07
    function isetprm_c(cprmnm,ipval) result(ires) bind(C, name='isetprm_f')
      character(kind=c_char, len=1), intent(in) :: cprmnm(*)
      integer(c_int), value, intent(in) :: ipval
      integer(c_int) :: ires
      integer :: isetprm
      ires = isetprm(c_f_string(cprmnm),int(ipval))
    end function isetprm_c

    !> Define a customized maximum length for output BUFR messages.
    !>
    !> Wraps maxout() subroutine.
    !>
    !> @param max0 - New maximum length (in bytes) for all BUFR messages
    !> written to all output files.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine maxout_c(max0) bind(C, name='maxout_f')
      integer(c_int), value, intent(in) :: max0

      call maxout(max0)
    end subroutine maxout_c

    !> Get the maximum length of a BUFR message that can be written to an
    !> output file.
    !>
    !> Wraps igetmxby() function.
    !>
    !> @return igetmxby_c - Maximum length of a BUFR message that can be
    !> written to an output file.
    !>
    !> @author J. Ator @date 2023-04-07
    function igetmxby_c() result(ires) bind(C, name='igetmxby_f')
      integer(c_int) :: ires
      integer :: igetmxby

      ires = igetmxby()
    end function igetmxby_c

    !> Convert an FXY value from its WMO bit-wise representation to its
    !> six-character representation.
    !>
    !> Wraps cadn30() function.
    !>
    !> @param idn - WMO bit-wise representation of FXY value.
    !> @param adn - FXY value.
    !> @param adn_str_len - Length of adn string.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine cadn30_c(idn, adn, adn_str_len) bind(C, name='cadn30_f')
      integer(c_int), intent(in), value :: idn, adn_str_len
      character(kind=c_char, len=1), intent(out) :: adn(*)
      character(len=8) :: adn_f

      call cadn30(idn, adn_f)
      call copy_f_c_str(adn_f, adn, adn_str_len)
    end subroutine cadn30_c

    !> Get the next index for storing an entry within an internal DX BUFR table.
    !>
    !> Wraps igetntbi() subroutine.
    !>
    !> @param lun - File ID.
    !> @param table_type - Type of internal DX BUFR table ('A', 'B', or 'D')
    !>
    !> @return igetntbi_c - Next available index within table_type.
    !>
    !> @author Ronald McLaren  @date 2022-08-16
    function igetntbi_c(lun, table_type) result(ires) bind(C, name='igetntbi_f')
      integer(c_int), value, intent(in) :: lun
      character(kind=c_char,len=1), intent(in) :: table_type(*)
      integer(c_int) :: ires
      integer :: igetntbi
      character(len=1) :: table_type_f

      table_type_f(1:1) = table_type(1)(1:1)

      ires = igetntbi(int(lun), table_type_f)
    end function igetntbi_c

    !> Decode the scale factor, reference value, bit width, and units from a Table B
    !> mnemonic definition.
    !>
    !> Wraps elemdx() subroutine.
    !>
    !> @param card - Mnemonic definition card.
    !> @param lun - File ID.
    !>
    !> @author J. Ator @date 2003-11-04
    subroutine elemdx_c(card,lun) bind(C, name='elemdx_f')
      integer(c_int), value, intent(in) :: lun
      character(kind=c_char, len=1), intent(in) :: card(*)
      character(len=80) :: card_f
      integer :: ii

      do ii = 1,80
        card_f(ii:ii) = card(1)(ii:ii)
      enddo
      call elemdx(card_f, lun)
    end subroutine elemdx_c

    !> Search for a Table B or Table D descriptor within the internal DX BUFR tables.
    !>
    !> Wraps numtbd() subroutine.
    !>
    !> @param lun - File ID.
    !> @param idn - WMO bit-wise representation of FXY value.
    !> @param nemo - Mnemonic.
    !> @param nemo_str_len - Length of nemo string.
    !> @param tab - Type of internal DX BUFR table ('B', or 'D').
    !> @param iret - Positional index of idn within Table B or D, or 0 if not found.
    !>
    !> @author J. Ator  @date 2003-11-04
    subroutine numtbd_c(lun,idn,nemo,nemo_str_len,tab,iret) bind(C, name='numtbd_f')
      integer(c_int), value, intent(in) :: lun, idn, nemo_str_len
      character(kind=c_char,len=1), intent(out) :: nemo(*), tab(*)
      integer(c_int), intent(out) :: iret

      character(len=9) :: nemo_f
      character(len=1) :: tab_f

      call numtbd(lun, idn, nemo_f, tab_f, iret)

      call copy_f_c_str(nemo_f, nemo, nemo_str_len)
      tab(1)(1:1) = tab_f(1:1)
    end subroutine numtbd_c

    !> Convert an FXY value from its 6 character representation to its WMO bit-wise
    !> representation.
    !>
    !> Wraps ifxy() function.
    !>
    !> @param cfxy - FXY value.
    !>
    !> @return ifxy_c - WMO bit-wise representation of FXY value.
    !>
    !> @author J. Ator @date 2023-04-07
    function ifxy_c(cfxy) result(ires) bind(C, name='ifxy_f')
      character(kind=c_char, len=1), intent(in) :: cfxy(*)
      integer(c_int) :: ires
      integer :: ifxy

      ires = ifxy(c_f_string(cfxy))
    end function ifxy_c

    !> Get the WMO bit-wise representation of the FXY value corresponding
    !> to a child mnemonic of a Table D sequence.
    !>
    !> Wraps uptdd() subroutine.
    !>
    !> @param id - Positional index of parent mnemonic within internal
    !> BUFR Table D array.
    !> @param lun - File ID.
    !> @param ient - Ordinal indicator of child mnemonic to return from
    !> within parent sequence; set to 0 to request a count of the total
    !> number of child mnemonics.
    !> @param iret - Total number of child mnemonics if ient = 0; otherwise
    !> the WMO bit-wise representation of the FXY value corresponding to
    !> the ient'th mnemonic.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine uptdd_c(id, lun, ient, iret) bind(C, name='uptdd_f')
      integer(c_int), intent(in), value :: id, lun, ient
      integer(c_int), intent(out) :: iret

      call uptdd(id, lun, ient, iret)
    end subroutine uptdd_c

    !> Check whether a specified mnemonic is a Table C marker operator.
    !>
    !> Wraps imrkopr() function.
    !>
    !> @param nemo - Mnemonic.
    !>
    !> @return imrkopr_c - 1 if nemo is a Table C marker operator, else 0.
    !>
    !> @author J. Ator @date 2023-04-07
    function imrkopr_c(nemo) result(ires) bind(C, name='imrkopr_f')
      character(kind=c_char, len=1), intent(in) :: nemo(*)
      integer(c_int) :: ires
      integer :: imrkopr

      ires = imrkopr(c_f_string(nemo))
    end function imrkopr_c

    !> Check whether a descriptor is WMO-standard.
    !>
    !> Wraps istdesc() function.
    !>
    !> @param idn - WMO bit-wise representation of FXY value for descriptor.
    !>
    !> @return istdesc_c - 1 if idn is WMO-standard, else 0.
    !>
    !> @author J. Ator @date 2023-04-07
    function istdesc_c(idn) result(ires) bind(C, name='istdesc_f')
      integer(c_int), intent(in), value :: idn
      integer(c_int) :: ires
      integer :: istdesc

      ires = istdesc(idn)
    end function istdesc_c

    !> Read/write an entire sequence of data values from/to a data subset.
    !>
    !> Wraps ufbseq() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read or write
    !> @param iret - Return value, length of data read
    !> @param table_d_mnemonic - Table A or Table D mnemonic.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine ufbseq_c(bufr_unit, c_data, dim_1, dim_2, iret, table_d_mnemonic) bind(C, name='ufbseq_f')
      integer(c_int), value, intent(in) :: bufr_unit
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), value, intent(in) :: dim_1, dim_2
      integer(c_int), intent(out) :: iret
      character(kind=c_char, len=1), intent(in) :: table_d_mnemonic(*)
      real, pointer :: f_data

      call c_f_pointer(c_data, f_data)
      call ufbseq(bufr_unit, f_data, dim_1, dim_2, iret, c_f_string(table_d_mnemonic))
    end subroutine ufbseq_c

    !> Read the next data subset from a BUFR file.
    !>
    !> Wraps ireadns() function.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from.
    !> @param c_subset - Subset string.
    !> @param iddate - Datetime of message.
    !> @param subset_str_len - Length of the subset string.
    !>
    !> @return ireadns_c - Return code:
    !>  - 0 new BUFR data subset was successfully read into internal arrays.
    !>  - -1 there are no more BUFR data subsets in bufr_unit.
    !>
    !> @author J. Ator @date 2023-04-07
    function ireadns_c(bufr_unit, c_subset, iddate, subset_str_len) result(ires) bind(C, name='ireadns_f')
      integer(c_int), value, intent(in) :: bufr_unit
      character(kind=c_char, len=1), intent(out) :: c_subset(*)
      integer(c_int), intent(out) :: iddate
      integer(c_int), value, intent(in) :: subset_str_len
      integer(c_int) :: ires
      character(len=25) :: f_subset
      integer :: ireadns

      ires = ireadns(int(bufr_unit), f_subset, iddate)

      if (ires == 0) then
        call copy_f_c_str(f_subset, c_subset, subset_str_len)
      end if
    end function ireadns_c

    !> Test whether a data value is "missing".
    !>
    !> Wraps ibfms() function.
    !>
    !> @param r8val - Data value.
    !>
    !> @return ibfms_c - 1 if r8val is "missing", or 0 otherwise.
    !>
    !> @author J. Ator @date 2023-04-07
    function ibfms_c(r8val) result(ires) bind(C, name='ibfms_f')
      real(c_double), intent(in), value :: r8val
      integer(c_int) :: ires
      integer :: ibfms

      ires = ibfms(real(r8val,kind=8))
    end function ibfms_c

    !> Decode an integer from a character string.
    !>
    !> Wraps strnum() subroutine.
    !>
    !> @param str - String.
    !> @param num - Value decoded from str.
    !> @param iret - 0 if successful, -1 otherwise.
    !>
    !> @author J. Ator @date 2003-11-04
    subroutine strnum_c(str,num,iret) bind(C, name='strnum_f')
      character(kind=c_char, len=1), intent(in) :: str(*)
      integer(c_int), intent(out) :: num, iret

      call strnum(c_f_string(str), num, iret)
    end subroutine strnum_c

    !> Store a new entry within the internal BUFR Table B or D.
    !>
    !> Wraps stntbi() subroutine.
    !>
    !> @param n - Storage index into internal Table B or D.
    !> @param lun - File ID.
    !> @param numb - FXY number for new entry.
    !> @param nemo - Mnemonic corresponding to numb.
    !> @param celsq - Element or sequence description corresponding to numb.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine stntbi_c(n,lun,numb,nemo,celsq) bind(C, name='stntbi_f')
      integer(c_int), intent(in), value :: n, lun
      character(kind=c_char, len=1), intent(in) :: numb(*), nemo(*), celsq(*)
      character(len=6) :: numb_f
      character(len=8) :: nemo_f
      character(len=55) :: celsq_f
      integer :: ii

      do ii = 1,6
        numb_f(ii:ii) = numb(1)(ii:ii)
      enddo
      do ii = 1,8
        nemo_f(ii:ii) = nemo(1)(ii:ii)
      enddo
      do ii = 1,55
        celsq_f(ii:ii) = celsq(1)(ii:ii)
      enddo
      call stntbi(n, lun, numb_f, nemo_f, celsq_f)
    end subroutine stntbi_c

    !> Get the next usable Table D index for the current master table, or
    !> reset the index.
    !>
    !> Wraps igettdi() function.
    !>
    !> @param iflag - if 0, will reset the index.
    !>
    !> @return igettdi_c - -1 if iflag=0, otherwise the next usable index.
    !>
    !> @author J. Ator @date 2023-04-07
    function igettdi_c(iflag) result(ires) bind(C, name='igettdi_f')
      integer(c_int), intent(in), value :: iflag
      integer(c_int) :: ires
      integer :: igettdi

      ires = igettdi(int(iflag))
    end function igettdi_c

    !> Store information about a child mnemonic within the internal arrays.
    !>
    !> Wraps pktdd() subroutine.
    !>
    !> @param id - Index of parent mnemonic within internal BUFR Table D array.
    !> @param lun - File ID.
    !> @param idn - WMO bit-wise representation of FXY value corresponding to child
    !> mnemonic; set to 0 to delete all child mnemonic information.
    !> @param iret - 0 if idn=0; -1 if error occurred; otherwise, the total number of
    !> child mnemonics stored so far for parent mnemonic id.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine pktdd_c(id, lun, idn, iret) bind(C, name='pktdd_f')
      integer(c_int), intent(in), value :: id, lun, idn
      integer(c_int), intent(out) :: iret

      call pktdd(id, lun, idn, iret)
    end subroutine pktdd_c

    !> Log one error message and abort application program.
    !>
    !> Wraps bort() subroutine.
    !>
    !> @param errstr - Error message.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine bort_c(errstr) bind(C, name='bort_f')
      character(kind=c_char, len=1), intent(in) :: errstr(*)

      call bort(c_f_string(errstr))
    end subroutine bort_c

    !> Open a new message for output in a BUFR file that was
    !> previously opened for writing.
    !>
    !> Wraps openmb() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to write to.
    !> @param c_subset - Table A mnemonic of message.
    !> @param iddate - Date-time to be stored within Section 1 of message.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine openmb_c(bufr_unit, c_subset, iddate) bind(C, name='openmb_f')
      integer(c_int), value, intent(in) :: bufr_unit, iddate
      character(kind=c_char, len=1), intent(in) :: c_subset(*)

      call openmb(bufr_unit, c_f_string(c_subset), iddate)
    end subroutine openmb_c

    !> Get the version number of the NCEPLIBS-bufr software.
    !>
    !> Wraps bvers() subroutine.
    !>
    !> @param cverstr - Version string.
    !> @param cverstr_len - Length of the version string.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine bvers_c(cverstr, cverstr_len) bind(C, name='bvers_f')
      character(kind=c_char, len=1), intent(out) :: cverstr(*)
      integer(c_int), value, intent(in) :: cverstr_len
      character(len=10) :: f_cverstr

      call bvers(f_cverstr)
      call copy_f_c_str(f_cverstr, cverstr, cverstr_len)
    end subroutine bvers_c

    !> Determine important information about the local machine.
    !>
    !> Wraps wrdlen() subroutine.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine wrdlen_c() bind(C, name='wrdlen_f')
      call wrdlen()
    end subroutine wrdlen_c

    !> Decode an integer value from an integer array.
    !>
    !> Wraps iupb() function.
    !>
    !> @param mbay - Array containing encoded value.
    !> @param nbyt - Byte within mbay at whose first bit to begin decoding.
    !> @param nbit - Number of bits to decode.
    !>
    !> @return iupb_c - Decoded value.
    !>
    !> @author J. Ator @date 2023-04-07
    function iupb_c(mbay,nbyt,nbit) result(ires) bind(C, name='iupb_f')
      integer(c_int), intent(in) :: mbay(*)
      integer(c_int), intent(in), value :: nbyt, nbit
      integer(c_int) :: ires
      integer :: iupb

      ires = iupb(mbay,nbyt,nbit)
    end function iupb_c

    !> Specify the use of compression when writing BUFR messages.
    !>
    !> Wraps cmpmsg() subroutine.
    !>
    !> @param cf - Flag indicating whether future BUFR output messages are to be
    !> compressed ('Y' = Yes, 'N' = No).
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine cmpmsg_c(cf) bind(C, name='cmpmsg_f')
      character(kind=c_char, len=1), intent(in) :: cf(*)

      call cmpmsg(c_f_string(cf))
    end subroutine cmpmsg_c

end module bufr_c2f_interface
