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
  public :: open_c, close_c, openbf_c, closbf_c, exitbufr_c, bort_c, readmg_c, readsb_c, readns_c, ireadmg_c, ireadsb_c
  public :: ireadns_c, openmb_c, openmg_c, ufbint_c, ufbrep_c, ufbseq_c, mtinfo_c, bvers_c, status_c, ibfms_c
  public :: get_isc_c, get_link_c, get_itp_c, get_typ_c, get_tag_c, get_jmpb_c, get_inode_c, get_nval_c, get_val_c
  public :: get_inv_c, get_irf_c, readlc_c, delete_table_data_c, cmpmsg_c, catch_borts_c, check_for_bort_c, iupbs01_c
  public :: imrkopr_c, istdesc_c, ifxy_c, igetntbi_c, igettdi_c, stntbi_c, igetprm_c, isetprm_c, maxout_c, igetmxby_c
  public :: elemdx_c, cadn30_c, strnum_c, uptdd_c, pktdd_c, nemdefs_c, nemspecs_c, nemtab_c, nemtbb_c, numtbd_c
  public :: writsb_c, writsa_c, ufbstp_c, writlc_c, drfini_c, ufbcnt_c, ufbevn_c, ufbqcd_c, ufbqcp_c, getcfmng_c
  public :: upftbv_c, ufbtab_c, ufbpos_c, datelen_c, iupvs01_c, nmsub_c, pkvs01_c, datebf_c, dumpbf_c, minimg_c, upds3_c
  public :: pkbs1_c, strcpt_c, rtrcpt_c, atrcpt_c, dxdump_c, ufbdmp_c, ufdump_c, copybf_c, copymg_c, copysb_c, ufbcpy_c
  public :: readerme_c, rdmgsb_c, ufbmem_c, ufbmex_c, ufbmms_c, ufbmns_c, rdmemm_c, rdmems_c, ufbrms_c, ufbtam_c
  public :: cpymem_c, ufbcup_c, stdmsg_c, stndrd_c, codflg_c, gettagpr_c, gettagre_c, cnved4_c, lcmgdf_c
  public :: setvalnb_c, getvalnb_c, getabdb_c, ufbget_c, ufbinx_c, ufbovr_c, closmg_c, ifbget_c, igetsc_c
  public :: wrdxtb_c, mesgbf_c, mesgbc_c, invmrg_c, ipkm_c, iupm_c, dealloc_vars_c

  integer, allocatable, target, save :: isc_f(:), link_f(:), itp_f(:), jmpb_f(:), irf_f(:)
  character(len=10), allocatable, target, save :: tag_f(:)
  character(len=3), allocatable, target, save :: typ_f(:)

  character*(:), allocatable, save :: bvers_fstr_outer, bvers_fstr_inner
  character*(:), allocatable, save :: readlc_fchr_outer, readlc_fchr_inner
  character*(:), allocatable, save :: getcfmng_cmng_outer, getcfmng_cmng_inner
  character*128, allocatable, save :: getabdb_tabdb_outer(:), getabdb_tabdb_inner(:)
  character*(:), allocatable, save :: writlc_fchr_outer, writlc_fchr_inner
  character*6, allocatable, save :: upds3_cds3_outer(:), upds3_cds3_inner(:)

  contains

    !> Count the number of characters in a C string
    !>
    !> @param c_str - Pointer to a null-terminated C string
    !>
    !> @return nchars - Number of characters in c_str
    !>
    !> @author Jeff Ator @date 2025-09-05
    function get_c_string_length(c_str) result(nchars)
      character(kind=c_char, len=1), intent(in) :: c_str(*)
      integer :: nchars

      nchars = 1
      do while (c_str(nchars) /= c_null_char)
        nchars = nchars + 1
      end do
      nchars = nchars - 1
    end function get_c_string_length

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
      character(kind=c_char, len=1), intent(in) :: c_str(*)
      character(len=:), allocatable :: f_str
      integer :: nchars

      nchars = get_c_string_length(c_str)

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
      character(len=*), intent(in) :: f_str
      character(kind=c_char), intent(inout) :: c_str(*)
      integer, intent(in) :: c_str_len
      integer :: ii

      if (c_str_len /= 0) then
        if (c_str_len > 1) then
          do ii = 1, c_str_len-1
            c_str(ii) = f_str(ii:ii)
          enddo
        end if
        c_str(c_str_len) = c_null_char
      end if
    end subroutine copy_f_c_str

    !> Copy an array of fixed-length Fortran strings into an array of C strings.
    !>
    !> @param f_arr - Fortran string array to be copied.
    !> @param c_arr - C string array to be copied into.
    !> @param lenstr - Length of each string.
    !> @param numstr - Number of strings to be copied.
    !>
    !> @author Jeff Ator @date 2026-02-23
    subroutine copy_f_c_str_arr(f_arr, c_arr, lenstr, numstr)
      integer, intent(in) :: lenstr, numstr
      character*(lenstr), intent(in) :: f_arr(*)
      character(kind=c_char), intent(out) :: c_arr(lenstr,*)
      integer :: ii, jj

      if (numstr > 0) then
        do jj = 1, numstr
          do ii = 1, lenstr
            c_arr(ii,jj) = f_arr(jj)(ii:ii)
          enddo
        enddo
      endif
    end subroutine copy_f_c_str_arr

    !> Deallocate one or more previously-allocated local variables.
    !>
    !> This subroutine is called from C immediately following a caught bort error, in order to
    !> explicitly deallocate any Fortran memory within subpname that otherwise wouldn't get
    !> deallocated because of the direct jump to the bort target location.
    !>
    !> @param subpname - Name of local routine for which to deallocate variables
    !>
    !> @author Jeff Ator @date 2026-01-28
    subroutine dealloc_vars_c(subpname) bind(C, name='dealloc_vars_f')
      character(kind=c_char), intent(in) :: subpname(*)

      select case (c_f_string(subpname))
        ! Explicitly deallocate the inner-most allocated variables within subpname.
        ! These will be the "inner" variables if a previous call was directly made to
        ! subpname from a C application program; otherwise, it will be the "outer"
        ! variables.  In any case, the inner-most variables will always be the
        ! most-recently allocated ones, so they're the ones which will always be
        ! active when a bort error is caught, and therefore the only ones which should
        ! ever be explicitly deallocated within this subroutine.
        case ('bvers_f')
          if (allocated(bvers_fstr_inner)) then
            deallocate(bvers_fstr_inner)
          else
            deallocate(bvers_fstr_outer)
          end if
        case ('readlc_f')
          if (allocated(readlc_fchr_inner)) then
            deallocate(readlc_fchr_inner)
          else
            deallocate(readlc_fchr_outer)
          end if
        case ('getcfmng_f')
          if (allocated(getcfmng_cmng_inner)) then
            deallocate(getcfmng_cmng_inner)
          else
            deallocate(getcfmng_cmng_outer)
          end if
        case ('getabdb_f')
          if (allocated(getabdb_tabdb_inner)) then
            deallocate(getabdb_tabdb_inner)
          else
            deallocate(getabdb_tabdb_outer)
          end if
        case ('writlc_f')
          ! It is possible for writlc_f to have previously called writlc without
          ! allocating any memory, so we need to explicitly check for that.
          if (allocated(writlc_fchr_inner)) then
            deallocate(writlc_fchr_inner)
          else if (allocated(writlc_fchr_outer)) then
            deallocate(writlc_fchr_outer)
          end if
        case ('upds3_f')
          if (allocated(upds3_cds3_inner)) then
            deallocate(upds3_cds3_inner)
          else
            deallocate(upds3_cds3_outer)
          end if
      end select
    end subroutine dealloc_vars_c

    !> Open a Fortran file from a C program.
    !>
    !> @param lunit - Fortran logical unit
    !> @param filepath - Path to the file we want to open.
    !>
    !> @author Ronald McLaren @date 2020-07-29
    subroutine open_c(lunit, filepath) bind(C, name='open_f')
      integer(c_int), value, intent(in) :: lunit
      character(kind=c_char) :: filepath

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
    recursive subroutine openbf_c(bufr_unit, cio, table_file_id) bind(C, name='openbf_f')
      integer(c_int), value, intent(in) :: bufr_unit, table_file_id
      character(kind=c_char), intent(in) :: cio(*)
      character(len=5) :: io
      integer :: lio

      lio = get_c_string_length(cio)
      if (lio == 0) then
        io(1:1) = ' '
        lio = 1
      else
        io = transfer(cio(1:lio), io)
      endif
      call openbf(bufr_unit, io(1:lio), table_file_id)
    end subroutine openbf_c

    !> Close a previously opened file and disconnect it from the library.
    !>
    !> Wraps closbf() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to close
    !>
    !> @author Ronald McLaren @date 2020-07-29
    recursive subroutine closbf_c(bufr_unit) bind(C, name='closbf_f')
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
    !>  - 0 new BUFR message was successfully read into internal arrays
    !>  - -1 there are no more BUFR messages in bufr_unit
    !>
    !> @author Ronald McLaren @date 2020-07-29
    function ireadmg_c(bufr_unit, c_subset, iddate, subset_str_len) result(ires) bind(C, name='ireadmg_f')
      integer(c_int), value, intent(in) :: bufr_unit, subset_str_len
      character(kind=c_char), intent(out) :: c_subset(*)
      integer(c_int), intent(out) :: iddate
      integer(c_int) :: ires
      character(len=25) :: f_subset
      integer :: ireadmg

      ires = ireadmg(bufr_unit, f_subset, iddate)

      if (ires == 0) then
        call copy_f_c_str(f_subset, c_subset, subset_str_len)
      end if
    end function ireadmg_c

    !> Read the next message from a BUFR file.
    !>
    !> Wraps readmg() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_subset - Subset string
    !> @param iddate - Datetime of message
    !> @param subset_str_len - Length of the subset string
    !> @param ires - Return code:
    !>  - 0 new BUFR message was successfully read into internal arrays
    !>  - -1 there are no more BUFR messages in bufr_unit
    !>
    !> @author Jeff Ator @date 2025-08-25
    recursive subroutine readmg_c(bufr_unit, c_subset, iddate, subset_str_len, ires) bind(C, name='readmg_f')
      integer(c_int), value, intent(in) :: bufr_unit, subset_str_len
      character(kind=c_char), intent(out) :: c_subset(*)
      integer(c_int), intent(out) :: iddate, ires
      character(len=25) :: f_subset

      call readmg(bufr_unit, f_subset, iddate, ires)

      if (ires == 0) then
        call copy_f_c_str(f_subset, c_subset, subset_str_len)
      end if
    end subroutine readmg_c

    !> Read the next data subset from a BUFR message.
    !>
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

      ires = ireadsb(bufr_unit)
    end function ireadsb_c

    !> Read the next data subset from a BUFR message.
    !>
    !> Wraps readsb() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param ires - Return code:
    !>  - 0 new BUFR data subset was successfully read into internal arrays
    !>  - -1 there are no more BUFR data subsets in bufr_unit
    !>
    !> @author Jeff Ator @date 2025-09-05
    recursive subroutine readsb_c(bufr_unit, ires) bind(C, name='readsb_f')
      integer(c_int), value, intent(in) :: bufr_unit
      integer(c_int), intent(out) :: ires

      call readsb(bufr_unit, ires)
    end subroutine readsb_c

    !> Write the next data subset to a BUFR message.
    !>
    !> Wraps writsb() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to write to
    !>
    !> @author Jeff Ator @date 2025-10-20
    recursive subroutine writsb_c(bufr_unit) bind(C, name='writsb_f')
      integer(c_int), value, intent(in) :: bufr_unit

      call writsb(bufr_unit)
    end subroutine writsb_c

    !> Write the next data subset to a BUFR message, and return a copy of any completed message.
    !>
    !> Wraps writsa() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to write to
    !> @param bufr_len - Allocated length of bufr array
    !> @param bufr - BUFR message
    !> @param nbufr - Number of integers returned in bufr array, or 0 if no message was returned
    !>
    !> @author Jeff Ator @date 2025-10-20
    recursive subroutine writsa_c(bufr_unit, bufr_len, bufr, nbufr) bind(C, name='writsa_f')
      integer(c_int), value, intent(in) :: bufr_unit, bufr_len
      integer(c_int), intent(out) :: bufr(*), nbufr

      call writsa(bufr_unit, bufr_len, bufr, nbufr)
    end subroutine writsa_c

    !> Read/write one or more data values from/to a data subset.
    !>
    !> Wraps ufbint() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from or write to
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read or write
    !> @param iret - Return value, length of data read
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author Ronald McLaren @date 2020-07-29
    recursive subroutine ufbint_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbint_f')
      integer(c_int), value, intent(in) :: bufr_unit, dim_1, dim_2
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif
      call c_f_pointer(c_data, f_data)
      call ufbint(bufr_unit, f_data, dim_1, dim_2, iret, str(1:lstr))
    end subroutine ufbint_c

    !> Read/write one or more data values from/to a data subset.
    !>
    !> Wraps ufbrep() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from or write to
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read or write
    !> @param iret - Return value, length of data read
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author Ronald McLaren @date 2020-07-29
    recursive subroutine ufbrep_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbrep_f')
      integer(c_int), value, intent(in) :: bufr_unit, dim_1, dim_2
      type(c_ptr), intent(inout) :: c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif
      call c_f_pointer(c_data, f_data)
      call ufbrep(bufr_unit, f_data, dim_1, dim_2, iret, str(1:lstr))
    end subroutine ufbrep_c

    !> Read/write one or more data values from/to a data subset.
    !>
    !> Wraps ufbstp() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from or write to
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read or write
    !> @param iret - Return value, length of data read
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author J. Ator @date 2025-10-24
    recursive subroutine ufbstp_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbstp_f')
      integer(c_int), value, intent(in) :: bufr_unit, dim_1, dim_2
      type(c_ptr), intent(inout) :: c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif
      call c_f_pointer(c_data, f_data)
      call ufbstp(bufr_unit, f_data, dim_1, dim_2, iret, str(1:lstr))
    end subroutine ufbstp_c

    !> Read one or more data values from a data subset.
    !>
    !> Wraps ufbevn() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2, dim_3 - Dimensionality of data to read
    !> @param iret - Return value, length of data read
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author J. Ator @date 2025-11-05
    recursive subroutine ufbevn_c(bufr_unit, c_data, dim_1, dim_2, dim_3, iret, table_b_mnemonic) bind(C, name='ufbevn_f')
      integer(c_int), value, intent(in) :: bufr_unit, dim_1, dim_2, dim_3
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif
      call c_f_pointer(c_data, f_data)
      call ufbevn(bufr_unit, f_data, dim_1, dim_2, dim_3, iret, str(1:lstr))
    end subroutine ufbevn_c

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
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), value, intent(in) :: file_unit_1, file_unit_2
      character(len=:), allocatable :: mtdir
      integer :: lmtdir

      lmtdir = get_c_string_length(path)
      if (lmtdir == 0) then
        call mtinfo(' ', file_unit_1, file_unit_2)
      else
        allocate(character(len=lmtdir) :: mtdir)
        mtdir = transfer(path(1:lmtdir), mtdir)
        call mtinfo(mtdir(1:lmtdir), file_unit_1, file_unit_2)
        deallocate(mtdir)
      endif

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
    recursive subroutine status_c(file_unit, lun, il, im) bind(C, name='status_f')
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
    recursive subroutine nemdefs_c(file_unit, mnemonic, unit_c, unit_str_len, desc_c, desc_str_len, iret) &
            bind(C, name='nemdefs_f')
      integer(c_int), value, intent(in) :: file_unit, unit_str_len, desc_str_len
      character(kind=c_char), intent(in) :: mnemonic(*)
      character(kind=c_char), intent(out) :: unit_c(*), desc_c(*)
      integer(c_int), intent(out) :: iret
      character(len=25) :: unit_f
      character(len=55) :: desc_f
      character(len=10) :: tag
      integer :: ltag

      ltag = get_c_string_length(mnemonic)
      if (ltag == 0) then
        tag(1:1) = ' '
        ltag = 1
      else
        tag = transfer(mnemonic(1:ltag), tag)
      endif

      ! Get the unit and description strings
      call nemdefs ( file_unit, tag(1:ltag), desc_f, unit_f, iret)

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
    recursive subroutine nemspecs_c(file_unit, mnemonic, mnemonic_idx, scale, reference, bits, iret) &
            bind(C, name='nemspecs_f')
      integer(c_int), value, intent(in) :: file_unit, mnemonic_idx
      character(kind=c_char), intent(in) :: mnemonic(*)
      integer(c_int), intent(out) :: scale, reference, bits, iret
      character(len=10) :: tag
      integer :: ltag

      ltag = get_c_string_length(mnemonic)
      if (ltag == 0) then
        tag(1:1) = ' '
        ltag = 1
      else
        tag = transfer(mnemonic(1:ltag), tag)
      endif

      ! Get the scale, reference and bits
      call nemspecs(file_unit, tag(1:ltag), mnemonic_idx, scale, reference, bits, iret)
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
      character(kind=c_char), intent(in) :: mnemonic(*)
      integer(c_int), intent(out) :: descriptor, table_idx
      character(kind=c_char), intent(out) :: table_type(*)
      character(len=1) :: table_type_f
      character(len=10) :: tag
      integer :: ltag

      ltag = get_c_string_length(mnemonic)
      if (ltag == 0) then
        tag(1:1) = ' '
        ltag = 1
      else
        tag = transfer(mnemonic(1:ltag), tag)
      endif

      call nemtab(lun, tag(1:ltag), descriptor, table_type_f, table_idx)

      table_type(1) = table_type_f(1:1)
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
      character(kind=c_char), intent(out) :: unit_str(*)
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

    !> Get a long string from the BUFR file.
    !>
    !> @param lunit - Fortran logical unit.
    !> @param str_id - Mnemonic for the string for the source field plus the index number
    !>                 (ex: 'IDMN#2')
    !> @param cchr - The pre-allocated result string
    !> @param cchr_len - Size of the result string buffer
    !>
    !> @author Ronald McLaren @date 2023-07-03
    recursive subroutine readlc_c(lunit, str_id, cchr, cchr_len) bind(C, name='readlc_f')
      integer(c_int), value, intent(in) :: lunit, cchr_len
      character(kind=c_char), intent(in) :: str_id(*)
      character(kind=c_char), intent(out) :: cchr(*)
      character(len=14) :: str
      integer :: lchr, lstr, lallc

      lstr = get_c_string_length(str_id)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(str_id(1:lstr), str)
      endif

      ! Strings allocated within this subroutine will be for use in Fortran, so we won't need
      ! space for a trailing null and can therefore subtract 1 from cchr_len.
      lallc = cchr_len - 1

      if (lallc <= 0) then
        ! Any writeable string passed in from a C routine will always contain at least one byte
        ! for a trailing null, even if it's an empty string!
        cchr(1) = c_null_char
      else if (allocated(readlc_fchr_outer)) then
        ! A previous call was directly made to this subroutine from within a C application
        ! program with bort catching enabled.  So we now need to allocate a separate "inner"
        ! string and recursively call readlc() again with that string.
        allocate(character*(lallc) :: readlc_fchr_inner)
        call readlc(lunit, readlc_fchr_inner, str(1:lstr))
        lchr = len_trim(readlc_fchr_inner) + 1  ! add 1 for the null terminator
        call copy_f_c_str(readlc_fchr_inner, cchr, lchr)
        deallocate(readlc_fchr_inner)
      else
        allocate(character*(lallc) :: readlc_fchr_outer)
        call readlc(lunit, readlc_fchr_outer, str(1:lstr))
        lchr = len_trim(readlc_fchr_outer) + 1  ! add 1 for the null terminator
        call copy_f_c_str(readlc_fchr_outer, cchr, lchr)
        deallocate(readlc_fchr_outer)
      end if
    end subroutine readlc_c

    !> Write a long string to the BUFR file.
    !>
    !> @param lunit - Fortran logical unit.
    !> @param str - Mnemonic for the string for the source field plus the index number
    !>                 (ex: 'IDMN#2')
    !> @param chr - Value corresponding to str
    !>
    !> @author Jeff Ator @date 2025-10-24
    recursive subroutine writlc_c(lunit, str, chr) bind(C, name='writlc_f')
      integer(c_int), value, intent(in) :: lunit
      character(kind=c_char), intent(in) :: str(*), chr(*)
      character(len=14) :: my_str
      integer :: lstr, lchr

      lstr = get_c_string_length(str)
      if (lstr == 0) then
        my_str(1:1) = ' '
        lstr = 1
      else
        my_str = transfer(str(1:lstr), my_str)
      endif

      lchr = get_c_string_length(chr)
      if (lchr == 0) then
        call writlc(lunit, ' ', my_str(1:lstr))
      else if (allocated(writlc_fchr_outer)) then
        ! A previous call was directly made to this subroutine from within a C application
        ! program with bort catching enabled.  So we now need to allocate a separate "inner"
        ! string and recursively call writlc() again with that string.
        allocate(character(len=lchr) :: writlc_fchr_inner)
        writlc_fchr_inner = transfer(chr(1:lchr), writlc_fchr_inner)
        call writlc(lunit, writlc_fchr_inner(1:lchr), my_str(1:lstr))
        deallocate(writlc_fchr_inner)
      else
        allocate(character(len=lchr) :: writlc_fchr_outer)
        writlc_fchr_outer = transfer(chr(1:lchr), writlc_fchr_outer)
        call writlc(lunit, writlc_fchr_outer(1:lchr), my_str(1:lstr))
        deallocate(writlc_fchr_outer)
      endif
    end subroutine writlc_c

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
      character(kind=c_char), intent(in) :: mnemonic(*)
      integer(c_int) :: ires
      integer :: iupbs01, ltag
      character(len=10) :: tag

      ltag = get_c_string_length(mnemonic)
      if (ltag == 0) then
        tag(1:1) = ' '
        ltag = 1
      else
        tag = transfer(mnemonic(1:ltag), tag)
      endif

      ires = iupbs01(bufr,tag(1:ltag))
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
      character(kind=c_char), intent(in) :: cprmnm(*)
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
      character(kind=c_char), intent(in) :: cprmnm(*)
      integer(c_int), value, intent(in) :: ipval
      integer(c_int) :: ires
      integer :: isetprm

      ires = isetprm(c_f_string(cprmnm),ipval)
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
      character(kind=c_char), intent(out) :: adn(*)
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
      character(kind=c_char), intent(in) :: table_type(*)
      integer(c_int) :: ires
      integer :: igetntbi
      character(len=1) :: table_type_f

      table_type_f(1:1) = table_type(1)(1:1)

      ires = igetntbi(lun, table_type_f)
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
      character(kind=c_char), intent(in) :: card(*)
      character(len=80) :: card_f
      integer :: ii

      do ii = 1,80
        card_f(ii:ii) = card(ii)
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
      character(kind=c_char), intent(out) :: nemo(*), tab(*)
      integer(c_int), intent(out) :: iret

      character(len=9) :: nemo_f
      character(len=1) :: tab_f

      call numtbd(lun, idn, nemo_f, tab_f, iret)

      call copy_f_c_str(nemo_f, nemo, nemo_str_len)
      tab(1) = tab_f(1:1)
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
      character(kind=c_char), intent(in) :: cfxy(*)
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
      character(kind=c_char), intent(in) :: nemo(*)
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

    !> Explicitly initialize delayed replication factors for writing to a data subset
    !>
    !> Wraps drfini() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to write to
    !> @param mdrf - Array of delayed replication factors
    !> @param ndrf - Number of delayed replication factors in mdrf
    !> @param table_d_mnemonic - Table D mnemonic
    !>
    !> @author Jeff Ator @date 2025-10-28
    recursive subroutine drfini_c(bufr_unit, mdrf, ndrf, table_d_mnemonic) bind(C, name='drfini_f')
      integer(c_int), value, intent(in) :: bufr_unit, ndrf
      integer(c_int), intent(in) :: mdrf(*)
      character(kind=c_char), intent(in) :: table_d_mnemonic(*)
      character(len=12) :: str
      integer :: lstr

      lstr = get_c_string_length(table_d_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_d_mnemonic(1:lstr), str)
      endif
      call drfini(bufr_unit, mdrf, ndrf, str(1:lstr))
    end subroutine drfini_c

    !> Read/write an entire sequence of data values from/to a data subset.
    !>
    !> Wraps ufbseq() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from or write to
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read or write
    !> @param iret - Return value, length of data read
    !> @param table_d_mnemonic - Table A or Table D mnemonic.
    !>
    !> @author J. Ator @date 2023-04-07
    recursive subroutine ufbseq_c(bufr_unit, c_data, dim_1, dim_2, iret, table_d_mnemonic) bind(C, name='ufbseq_f')
      integer(c_int), value, intent(in) :: bufr_unit, dim_1, dim_2
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_d_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_d_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_d_mnemonic(1:lstr), str)
      endif
      call c_f_pointer(c_data, f_data)
      call ufbseq(bufr_unit, f_data, dim_1, dim_2, iret, str(1:lstr))
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
      character(kind=c_char), intent(out) :: c_subset(*)
      integer(c_int), intent(out) :: iddate
      integer(c_int), value, intent(in) :: subset_str_len
      integer(c_int) :: ires
      character(len=25) :: f_subset
      integer :: ireadns

      ires = ireadns(bufr_unit, f_subset, iddate)

      if (ires == 0) then
        call copy_f_c_str(f_subset, c_subset, subset_str_len)
      end if
    end function ireadns_c

    !> Read the next data subset from a BUFR file.
    !>
    !> Wraps readns() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_subset - Subset string
    !> @param iddate - Datetime of message
    !> @param subset_str_len - Length of the subset string
    !> @param ires - Return code:
    !>  - 0 new BUFR data subset was successfully read into internal arrays
    !>  - -1 there are no more BUFR data subsets in bufr_unit
    !>
    !> @author Jeff Ator @date 2025-09-05
    recursive subroutine readns_c(bufr_unit, c_subset, iddate, subset_str_len, ires) bind(C, name='readns_f')
      integer(c_int), value, intent(in) :: bufr_unit
      character(kind=c_char), intent(out) :: c_subset(*)
      integer(c_int), intent(out) :: iddate, ires
      integer(c_int), value, intent(in) :: subset_str_len
      character(len=25) :: f_subset

      call readns(bufr_unit, f_subset, iddate, ires)

      if (ires == 0) then
        call copy_f_c_str(f_subset, c_subset, subset_str_len)
      end if
    end subroutine readns_c

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

      ires = ibfms(r8val)
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
      character(kind=c_char), intent(in) :: str(*)
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
      character(kind=c_char), intent(in) :: numb(*), nemo(*), celsq(*)
      character(len=6) :: numb_f
      character(len=8) :: nemo_f
      character(len=55) :: celsq_f
      integer :: ii

      do ii = 1,6
        numb_f(ii:ii) = numb(ii)
      enddo
      do ii = 1,8
        nemo_f(ii:ii) = nemo(ii)
      enddo
      do ii = 1,55
        celsq_f(ii:ii) = celsq(ii)
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

      ires = igettdi(iflag)
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
    recursive subroutine bort_c(errstr) bind(C, name='bort_f')
      character(kind=c_char), intent(in) :: errstr(*)
      character(len=255) :: my_errstr
      integer :: lers

      lers = get_c_string_length(errstr)
      my_errstr = transfer(errstr(1:lers), my_errstr)

      call bort(my_errstr(1:lers))
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
    recursive subroutine openmb_c(bufr_unit, c_subset, iddate) bind(C, name='openmb_f')
      integer(c_int), value, intent(in) :: bufr_unit, iddate
      character(kind=c_char), intent(in) :: c_subset(*)
      character(len=8) :: f_subset
      integer :: lfs

      lfs = get_c_string_length(c_subset)
      if (lfs == 0) then
        f_subset(1:1) = ' '
        lfs = 1
      else
        f_subset = transfer(c_subset(1:lfs), f_subset)
      endif

      call openmb(bufr_unit, f_subset(1:lfs), iddate)
    end subroutine openmb_c

    !> Open a new message for output in a BUFR file that was
    !> previously opened for writing.
    !>
    !> Wraps openmg() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to write to.
    !> @param c_subset - Table A mnemonic of message.
    !> @param iddate - Date-time to be stored within Section 1 of message.
    !>
    !> @author J. Ator @date 2025-10-20
    recursive subroutine openmg_c(bufr_unit, c_subset, iddate) bind(C, name='openmg_f')
      integer(c_int), value, intent(in) :: bufr_unit, iddate
      character(kind=c_char), intent(in) :: c_subset(*)
      character(len=8) :: f_subset
      integer :: lfs

      lfs = get_c_string_length(c_subset)
      if (lfs == 0) then
        f_subset(1:1) = ' '
        lfs = 1
      else
        f_subset = transfer(c_subset(1:lfs), f_subset)
      endif

      call openmg(bufr_unit, f_subset(1:lfs), iddate)
    end subroutine openmg_c

    !> Close a BUFR message
    !>
    !> Wraps closmg() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to write to.
    !>
    !> @author J. Ator @date 2025-12-09
    recursive subroutine closmg_c(bufr_unit) bind(C, name='closmg_f')
      integer(c_int), value, intent(in) :: bufr_unit

      call closmg(bufr_unit)
    end subroutine closmg_c

    !> Get the version number of the NCEPLIBS-bufr software.
    !>
    !> Wraps bvers() subroutine.
    !>
    !> @param cverstr - Version string.
    !> @param cverstr_len - Length of the version string.
    !>
    !> @author J. Ator @date 2023-04-07
    recursive subroutine bvers_c(cverstr, cverstr_len) bind(C, name='bvers_f')
      character(kind=c_char), intent(out) :: cverstr(*)
      integer(c_int), value, intent(in) :: cverstr_len
      integer :: lallc

      ! Strings allocated within this subroutine will be for use in Fortran, so we won't need
      ! space for a trailing null and can therefore subtract 1 from cverstr_len.
      lallc = cverstr_len - 1

      if (lallc <= 0) then
        ! Any writeable string passed in from a C routine will always contain at least one byte
        ! for a trailing null, even if it's an empty string!
        cverstr(1) = c_null_char
      else if (allocated(bvers_fstr_outer)) then
        ! A previous call was directly made to this subroutine from within a C application
        ! program with bort catching enabled.  So we now need to allocate a separate "inner"
        ! string and recursively call bvers() again with that string.
        allocate(character*(lallc) :: bvers_fstr_inner)
        call bvers(bvers_fstr_inner)
        call copy_f_c_str(bvers_fstr_inner, cverstr, cverstr_len)
        deallocate(bvers_fstr_inner)
      else
        allocate(character*(lallc) :: bvers_fstr_outer)
        call bvers(bvers_fstr_outer)
        call copy_f_c_str(bvers_fstr_outer, cverstr, cverstr_len)
        deallocate(bvers_fstr_outer)
      end if
    end subroutine bvers_c

    !> Specify the use of compression when writing BUFR messages.
    !>
    !> Wraps cmpmsg() subroutine.
    !>
    !> @param cf - Flag indicating whether future BUFR output messages are to be
    !> compressed ('Y' = Yes, 'N' = No).
    !>
    !> @author J. Ator @date 2023-04-07
    recursive subroutine cmpmsg_c(cf) bind(C, name='cmpmsg_f')
      character(kind=c_char), intent(in) :: cf(*)
      character :: ch

      ch = cf(1)
      call cmpmsg(ch)
    end subroutine cmpmsg_c

    !> Specify whether subsequent bort errors should be caught and returned to
    !> the application program
    !>
    !> Wraps catch_borts() function.
    !>
    !> @param cf - Flag indicating whether subsequent bort errors should be caught
    !> and returned to the application program ('Y' = Yes, 'N' = No).
    !>
    !> @return catch_borts_c - -1 if cf contained an illegal value, otherwise 0
    !>
    !> @author J. Ator @date 2025-10-15
    function catch_borts_c(cf) result(ires) bind(C, name='catch_borts_f')
      character(kind=c_char), intent(in) :: cf(*)
      character :: ch
      integer(c_int) :: ires
      integer :: catch_borts

      ch = cf(1)
      ires = catch_borts(ch)
    end function catch_borts_c

    !> Sets a new bort target, if bort catching is enabled and such a target doesn't already exist.
    !>
    !> Wraps bort_target_set() function.
    !>
    !> @returns bort_target_set_c - Return code:
    !>  - 0 = a new bort target was not set during this call, or bort catching is disabled
    !>  - 1 = a new bort target was set during this call
    !>
    !> @author J. Ator @date 2025-11-05
    function bort_target_set_c() result(ires) bind(C, name='bort_target_set_f')
      integer(c_int) :: ires
      integer :: bort_target_set

      ires = bort_target_set()
    end function bort_target_set_c

    !> Clear any existing bort target.
    !>
    !> Wraps bort_target_unset() function.
    !>
    !> @author J. Ator @date 2025-11-05
    subroutine bort_target_unset_c() bind(C, name='bort_target_unset_f')
      call bort_target_unset
    end subroutine bort_target_unset_c

    !> Check whether a bort error was caught during a previous call to a library
    !> function or subroutine
    !>
    !> Wraps check_for_bort() subroutine.
    !>
    !> @param error_str - Error string if a bort error occurred; otherwise empty
    !> @param error_str_len - Allocated size of error_str
    !>
    !> @author J. Ator @date 2025-10-15
    subroutine check_for_bort_c(error_str, error_str_len) bind(C, name='check_for_bort_f')
      integer(c_int), value, intent(in) :: error_str_len
      character(kind=c_char), intent(out) :: error_str(*)
      character(len=:), allocatable :: error_str_f
      integer :: error_str_len_f

      if (error_str_len <= 1) then
        ! Any writeable string passed in from a C routine will always contain at least one byte for a trailing null,
        ! even if it's an empty string!
        error_str(1) = c_null_char
      else
        ! The following allocated string will be for use in Fortran, so we won't need space for a trailing null and can
        ! therefore subtract 1 from error_str_len.
        allocate(character(len=error_str_len-1) :: error_str_f)
        call check_for_bort(error_str_f, error_str_len_f)
        if (error_str_len_f == -1) error_str_len_f = 0  ! return empty string if catch_borts() wasn't previously called
        error_str_len_f = error_str_len_f + 1  ! add 1 for the null terminator
        call copy_f_c_str(error_str_f, error_str, error_str_len_f)
        deallocate(error_str_f)
      endif
    end subroutine check_for_bort_c

    !> Get the current location of the file pointer within a BUFR file.
    !>
    !> Wraps ufbcnt() subroutine.
    !>
    !> @param lunit - Fortran logical unit.
    !> @param kmsg - Message number
    !> @param ksub - Subset number
    !>
    !> @author J. Ator @date 2025-11-05
    recursive subroutine ufbcnt_c(lunit, kmsg, ksub) bind(C, name='ufbcnt_f')
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: kmsg, ksub

      call ufbcnt(lunit, kmsg, ksub)
    end subroutine ufbcnt_c

    !> Return a prepbufr program code corresponding to a mnemonic.
    !>
    !> Wraps ufbqcd() subroutine.
    !>
    !> @param lunit - Fortran logical unit.
    !> @param cnemo - Mnemonic
    !> @param iqcd - Y value of descriptor associated with mnemonic
    !>
    !> @author J. Ator @date 2025-11-05
    recursive subroutine ufbqcd_c(lunit, cnemo, iqcd) bind(C, name='ufbqcd_f')
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: iqcd
      character(kind=c_char), intent(in) :: cnemo(*)
      character(len=12) :: nemo
      integer :: lcn

      lcn = get_c_string_length(cnemo)
      if (lcn == 0) then
        nemo(1:1) = ' '
        lcn = 1
      else
        nemo = transfer(cnemo(1:lcn), nemo)
      endif
      call ufbqcd(lunit, nemo(1:lcn), iqcd)
    end subroutine ufbqcd_c

    !> Return a mnemonic corresponding to a prepbufr program code.
    !>
    !> Wraps ufbqcp() subroutine.
    !>
    !> @param lunit - Fortran logical unit.
    !> @param iqcp - Y value of a Category 63 Table D descriptor
    !> @param cnemo - Mnemonic associated with iqcp
    !> @param cnemo_len - Allocated length of cnemo string
    !>
    !> @author J. Ator @date 2025-11-05
    recursive subroutine ufbqcp_c(lunit, iqcp, cnemo, cnemo_len) bind(C, name='ufbqcp_f')
      integer(c_int), value, intent(in) :: lunit, iqcp, cnemo_len
      character(kind=c_char), intent(out) :: cnemo(*)
      character(len=8) :: nemo
      integer :: lnm

      call ufbqcp(lunit, iqcp, nemo)

      lnm = len_trim(nemo) + 1  ! add 1 for the null terminator
      call copy_f_c_str(nemo, cnemo, min(lnm, cnemo_len))
    end subroutine ufbqcp_c

    !> Get the meaning of a numerical value from a code or flag table
    !>
    !> @param lunit - Fortran logical unit.
    !> @param cnemoi - Mnemonic to search for
    !> @param ivali - Value associated with cnemoi
    !> @param cnemod - Optional second mnemonic upon which cnemoi may depend
    !> @param ivald - Value associated with cnemod
    !> @param cmeang_c - Meaning associated with cnemoi and ivali (and possibly cnemod and ivald as well)
    !> @param lcmgc - Allocated length of cmeang_c
    !> @param iret - Return code from call to getcfmng
    !>
    !> @author J. Ator @date 2025-11-05
    recursive subroutine getcfmng_c(lunit, cnemoi, ivali, cnemod, ivald, cmeang_c, lcmgc, iret) &
        bind(C, name='getcfmng_f')
      integer(c_int), value, intent(in) :: lunit, ivali, ivald, lcmgc
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: cnemoi(*), cnemod(*)
      character(kind=c_char), intent(out) :: cmeang_c(*)
      character(len=8) :: nemoi, nemod
      integer :: lcni, lcnd, lcmg, lallc

      lcni = get_c_string_length(cnemoi)
      if (lcni == 0) then
        nemoi(1:1) = ' '
        lcni = 1
      else
        nemoi = transfer(cnemoi(1:lcni), nemoi)
      endif
      lcnd = get_c_string_length(cnemod)
      if (lcnd == 0) then
        nemod(1:1) = ' '
        lcnd = 1
      else
        nemod = transfer(cnemod(1:lcnd), nemod)
      endif

      ! Strings allocated within this subroutine will be for use in Fortran, so we won't need
      ! space for a trailing null and can therefore subtract 1 from lcmgc.
      lallc = lcmgc - 1

      if (lallc <= 0) then
        ! Any writeable string passed in from a C routine will always contain at least one byte
        ! for a trailing null, even if it's an empty string!
        cmeang_c(1) = c_null_char
      else if (allocated(getcfmng_cmng_outer)) then
        ! A previous call was directly made to this subroutine from within a C application
        ! program with bort catching enabled.  So we now need to allocate a separate "inner"
        ! string and recursively call getcfmng() again with that string.
        allocate(character*(lallc) :: getcfmng_cmng_inner)
        call getcfmng(lunit, nemoi(1:lcni), ivali, nemod(1:lcnd), ivald, getcfmng_cmng_inner, lcmg, iret)
        lcmg = lcmg + 1  ! add 1 for the null terminator
        call copy_f_c_str(getcfmng_cmng_inner, cmeang_c, lcmg)
        deallocate(getcfmng_cmng_inner)
      else
        allocate(character*(lallc) :: getcfmng_cmng_outer)
        call getcfmng(lunit, nemoi(1:lcni), ivali, nemod(1:lcnd), ivald, getcfmng_cmng_outer, lcmg, iret)
        lcmg = lcmg + 1  ! add 1 for the null terminator
        call copy_f_c_str(getcfmng_cmng_outer, cmeang_c, lcmg)
        deallocate(getcfmng_cmng_outer)
      end if
    end subroutine getcfmng_c

    !> Get the bit settings equivalent to a given numerical value for a flag table mnemonic.
    !>
    !> Wraps upftbv() subroutine.
    !>
    !> @param lunit - Fortran logical unit.
    !> @param cnemo - Mnemonic with flag table units
    !> @param val - Value corresponding to cnemo
    !> @param ibit - Bit numbers which were set to "On" in val
    !> @param mxib - Allocated size of ibit
    !> @param nib - Number of bit numbers returned in ibit
    !>
    !> @author J. Ator @date 2025-11-05
    recursive subroutine upftbv_c(lunit, cnemo, val, ibit, mxib, nib) bind(C, name='upftbv_f')
      integer(c_int), value, intent(in) :: lunit, mxib
      integer(c_int), intent(out) :: ibit(*), nib
      real(c_double), value, intent(in) :: val
      character(kind=c_char), intent(in) :: cnemo(*)
      character(len=12) :: nemo
      integer :: lcn

      lcn = get_c_string_length(cnemo)
      if (lcn == 0) then
        nemo(1:1) = ' '
        lcn = 1
      else
        nemo = transfer(cnemo(1:lcn), nemo)
      endif
      call upftbv(lunit, nemo(1:lcn), val, mxib, ibit, nib)
    end subroutine upftbv_c

    !> Read one or more data values from every data subset in a BUFR file.
    !>
    !> Wraps ufbtab() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read
    !> @param iret - Return value, number of data subsets read
    !> @param table_b_mnemonic - String of mnemonics to read from each data subset
    !>
    !> @author J. Ator @date 2025-11-13
    recursive subroutine ufbtab_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbtab_f')
      integer(c_int), value, intent(in) :: bufr_unit, dim_1, dim_2
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), intent(inout) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif
      call c_f_pointer(c_data, f_data)

      call ufbtab(bufr_unit, f_data, dim_1, dim_2, iret, str(1:lstr))
    end subroutine ufbtab_c

    !> Jump forwards or backwards to a specified data subset within a BUFR file.
    !>
    !> Wraps ufbpos() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param irec - Ordinal number of message to be read
    !> @param isub - Ordinal number of subset to be read from (irec)th message
    !> @param c_subset - Subset string
    !> @param iddate - Datetime of message
    !> @param subset_str_len - Length of the subset string
    !>
    !> @author Jeff Ator @date 2025-11-13
    recursive subroutine ufbpos_c(bufr_unit, irec, isub, c_subset, iddate, subset_str_len) bind(C, name='ufbpos_f')
      integer(c_int), value, intent(in) :: bufr_unit, subset_str_len, irec, isub
      character(kind=c_char), intent(out) :: c_subset(*)
      integer(c_int), intent(out) :: iddate
      character(len=25) :: f_subset

      call ufbpos(bufr_unit, irec, isub, f_subset, iddate)

      call copy_f_c_str(f_subset, c_subset, subset_str_len)
    end subroutine ufbpos_c

    !> Specify the format of Section 1 date-time values that will be output by future calls to message-reading subroutines.
    !>
    !> Wraps datelen() subroutine.
    !>
    !> @param len - Length of Section 1 date-time values to be output by all future calls to message-reading subroutines
    !>
    !> @author Jeff Ator @date 2025-11-14
    recursive subroutine datelen_c(len) bind(C, name='datelen_f')
      integer(c_int), value, intent(in) :: len

      call datelen(len)
    end subroutine datelen_c

    !> Read a specified value from within Section 0 or 1 of a BUFR message.
    !>
    !> Wraps iupvs01() function.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param c_s01m - Mnemonic
    !>
    !> @returns iupvs01_c - Value corresponding to mnemonic
    !>
    !> @author Jeff Ator @date 2025-11-14
    recursive function iupvs01_c(bufr_unit, c_s01m) result(ires) bind(C, name='iupvs01_f')
      integer(c_int), value, intent(in) :: bufr_unit
      character(kind=c_char), intent(in) :: c_s01m(*)
      integer(c_int) :: ires
      integer :: iupvs01, lfs
      character(len=12) :: f_s01m

      lfs = get_c_string_length(c_s01m)
      if (lfs == 0) then
        f_s01m(1:1) = ' '
        lfs = 1
      else
        f_s01m = transfer(c_s01m(1:lfs), f_s01m)
      endif

      ires = iupvs01(bufr_unit, f_s01m(1:lfs))
    end function iupvs01_c

    !> Get the total number of data subsets available within a BUFR message
    !>
    !> Wraps nmsub() function.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !>
    !> @returns nmsub_c - Number of data subsets
    !>
    !> @author Jeff Ator @date 2025-11-14
    recursive function nmsub_c(bufr_unit) result(ires) bind(C, name='nmsub_f')
      integer(c_int), value, intent(in) :: bufr_unit
      integer(c_int) :: ires
      integer :: nmsub

      ires = nmsub(bufr_unit)
    end function nmsub_c

    !> Specify a value to be written into Section 0 or 1 of all future BUFR messages
    !>
    !> Wraps pkvs01() subroutine.
    !>
    !> @param c_s01m - Mnemonic
    !> @param ival - Value corresponding to mnemonic
    !>
    !> @author Jeff Ator @date 2025-11-14
    recursive subroutine pkvs01_c(c_s01m, ival) bind(C, name='pkvs01_f')
      character(kind=c_char), intent(in) :: c_s01m(*)
      integer(c_int), value, intent(in) :: ival
      integer :: lfs
      character(len=12) :: f_s01m

      lfs = get_c_string_length(c_s01m)
      if (lfs == 0) then
        f_s01m(1:1) = ' '
        lfs = 1
      else
        f_s01m = transfer(c_s01m(1:lfs), f_s01m)
      endif

      call pkvs01(f_s01m(1:lfs), ival)
    end subroutine pkvs01_c

    !> Get the Section 1 date-time from the first data message of a BUFR file.
    !>
    !> Wraps datebf() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param mear - Year stored within Section 1 of first data message
    !> @param mmon - Month stored within Section 1 of first data message
    !> @param mday - Day stored within Section 1 of first data message
    !> @param mour - Hour stored within Section 1 of first data message
    !> @param idate - Date-time stored within Section 1 of first data message
    !>
    !> @author Jeff Ator @date 2025-11-18
    recursive subroutine datebf_c(bufr_unit, mear, mmon, mday, mour, idate) bind(C, name='datebf_f')
      integer(c_int), value, intent(in) :: bufr_unit
      integer(c_int), intent(out) :: mear, mmon, mday, mour, idate

      call datebf(bufr_unit, mear, mmon, mday, mour, idate)
    end subroutine datebf_c

    !> Get the Section 1 date-time from the first two "dummy" messages of an NCEP dump file.
    !>
    !> Wraps dumpbf() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param jdate - Dump center date-time stored within Section 1 of first "dummy" message
    !> @param jdump - Dump initiation date-time stored within Section 1 of second "dummy" message
    !>
    !> @author Jeff Ator @date 2025-11-18
    recursive subroutine dumpbf_c(bufr_unit, jdate, jdump) bind(C, name='dumpbf_f')
      integer(c_int), value, intent(in) :: bufr_unit
      integer(c_int), intent(out) :: jdate(*), jdump(*)

      call dumpbf(bufr_unit, jdate, jdump)
    end subroutine dumpbf_c

    !> Write a minutes value into Section 1 of a BUFR message.
    !>
    !> Wraps minimg() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param mini - Minutes value
    !>
    !> @author Jeff Ator @date 2025-11-18
    recursive subroutine minimg_c(bufr_unit, mini) bind(C, name='minimg_f')
      integer(c_int), value, intent(in) :: bufr_unit, mini

      call minimg(bufr_unit, mini)
    end subroutine minimg_c

    !> Get the sequence of data descriptors contained within Section 3 of a BUFR message.
    !>
    !> Wraps upds3() subroutine.
    !>
    !> @param mbay - BUFR message
    !> @param lcds3 - Allocated length of cds3
    !> @param ccds3 - Data descriptor sequence within Section 3 of mbay
    !> @param nds3 - Number of descriptors returned in cds3
    !>
    !> @author Jeff Ator @date 2025-11-18
    recursive subroutine upds3_c(mbay, lcds3, ccds3, nds3) bind(C, name='upds3_f')
      integer(c_int), value, intent(in) :: lcds3
      integer(c_int), intent(in) :: mbay(*)
      integer(c_int), intent(out) :: nds3
      character(kind=c_char), intent(out) :: ccds3(6,*)

      if (lcds3 <= 0) then
        nds3 = 0
      else if (allocated(upds3_cds3_outer)) then
        ! A previous call was directly made to this subroutine from within a C application
        ! program with bort catching enabled.  So we now need to allocate a separate "inner"
        ! array and recursively call upds3() again with that array.
        allocate(upds3_cds3_inner(lcds3))
        call upds3(mbay, lcds3, upds3_cds3_inner, nds3)
        call copy_f_c_str_arr(upds3_cds3_inner, ccds3, 6, nds3)
        deallocate(upds3_cds3_inner)
      else
        allocate(upds3_cds3_outer(lcds3))
        call upds3(mbay, lcds3, upds3_cds3_outer, nds3)
        call copy_f_c_str_arr(upds3_cds3_outer, ccds3, 6, nds3)
        deallocate(upds3_cds3_outer)
      end if
    end subroutine upds3_c

    !> Specify a value to be written into Section 1 of a BUFR message
    !>
    !> Wraps pkbs1() subroutine.
    !>
    !> @param ival - Value corresponding to mnemonic
    !> @param mbay - BUFR message
    !> @param c_s1m - Mnemonic
    !>
    !> @author Jeff Ator @date 2025-11-18
    recursive subroutine pkbs1_c(ival, mbay, c_s1m) bind(C, name='pkbs1_f')
      character(kind=c_char), intent(in) :: c_s1m(*)
      integer(c_int), value, intent(in) :: ival
      integer(c_int), intent(inout) :: mbay(*)
      integer :: lfs
      character(len=12) :: f_s1m

      lfs = get_c_string_length(c_s1m)
      if (lfs == 0) then
        f_s1m(1:1) = ' '
        lfs = 1
      else
        f_s1m = transfer(c_s1m(1:lfs), f_s1m)
      endif

      call pkbs1(ival, mbay, f_s1m(1:lfs))
    end subroutine pkbs1_c

    !> Specify a tank receipt time to be written into Section 1 of all future BUFR messages
    !>
    !> Wraps strcpt() subroutine.
    !>
    !> @param cf - Flag indicating whether future BUFR output messages should include a tank receipt time
    !> @param iyr - Tank receipt year
    !> @param imo - Tank receipt month
    !> @param idy - Tank receipt day
    !> @param ihr - Tank receipt hour
    !> @param imi - Tank receipt minute
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine strcpt_c(cf, iyr, imo, idy, ihr, imi) bind(C, name='strcpt_f')
      integer(c_int), value, intent(in) :: iyr, imo, idy, ihr, imi
      character(kind=c_char), intent(in) :: cf(*)
      character :: ch

      ch = cf(1)
      call strcpt(ch, iyr, imo, idy, ihr, imi)
    end subroutine strcpt_c

    !> Get the tank receipt time from Section 1 of a BUFR message
    !>
    !> Wraps rtrcpt() subroutine.
    !>
    !> @param lunit - Fortran logical unit
    !> @param iyr - Tank receipt year
    !> @param imo - Tank receipt month
    !> @param idy - Tank receipt day
    !> @param ihr - Tank receipt hour
    !> @param imi - Tank receipt minute
    !> @param iret - Return code
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine rtrcpt_c(lunit, iyr, imo, idy, ihr, imi, iret) bind(C, name='rtrcpt_f')
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: iyr, imo, idy, ihr, imi, iret

      call rtrcpt(lunit, iyr, imo, idy, ihr, imi, iret)
    end subroutine rtrcpt_c

    !> Read a BUFR message and output an equivalent message with a tank receipt time added to Section 1
    !>
    !> Wraps atrcpt() subroutine.
    !>
    !> @param msgin - BUFR message
    !> @param lmsgot - Allocated length of msgot
    !> @param msgot - Copy of msgin with a tank receipt time added to Section 1
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine atrcpt_c(msgin, lmsgot, msgot) bind(C, name='atrcpt_f')
      integer(c_int), value, intent(in) :: lmsgot
      integer(c_int), intent(in) :: msgin(*)
      integer(c_int), intent(out) :: msgot(*)

      call atrcpt(msgin, lmsgot, msgot)
    end subroutine atrcpt_c

    !> Print a copy of the DX BUFR table associated with a specified Fortran logical unit
    !>
    !> Wraps dxdump() subroutine.
    !>
    !> @param lunit - Fortran logical unit for BUFR file
    !> @param luprt - Fortran logical unit for print output
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine dxdump_c(lunit, luprt) bind(C, name='dxdump_f')
      integer(c_int), value, intent(in) :: lunit, luprt

      call dxdump(lunit, luprt)
    end subroutine dxdump_c

    !> Print a verbose listing of the contents of a data subset
    !>
    !> Wraps ufbdmp() subroutine.
    !>
    !> @param lunit - Fortran logical unit for BUFR file
    !> @param luprt - Fortran logical unit for print output
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine ufbdmp_c(lunit, luprt) bind(C, name='ufbdmp_f')
      integer(c_int), value, intent(in) :: lunit, luprt

      call ufbdmp(lunit, luprt)
    end subroutine ufbdmp_c

    !> Print a verbose listing of the contents of a data subset
    !>
    !> Wraps ufdump() subroutine.
    !>
    !> @param lunit - Fortran logical unit for BUFR file
    !> @param luprt - Fortran logical unit for print output
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine ufdump_c(lunit, luprt) bind(C, name='ufdump_f')
      integer(c_int), value, intent(in) :: lunit, luprt

      call ufdump(lunit, luprt)
    end subroutine ufdump_c

    !> Copy an entire BUFR file from one Fortran logical unit to another
    !>
    !> Wraps copybf() subroutine.
    !>
    !> @param lunin - Fortran logical unit for source BUFR file
    !> @param lunot - Fortran logical unit for target BUFR file
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine copybf_c(lunin, lunot) bind(C, name='copybf_f')
      integer(c_int), value, intent(in) :: lunin, lunot

      call copybf(lunin, lunot)
    end subroutine copybf_c

    !> Copy a BUFR message from one Fortran logical unit to another
    !>
    !> Wraps copymg() subroutine.
    !>
    !> @param lunin - Fortran logical unit for source BUFR file
    !> @param lunot - Fortran logical unit for target BUFR file
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine copymg_c(lunin, lunot) bind(C, name='copymg_f')
      integer(c_int), value, intent(in) :: lunin, lunot

      call copymg(lunin, lunot)
    end subroutine copymg_c

    !> Copy a BUFR data subset from one Fortran logical unit to another
    !>
    !> Wraps copysb() subroutine.
    !>
    !> @param lunin - Fortran logical unit for source BUFR file
    !> @param lunot - Fortran logical unit for target BUFR file
    !> @param iret - Return code
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine copysb_c(lunin, lunot, iret) bind(C, name='copysb_f')
      integer(c_int), value, intent(in) :: lunin, lunot
      integer(c_int), intent(out) :: iret

      call copysb(lunin, lunot, iret)
    end subroutine copysb_c

    !> Copy a BUFR data subset from one Fortran logical unit to another
    !>
    !> Wraps ufbcpy() subroutine.
    !>
    !> @param lunin - Fortran logical unit for source BUFR file
    !> @param lunot - Fortran logical unit for target BUFR file
    !>
    !> @author J. Ator @date 2025-11-20
    recursive subroutine ufbcpy_c(lunin, lunot) bind(C, name='ufbcpy_f')
      integer(c_int), value, intent(in) :: lunin, lunot

      call ufbcpy(lunin, lunot)
    end subroutine ufbcpy_c

    !> Read a BUFR message from a memory array.
    !>
    !> Wraps readerme() subroutine.
    !>
    !> @param mesg - BUFR message
    !> @param bufr_unit - Fortran logical unit number
    !> @param c_subset - Subset string
    !> @param iddate - Datetime of message
    !> @param subset_str_len - Length of the subset string
    !> @param ires - Return code
    !>
    !> @author Jeff Ator @date 2025-11-25
    recursive subroutine readerme_c(mesg, bufr_unit, c_subset, iddate, subset_str_len, ires) bind(C, name='readerme_f')
      integer(c_int), value, intent(in) :: bufr_unit, subset_str_len
      integer(c_int), intent(in) :: mesg(*)
      integer(c_int), intent(out) :: iddate, ires
      character(kind=c_char), intent(out) :: c_subset(*)
      character(len=25) :: f_subset

      call readerme(mesg, bufr_unit, f_subset, iddate, ires)

      if (ires == 0) then
        call copy_f_c_str(f_subset, c_subset, subset_str_len)
      end if
    end subroutine readerme_c

    !> Read a specified data subset from a BUFR file.
    !>
    !> Wraps rdmgsb() subroutine.
    !>
    !> @param lunit - Fortran logical unit for BUFR file
    !> @param imsg - Message number
    !> @param isub - Subset number
    !>
    !> @author J. Ator @date 2025-11-25
    recursive subroutine rdmgsb_c(lunit, imsg, isub) bind(C, name='rdmgsb_f')
      integer(c_int), value, intent(in) :: lunit, imsg, isub

      call rdmgsb(lunit, imsg, isub)
    end subroutine rdmgsb_c

    !> Read an entire BUFR file into internal arrays.
    !>
    !> Wraps ufbmem() subroutine.
    !>
    !> @param lunit - Fortran logical unit for BUFR file
    !> @param inew - Processing option
    !> @param iret - Number of BUFR messages that were read and stored into internal arrays
    !> @param iunit - File status
    !>
    !> @author J. Ator @date 2025-11-25
    recursive subroutine ufbmem_c(lunit, inew, iret, iunit) bind(C, name='ufbmem_f')
      integer(c_int), value, intent(in) :: lunit, inew
      integer(c_int), intent(out) :: iret, iunit

      call ufbmem(lunit, inew, iret, iunit)
    end subroutine ufbmem_c

    !> Read an entire BUFR file into internal arrays.
    !>
    !> Wraps ufbmex() subroutine.
    !>
    !> @param lunit - Fortran logical unit for BUFR file
    !> @param lundx - Fortran logical unit number containing DX BUFR table information
    !> @param inew - Processing option
    !> @param iret - Number of BUFR messages that were read and stored into internal arrays
    !> @param mesg - Types of BUFR messages that were read and stored into internal arrays
    !>
    !> @author J. Ator @date 2025-11-25
    recursive subroutine ufbmex_c(lunit, lundx, inew, iret, mesg) bind(C, name='ufbmex_f')
      integer(c_int), value, intent(in) :: lunit, lundx, inew
      integer(c_int), intent(out) :: iret, mesg(*)

      call ufbmex(lunit, lundx, inew, iret, mesg)
    end subroutine ufbmex_c

    !> Read a specified data subset from internal arrays.
    !>
    !> Wraps ufbmms() subroutine.
    !>
    !> @param imsg - Number of BUFR message to be read
    !> @param isub - Number of data subset to be read from imsg
    !> @param c_subset - Subset string
    !> @param jdate - Datetime of message
    !> @param subset_str_len - Length of the subset string
    !>
    !> @author Jeff Ator @date 2025-12-01
    recursive subroutine ufbmms_c(imsg, isub, c_subset, jdate, subset_str_len) bind(C, name='ufbmms_f')
      integer(c_int), value, intent(in) :: imsg, isub, subset_str_len
      integer(c_int), intent(out) :: jdate
      character(kind=c_char), intent(out) :: c_subset(*)
      character(len=10) :: f_subset

      call ufbmms(imsg, isub, f_subset, jdate)

      call copy_f_c_str(f_subset, c_subset, subset_str_len)
    end subroutine ufbmms_c

    !> Read a specified data subset from internal arrays.
    !>
    !> Wraps ufbmns() subroutine.
    !>
    !> @param irep - Number of data subset to be read
    !> @param c_subset - Subset string
    !> @param idate - Datetime of message
    !> @param subset_str_len - Length of the subset string
    !>
    !> @author Jeff Ator @date 2025-12-01
    recursive subroutine ufbmns_c(irep, c_subset, idate, subset_str_len) bind(C, name='ufbmns_f')
      integer(c_int), value, intent(in) :: irep, subset_str_len
      integer(c_int), intent(out) :: idate
      character(kind=c_char), intent(out) :: c_subset(*)
      character(len=10) :: f_subset

      call ufbmns(irep, f_subset, idate)

      call copy_f_c_str(f_subset, c_subset, subset_str_len)
    end subroutine ufbmns_c

    !> Read a specified message from internal arrays.
    !>
    !> Wraps rdmemm() subroutine.
    !>
    !> @param imsg - Number of BUFR message to be read
    !> @param c_subset - Subset string
    !> @param jdate - Datetime of message
    !> @param subset_str_len - Length of the subset string
    !> @param ires - Return code
    !>
    !> @author Jeff Ator @date 2025-12-01
    recursive subroutine rdmemm_c(imsg, c_subset, jdate, subset_str_len, ires) bind(C, name='rdmemm_f')
      integer(c_int), value, intent(in) :: imsg, subset_str_len
      character(kind=c_char), intent(out) :: c_subset(*)
      integer(c_int), intent(out) :: jdate, ires
      character(len=10) :: f_subset

      call rdmemm(imsg, f_subset, jdate, ires)

      if (ires == 0) then
        call copy_f_c_str(f_subset, c_subset, subset_str_len)
      end if
    end subroutine rdmemm_c

    !> Read a specified data subset from internal arrays.
    !>
    !> Wraps rdmems() subroutine.
    !>
    !> @param isub - Number of data subset to be read
    !> @param ires - Return code
    !>
    !> @author Jeff Ator @date 2025-12-01
    recursive subroutine rdmems_c(isub, ires) bind(C, name='rdmems_f')
      integer(c_int), value, intent(in) :: isub
      integer(c_int), intent(out) :: ires

      call rdmems(isub, ires)
    end subroutine rdmems_c

    !> Read one or more data values from internal arrays.
    !>
    !> Wraps ufbrms() subroutine.
    !>
    !> @param imsg - Number of BUFR message to be read
    !> @param isub - Number of data subset to be read from imsg
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read
    !> @param iret - Return value, length of data read
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author Jeff Ator @date 2025-12-01
    recursive subroutine ufbrms_c(imsg, isub, c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbrms_f')
      integer(c_int), value, intent(in) :: imsg, isub, dim_1, dim_2
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif
      call c_f_pointer(c_data, f_data)
      call ufbrms(imsg, isub, f_data, dim_1, dim_2, iret, str(1:lstr))
    end subroutine ufbrms_c

    !> Read one or more data values from every data subset in internal arrays.
    !>
    !> Wraps ufbtam() subroutine.
    !>
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read
    !> @param iret - Return value, number of data subsets read
    !> @param table_b_mnemonic - String of mnemonics to read from each data subset
    !>
    !> @author J. Ator @date 2025-12-01
    recursive subroutine ufbtam_c(c_data, dim_1, dim_2, iret, table_b_mnemonic) bind(C, name='ufbtam_f')
      integer(c_int), value, intent(in) :: dim_1, dim_2
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif
      call c_f_pointer(c_data, f_data)

      call ufbtam(f_data, dim_1, dim_2, iret, str(1:lstr))
    end subroutine ufbtam_c

    !> Copy a message from internal arrays to a file
    !>
    !> Wraps cpymem() subroutine.
    !>
    !> @param lunot - Fortran logical unit for target BUFR file
    !>
    !> @author J. Ator @date 2025-12-02
    recursive subroutine cpymem_c(lunot) bind(C, name='cpymem_f')
      integer(c_int), value, intent(in) :: lunot

      call cpymem(lunot)
    end subroutine cpymem_c

    !> Copy unique elements of a data subset from one file to another
    !>
    !> Wraps ufbcup() subroutine.
    !>
    !> @param lunin - Fortran logical unit for source BUFR file
    !> @param lunot - Fortran logical unit for target BUFR file
    !>
    !> @author J. Ator @date 2025-12-02
    recursive subroutine ufbcup_c(lunin, lunot) bind(C, name='ufbcup_f')
      integer(c_int), value, intent(in) :: lunin, lunot

      call ufbcup(lunin, lunot)
    end subroutine ufbcup_c

    !> Specify whether to standardize future output BUFR messages
    !>
    !> Wraps stdmsg() subroutine.
    !>
    !> @param cf - Flag indicating whether future BUFR output messages should be WMO-standard
    !>
    !> @author J. Ator @date 2025-12-02
    recursive subroutine stdmsg_c(cf) bind(C, name='stdmsg_f')
      character(kind=c_char), intent(in) :: cf(*)
      character :: ch

      ch = cf(1)
      call stdmsg(ch)
    end subroutine stdmsg_c

    !> Standardize a copy of a BUFR message
    !>
    !> Wraps stndrd() subroutine.
    !>
    !> @param lunit - Fortran logical unit for BUFR file
    !> @param msgin - BUFR message
    !> @param lmsgot - Allocated length of msgot
    !> @param msgot - Copy of msgin now fully WMO-standardized
    !>
    !> @author J. Ator @date 2025-12-02
    recursive subroutine stndrd_c(lunit, msgin, lmsgot, msgot) bind(C, name='stndrd_f')
      integer(c_int), value, intent(in) :: lunit, lmsgot
      integer(c_int), intent(in) :: msgin(*)
      integer(c_int), intent(out) :: msgot(*)

      call stndrd(lunit, msgin, lmsgot, msgot)
    end subroutine stndrd_c

    !> Specify whether to read code and flag table information from master BUFR tables
    !>
    !> Wraps codflg() subroutine.
    !>
    !> @param cf - Flag indicating whether code and flag table information should be included
    !> when reading from master BUFR tables
    !>
    !> @author J. Ator @date 2025-12-02
    recursive subroutine codflg_c(cf) bind(C, name='codflg_f')
      character(kind=c_char), intent(in) :: cf(*)
      character :: ch

      ch = cf(1)
      call codflg(ch)
    end subroutine codflg_c

    !> Get the parent for a specified occurrence of a Table B or Table D mnemonic
    !>
    !> Wraps gettagpr() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number
    !> @param c_tagch - Table B or Table D mnemonic
    !> @param ntagch - Ordinal occurrence of c_tagch for which c_tagpr is to be returned
    !> @param c_tagpr - Table D mnemonic
    !> @param tagpr_len - Allocated length of c_tagpr
    !> @param ires - Return code
    !>
    !> @author J. Ator @date 2025-12-03
    recursive subroutine gettagpr_c(bufr_unit, c_tagch, ntagch, c_tagpr, tagpr_len, ires) bind(C, name='gettagpr_f')
      integer(c_int), value, intent(in) :: bufr_unit, ntagch, tagpr_len
      integer(c_int), intent(out) :: ires
      character(kind=c_char), intent(in) :: c_tagch(*)
      character(kind=c_char), intent(out) :: c_tagpr(*)
      character(len=10) :: f_tagch, f_tagpr
      integer :: lfc, lfp

      lfc = get_c_string_length(c_tagch)
      if (lfc == 0) then
        f_tagch(1:1) = ' '
        lfc = 1
      else
        f_tagch = transfer(c_tagch(1:lfc), f_tagch)
      endif

      call gettagpr(bufr_unit, f_tagch(1:lfc), ntagch, f_tagpr, ires)

      lfp = len_trim(f_tagpr) + 1  ! add 1 for the null terminator
      call copy_f_c_str(f_tagpr, c_tagpr, min(lfp, tagpr_len))
    end subroutine gettagpr_c

    !> Check whether a specified Table B mnemonic references another Table B mnemonic via an internal bitmap
    !>
    !> Wraps gettagre() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number
    !> @param c_tagi - Table B mnemonic
    !> @param ntagi - Ordinal occurrence of c_tagi for which c_tagre is to be returned
    !> @param c_tagre - Table B mnemonic referenced by c_tagi via an internal bitmap
    !> @param tagre_len - Allocated length of c_tagre
    !> @param ntagre - Ordinal occurrence of tagre referenced by (ntagi)th occurrence of tagi
    !> @param ires - Return code
    !>
    !> @author J. Ator @date 2025-12-03
    recursive subroutine gettagre_c(bufr_unit, c_tagi, ntagi, c_tagre, tagre_len, ntagre, ires) bind(C, name='gettagre_f')
      integer(c_int), value, intent(in) :: bufr_unit, ntagi, tagre_len
      integer(c_int), intent(out) :: ntagre, ires
      character(kind=c_char), intent(in) :: c_tagi(*)
      character(kind=c_char), intent(out) :: c_tagre(*)
      character(len=10) :: f_tagi, f_tagre
      integer :: lfi, lfr

      lfi = get_c_string_length(c_tagi)
      if (lfi == 0) then
        f_tagi(1:1) = ' '
        lfi = 1
      else
        f_tagi = transfer(c_tagi(1:lfi), f_tagi)
      endif

      call gettagre(bufr_unit, f_tagi(1:lfi), ntagi, f_tagre, ntagre, ires)

      lfr = len_trim(f_tagre) + 1  ! add 1 for the null terminator
      call copy_f_c_str(f_tagre, c_tagre, min(lfr, tagre_len))
    end subroutine gettagre_c

    !> Convert a BUFR message to edition 4
    !>
    !> Wraps cnved4() subroutine.
    !>
    !> @param msgin - BUFR message
    !> @param lmsgot - Allocated length of msgot
    !> @param msgot - Copy of msgin now converted to edition 4
    !>
    !> @author J. Ator @date 2025-12-03
    recursive subroutine cnved4_c(msgin, lmsgot, msgot) bind(C, name='cnved4_f')
      integer(c_int), value, intent(in) :: lmsgot
      integer(c_int), intent(in) :: msgin(*)
      integer(c_int), intent(out) :: msgot(*)

      call cnved4(msgin, lmsgot, msgot)
    end subroutine cnved4_c

    !> Check if a subset definition contains any long character strings
    !>
    !> Wraps lcmgdf() function.
    !>
    !> @param bufr_unit - Fortran logical unit number
    !> @param c_subset - Table A mnemonic
    !>
    !> @returns lcmgdf_c - Return code
    !>
    !> @author J. Ator @date 2025-12-03
    recursive function lcmgdf_c(bufr_unit, c_subset) result(ires) bind(C, name='lcmgdf_f')
      integer(c_int), value, intent(in) :: bufr_unit
      integer(c_int) :: ires
      character(kind=c_char), intent(in) :: c_subset(*)
      character(len=8) :: f_subset
      integer :: lcmgdf, lfs

      lfs = get_c_string_length(c_subset)
      if (lfs == 0) then
        f_subset(1:1) = ' '
        lfs = 1
      else
        f_subset = transfer(c_subset(1:lfs), f_subset)
      endif

      ires = lcmgdf(bufr_unit, f_subset(1:lfs))
    end function lcmgdf_c

    !> Write a data value corresponding to a specific occurrence of a mnemonic
    !>
    !> Wraps setvalnb() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number
    !> @param c_tagpv - Pivot mnemonic
    !> @param ntagpv - Ordinal occurrence of c_tagpv to search for
    !> @param c_tagnb - Nearby mnemonic
    !> @param ntagnb - Ordinal occurrence of c_tagnb to search for
    !> @param r8val - Value to be stored
    !> @param ires - Return code
    !>
    !> @author J. Ator @date 2025-12-05
    recursive subroutine setvalnb_c(bufr_unit, c_tagpv, ntagpv, c_tagnb, ntagnb, r8val, ires) bind(C, name='setvalnb_f')
      integer(c_int), value, intent(in) :: bufr_unit, ntagpv, ntagnb
      integer(c_int), intent(out) :: ires
      character(kind=c_char), intent(in) :: c_tagpv(*), c_tagnb(*)
      real(c_double), value, intent(in) :: r8val
      character(len=10) :: f_tagpv, f_tagnb
      integer :: lfp, lfn

      lfp = get_c_string_length(c_tagpv)
      if (lfp == 0) then
        f_tagpv(1:1) = ' '
        lfp = 1
      else
        f_tagpv = transfer(c_tagpv(1:lfp), f_tagpv)
      endif
      lfn = get_c_string_length(c_tagnb)
      if (lfn == 0) then
        f_tagnb(1:1) = ' '
        lfn = 1
      else
        f_tagnb = transfer(c_tagnb(1:lfn), f_tagnb)
      endif

      call setvalnb(bufr_unit, f_tagpv(1:lfp), ntagpv, f_tagnb(1:lfn), ntagnb, r8val, ires)
    end subroutine setvalnb_c

    !> Read a data value corresponding to a specific occurrence of a mnemonic
    !>
    !> Wraps getvalnb() function.
    !>
    !> @param bufr_unit - Fortran logical unit number
    !> @param c_tagpv - Pivot mnemonic
    !> @param ntagpv - Ordinal occurrence of c_tagpv to search for
    !> @param c_tagnb - Nearby mnemonic
    !> @param ntagnb - Ordinal occurrence of c_tagnb to search for
    !>
    !> @returns getvalnb_c - Return value
    !>
    !> @author J. Ator @date 2025-12-05
    recursive function getvalnb_c(bufr_unit, c_tagpv, ntagpv, c_tagnb, ntagnb) result(r8val) bind(C, name='getvalnb_f')
      integer(c_int), value, intent(in) :: bufr_unit, ntagpv, ntagnb
      character(kind=c_char), intent(in) :: c_tagpv(*), c_tagnb(*)
      real(c_double) :: r8val
      character(len=10) :: f_tagpv, f_tagnb
      integer :: lfp, lfn
      real*8 :: getvalnb

      lfp = get_c_string_length(c_tagpv)
      if (lfp == 0) then
        f_tagpv(1:1) = ' '
        lfp = 1
      else
        f_tagpv = transfer(c_tagpv(1:lfp), f_tagpv)
      endif
      lfn = get_c_string_length(c_tagnb)
      if (lfn == 0) then
        f_tagnb(1:1) = ' '
        lfn = 1
      else
        f_tagnb = transfer(c_tagnb(1:lfn), f_tagnb)
      endif

      r8val = getvalnb(bufr_unit, f_tagpv(1:lfp), ntagpv, f_tagnb(1:lfn), ntagnb)
    end function getvalnb_c

    !> Get Table B and Table D information from the internal DX tables
    !>
    !> Wraps getabdb() subroutine.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param itab - Allocated length of ctabdb
    !> @param ctabdb - Internal Table B and Table D information
    !> @param jtab - Number of entries returned in ctabdb
    !>
    !> @author Jeff Ator @date 2025-12-05
    recursive subroutine getabdb_c(lunit, itab, ctabdb, jtab) bind(C, name='getabdb_f')
      integer(c_int), value, intent(in) :: lunit, itab
      integer(c_int), intent(out) :: jtab
      character(kind=c_char), intent(out) :: ctabdb(128,*)

      if (itab <= 0) then
        jtab = 0
      else if (allocated(getabdb_tabdb_outer)) then
        ! A previous call was directly made to this subroutine from within a C application
        ! program with bort catching enabled.  So we now need to allocate a separate "inner"
        ! array and recursively call getabdb() again with that array.
        allocate(getabdb_tabdb_inner(itab))
        call getabdb(lunit, getabdb_tabdb_inner, itab, jtab)
        call copy_f_c_str_arr(getabdb_tabdb_inner, ctabdb, 128, jtab)
        deallocate(getabdb_tabdb_inner)
      else
        allocate(getabdb_tabdb_outer(itab))
        call getabdb(lunit, getabdb_tabdb_outer, itab, jtab)
        call copy_f_c_str_arr(getabdb_tabdb_outer, ctabdb, 128, jtab)
        deallocate(getabdb_tabdb_outer)
      end if
    end subroutine getabdb_c

    !> Read one or more data values from a data subset without advancing the subset pointer
    !>
    !> Wraps ufbget() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param tab - Data values
    !> @param i1 - Allocated length of tab
    !> @param iret - Return code
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author Jeff Ator @date 2025-12-05
    recursive subroutine ufbget_c(bufr_unit, tab, i1, iret, table_b_mnemonic) bind(C, name='ufbget_f')
      integer(c_int), value, intent(in) :: bufr_unit, i1
      integer(c_int), intent(out) :: iret
      real(c_double), intent(out) :: tab(*)
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif

      call ufbget(bufr_unit, tab, i1, iret, str(1:lstr))
    end subroutine ufbget_c

    !> Read one or more data values from a specified data subset
    !>
    !> Wraps ufbinx() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to read from
    !> @param imsg - Number of BUFR message to be read
    !> @param isub - Number of data subset to be read from imsg
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to read
    !> @param iret - Return value, length of data read
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author Jeff Ator @date 2025-12-05
    recursive subroutine ufbinx_c(bufr_unit, imsg, isub, c_data, dim_1, dim_2, iret, table_b_mnemonic) &
        bind(C, name='ufbinx_f')
      integer(c_int), value, intent(in) :: bufr_unit, dim_1, dim_2, imsg, isub
      type(c_ptr), intent(inout) ::  c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif

      call c_f_pointer(c_data, f_data)
      call ufbinx(bufr_unit, imsg, isub, f_data, dim_1, dim_2, iret, str(1:lstr))
    end subroutine ufbinx_c

    !> Overwrite one or more data values within a data subset
    !>
    !> Wraps ufbovr() subroutine.
    !>
    !> @param bufr_unit - Fortran logical unit number to write to
    !> @param c_data - C-style pointer to a pre-allocated buffer
    !> @param dim_1, dim_2 - Dimensionality of data to write
    !> @param iret - Return value, length of data written
    !> @param table_b_mnemonic - String of mnemonics
    !>
    !> @author Jeff Ator @date 2025-12-05
    recursive subroutine ufbovr_c(bufr_unit, c_data, dim_1, dim_2, iret, table_b_mnemonic) &
        bind(C, name='ufbovr_f')
      integer(c_int), value, intent(in) :: bufr_unit, dim_1, dim_2
      type(c_ptr), intent(in) ::  c_data
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(in) :: table_b_mnemonic(*)
      character(len=90) :: str
      real, pointer :: f_data
      integer :: lstr

      lstr = get_c_string_length(table_b_mnemonic)
      if (lstr == 0) then
        str(1:1) = ' '
        lstr = 1
      else
        str = transfer(table_b_mnemonic(1:lstr), str)
      endif

      call c_f_pointer(c_data, f_data)
      call ufbovr(bufr_unit, f_data, dim_1, dim_2, iret, str(1:lstr))
    end subroutine ufbovr_c

    !> Check if there are any more data subsets available within a BUFR message.
    !>
    !> Wraps ifbget() function.
    !>
    !> @param bufr_unit - Fortran logical unit number
    !>
    !> @returns ifbget_c - Return code
    !>
    !> @author J. Ator @date 2025-12-09
    recursive function ifbget_c(bufr_unit) result(ires) bind(C, name='ifbget_f')
      integer(c_int), value, intent(in) :: bufr_unit
      integer(c_int) :: ires
      integer :: ifbget

      ires = ifbget(bufr_unit)
    end function ifbget_c

    !> Check for an abnormal status code associated with the processing of a file
    !>
    !> Wraps igetsc() function.
    !>
    !> @param bufr_unit - Fortran logical unit number
    !>
    !> @returns igetsc_c - Return code
    !>
    !> @author J. Ator @date 2025-12-09
    recursive function igetsc_c(bufr_unit) result(ires) bind(C, name='igetsc_f')
      integer(c_int), value, intent(in) :: bufr_unit
      integer(c_int) :: ires
      integer :: igetsc

      ires = igetsc(bufr_unit)
    end function igetsc_c

    !> Generate DX BUFR table messages and write them to a output file
    !>
    !> Wraps wrdxtb() subroutine.
    !>
    !> @param lundx - Fortran logical unit number containing DX BUFR table information
    !> @param lunot - Fortran logical unit number to write to
    !>
    !> @author J. Ator @date 2025-12-09
    recursive subroutine wrdxtb_c(lundx, lunot) bind(C, name='wrdxtb_f')
      integer(c_int), value, intent(in) :: lundx, lunot

      call wrdxtb(lundx, lunot)
    end subroutine wrdxtb_c

    !> Get information from the first data message in a BUFR file
    !>
    !> Wraps mesgbf() subroutine.
    !>
    !> @param lunit - Fortran logical unit number
    !> @param mesgtyp - Message type
    !>
    !> @author J. Ator @date 2025-12-09
    recursive subroutine mesgbf_c(lunit, mesgtyp) bind(C, name='mesgbf_f')
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: mesgtyp

      call mesgbf(lunit, mesgtyp)
    end subroutine mesgbf_c

    !> Get information from the first data message in a BUFR file
    !>
    !> Wraps mesgbc() subroutine.
    !>
    !> @param lunin - Fortran logical unit number
    !> @param mesgtyp - Message type
    !> @param icomp - Compression indicator
    !>
    !> @author J. Ator @date 2025-12-09
    recursive subroutine mesgbc_c(lunin, mesgtyp, icomp) bind(C, name='mesgbc_f')
      integer(c_int), value, intent(in) :: lunin
      integer(c_int), intent(out) :: mesgtyp, icomp

      call mesgbc(lunin, mesgtyp, icomp)
    end subroutine mesgbc_c

    !> Merge parts of data subsets
    !>
    !> Wraps invmrg() subroutine.
    !>
    !> @param lubfi - Fortran logical unit number for input file
    !> @param lubfj - Fortran logical unit number for output file
    !>
    !> @author J. Ator @date 2025-12-09
    recursive subroutine invmrg_c(lubfi, lubfj) bind(C, name='invmrg_f')
      integer(c_int), value, intent(in) :: lubfi, lubfj

      call invmrg(lubfi, lubfj)
    end subroutine invmrg_c

    !> Decode an integer from a character string
    !>
    !> Wraps iupm() function.
    !>
    !> @param cbay - Character string
    !> @param nbits - Number of bits to decode from cbay
    !> @param lcbay - Length of cbay
    !>
    !> @returns iupm_c - Decoded value
    !>
    !> @author J. Ator @date 2025-12-09
    recursive function iupm_c(cbay, nbits, lcbay) result(ires) bind(C, name='iupm_f')
      character(kind=c_char), intent(in) :: cbay(*)
      integer(c_int), value, intent(in) :: nbits, lcbay
      integer(c_int) :: ires
      integer :: iupm
      character(len=8) :: f_cbay

      f_cbay = transfer(cbay(1:lcbay), f_cbay)

      ires = iupm(f_cbay(1:lcbay), nbits)
    end function iupm_c

    !> Encode an integer into a character string
    !>
    !> Wraps ipkm() subroutine.
    !>
    !> @param cbay - Character string
    !> @param nbyt - Number of bytes of cbay within which to encode ival
    !> @param ival - Value to encode
    !> @param cbay_len - Allocated length of cbay
    !>
    !> @author J. Ator @date 2025-12-09
    recursive subroutine ipkm_c(cbay, nbyt, ival, cbay_len) bind(C, name='ipkm_f')
      character(kind=c_char), intent(out) :: cbay(*)
      integer(c_int), value, intent(in) :: nbyt, ival, cbay_len
      character(len=8) :: f_cbay
      integer :: nbytp1

      call ipkm(f_cbay, nbyt, ival)

      nbytp1 = nbyt + 1  ! add 1 for the null terminator
      call copy_f_c_str(f_cbay, cbay, min(nbytp1, cbay_len))
    end subroutine ipkm_c

    !> Rewind a file to the beginning, or restore the previous status.
    !>
    !> Wraps rewnbf() subroutine.
    !>
    !> @param file_unit - Fortran logical unit number of file.
    !> @param isr - Switch:
    !>   - 0 = Save current file status, then rewind file to beginning with read status
    !>   - 1 = Restore file to previous saved status
    !>
    !> @author Jeff Ator @date 2026-02-13
    recursive subroutine rewnbf_c(file_unit, isr) bind(C, name='rewnbf_f')
      integer(c_int), value, intent(in) :: file_unit, isr

      call rewnbf(file_unit, isr)
    end subroutine rewnbf_c

end module bufr_c2f_interface
