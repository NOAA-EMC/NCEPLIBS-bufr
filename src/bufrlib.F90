!> @file
!> @brief Enable a number of C NCEPLIBS-bufr functions to be called
!> from within the Fortran part of the library.
!>
!> @author J. Ator @date 2023-03-22

!> Wrap C NCEPLIBS-bufr functions so they can be called from within the Fortran part of the library.
!>
!> @author J. Ator @date 2023-03-22

module bufrlib

  interface

    !> @fn bufrlib::openrb_c::openrb_c(nfile,ufile)
    !> Open a new file for reading BUFR messages.
    !>
    !> Wraps openrb() function.
    !>
    !> @param nfile - File ID.
    !> @param ufile - [path/]name of file to be opened.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine openrb_c( nfile, ufile ) bind(C, name='openrb')
      use iso_c_binding
      character(kind=c_char), intent(in) :: ufile(*)
      integer(c_int), intent(in), value :: nfile
    end subroutine openrb_c

    !> @fn bufrlib::openwb_c::openwb_c(nfile,ufile)
    !> Open a new file for writing BUFR messages.
    !>
    !> Wraps openwb() function.
    !>
    !> @param nfile - File ID.
    !> @param ufile - [path/]name of file to be opened.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine openwb_c( nfile, ufile ) bind(C, name='openwb')
      use iso_c_binding
      character(kind=c_char), intent(in) :: ufile(*)
      integer(c_int), intent(in), value :: nfile
    end subroutine openwb_c

    !> @fn bufrlib::openab_c::openab_c(nfile,ufile)
    !> Open a new file for appending BUFR messages.
    !>
    !> Wraps openab() function.
    !>
    !> @param nfile - File ID.
    !> @param ufile - [path/]name of file to be opened.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine openab_c( nfile, ufile ) bind(C, name='openab')
      use iso_c_binding
      character(kind=c_char), intent(in) :: ufile(*)
      integer(c_int), intent(in), value :: nfile
    end subroutine openab_c

    !> @fn bufrlib::backbufr_c::backbufr_c(nfile)
    !> Backspace a BUFR file by one BUFR message.
    !>
    !> Wraps backbufr() function.
    !>
    !> @param nfile - File ID.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine backbufr_c( nfile ) bind(C, name='backbufr')
      use iso_c_binding
      integer(c_int), intent(in), value :: nfile
    end subroutine backbufr_c

    !> @fn bufrlib::cewind_c::cewind_c(nfile)
    !> Rewind a BUFR file back to its beginning.
    !>
    !> Wraps cewind() function.
    !>
    !> @param nfile - File ID.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine cewind_c( nfile ) bind(C, name='cewind')
      use iso_c_binding
      integer(c_int), intent(in), value :: nfile
    end subroutine cewind_c

    !> @fn bufrlib::closfb_c::closfb_c(nfile)
    !> Close a previously opened BUFR file.
    !>
    !> Wraps closfb() function.
    !>
    !> @param nfile - File ID.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine closfb_c( nfile ) bind(C, name='closfb')
      use iso_c_binding
      integer(c_int), intent(in), value :: nfile
    end subroutine closfb_c

    !> @fn bufrlib::crdbufr_c::crdbufr_c(nfile,bufr,mxwrd)
    !> Read the next message from a BUFR file that was previously opened for reading.
    !>
    !> Wraps crdbufr() function.
    !>
    !> @param nfile - File ID.
    !> @param bufr - BUFR message.
    !> @param mxwrd - Number of elements in bufr array; used by the function to
    !> ensure that it doesn't overflow the array.
    !>
    !> Return code:
    !> - 0 = normal return
    !> - -1 = end-of-file encountered while reading
    !> - -2 = I/O error encountered while reading
    !> - -3 = overflow of bufr array
    !>
    !> @author J. Ator @date 2005-11-29
    integer(c_int) function crdbufr_c( nfile, bufr, mxwrd ) bind(C, name='crdbufr')
      use iso_c_binding
      integer(c_int), intent(out) :: bufr(*)
      integer(c_int), intent(in), value :: nfile, mxwrd
    end function crdbufr_c

    !> @fn bufrlib::cwrbufr_c::cwrbufr_c(nfile,bufr,nwrd)
    !> Write a BUFR message into a file that was previously opened for writing.
    !>
    !> Wraps cwrbufr() function.
    !>
    !> @param nfile - File ID.
    !> @param bufr - BUFR message.
    !> @param nwrd - Size of BUFR message.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine cwrbufr_c( nfile, bufr, nwrd ) bind(C, name='cwrbufr')
      use iso_c_binding
      integer(c_int), intent(in) :: bufr(*)
      integer(c_int), intent(in), value :: nfile, nwrd
    end subroutine cwrbufr_c

    !> @fn bufrlib::icvidx_c::icvidx_c(ii,jj,numjj)
    !> Compute a 1-dimensional array index from 2-dimensional indices.
    !>
    !> Wraps icvidx() function.
    !>
    !> @author J. Ator
    !> @date 2022-09-01
    !>
    !> @param ii - First (row) index.
    !> @param jj - Second (column) index.
    !> @param numjj - Maximum number of column indices.
    !>
    !> Returns 1-dimensional index.
    !>
    integer(c_int) function icvidx_c( ii, jj, numjj ) bind(C, name='icvidx')
      use iso_c_binding
      integer(c_int), intent(in), value :: ii, jj, numjj
    end function icvidx_c

    !> @fn bufrlib::arallocc_c::arallocc_c()
    !> Dynamically allocate C language arrays.
    !>
    !> Wraps arallocc() function.
    !>
    !> @author J. Ator @date 2014-12-04
    subroutine arallocc_c() bind(C, name='arallocc')
      use iso_c_binding
    end subroutine arallocc_c

    !> @fn bufrlib::ardllocc_c::ardllocc_c()
    !> Free all memory allocated via arallocc_c().
    !>
    !> Wraps ardllocc() function.
    !>
    !> @author J. Ator @date 2014-12-04
    subroutine ardllocc_c() bind(C, name='ardllocc')
      use iso_c_binding
    end subroutine ardllocc_c

  end interface

end module bufrlib
