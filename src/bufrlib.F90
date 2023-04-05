!> @file
!> @brief Define signatures to enable a number of C NCEPLIBS-bufr functions to be called
!> from within the Fortran portion of the library.
!>
!> @author J. Ator @date 2023-03-22

!> This module contains signatures which wrap C NCEPLIBS-bufr functions so they can be
!> called from within the Fortran portion of the library.
!>
!> @author J. Ator @date 2023-03-22

module bufrlib

  use bufr_interface

  interface

    !> @fn bufrlib::openrb_c::openrb_c(nfile,ufile)
    !> Wraps NCEPLIBS-bufr openrb() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine openrb_c( nfile, ufile ) bind(C, name='openrb')
      use iso_c_binding
      character(kind=c_char), intent(in) :: ufile(*)
      integer(c_int), intent(in), value :: nfile
    end subroutine openrb_c

    !> @fn bufrlib::openwb_c::openwb_c(nfile,ufile)
    !> Wraps NCEPLIBS-bufr openwb() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine openwb_c( nfile, ufile ) bind(C, name='openwb')
      use iso_c_binding
      character(kind=c_char), intent(in) :: ufile(*)
      integer(c_int), intent(in), value :: nfile
    end subroutine openwb_c

    !> @fn bufrlib::openab_c::openab_c(nfile,ufile)
    !> Wraps NCEPLIBS-bufr openab() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine openab_c( nfile, ufile ) bind(C, name='openab')
      use iso_c_binding
      character(kind=c_char), intent(in) :: ufile(*)
      integer(c_int), intent(in), value :: nfile
    end subroutine openab_c

    !> @fn bufrlib::backbufr_c::backbufr_c(nfile)
    !> Wraps NCEPLIBS-bufr backbufr() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine backbufr_c( nfile ) bind(C, name='backbufr')
      use iso_c_binding
      integer(c_int), intent(in), value :: nfile
    end subroutine backbufr_c

    !> @fn bufrlib::cewind_c::cewind_c(nfile)
    !> Wraps NCEPLIBS-bufr cewind() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine cewind_c( nfile ) bind(C, name='cewind')
      use iso_c_binding
      integer(c_int), intent(in), value :: nfile
    end subroutine cewind_c

    !> @fn bufrlib::closfb_c::closfb_c(nfile)
    !> Wraps NCEPLIBS-bufr closfb() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine closfb_c( nfile ) bind(C, name='closfb')
      use iso_c_binding
      integer(c_int), intent(in), value :: nfile
    end subroutine closfb_c

    !> @fn bufrlib::crdbufr_c::crdbufr_c(nfile,bufr,mxwrd)
    !> Wraps NCEPLIBS-bufr crdbufr() function.
    !>
    !> @author J. Ator @date 2005-11-29
    integer(c_int) function crdbufr_c( nfile, bufr, mxwrd ) bind(C, name='crdbufr')
      use iso_c_binding
      integer(c_int), intent(out) :: bufr(*)
      integer(c_int), intent(in), value :: nfile, mxwrd
    end function crdbufr_c

    !> @fn bufrlib::cwrbufr_c::cwrbufr_c(nfile,bufr,nwrd)
    !> Wraps NCEPLIBS-bufr cwrbufr() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine cwrbufr_c( nfile, bufr, nwrd ) bind(C, name='cwrbufr')
      use iso_c_binding
      integer(c_int), intent(in) :: bufr(*)
      integer(c_int), intent(in), value :: nfile, nwrd
    end subroutine cwrbufr_c

  end interface

end module bufrlib
