!> @file
!> @brief Enable a number of C NCEPLIBS-bufr functions to be called
!> from within the Fortran part of the library.
!>
!> @author J. Ator @date 2023-03-22

!> Wrap C NCEPLIBS-bufr functions so they can be called from within the Fortran part of the library.
!>
!> @author J. Ator @date 2023-03-22

module bufrlib
  use bufr_interface

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

    !> @fn bufrlib::cpmstabs_c::cpmstabs_c(nmtb,ibfxyn,cbscl,cbsref,cbbw,cbunit,cbmnem,cbelem,
    !>                                     nmtd,idfxyn,cdseq,cdmnem,ndelem,idefxy,maxcd)
    !> Copy master Table B and Table D information.
    !>
    !> Wraps cpmstabs() function.
    !>
    !> @param nmtb - Number of master Table B entries.
    !> @param ibfxyn - WMO bit-wise representations of master Table B FXY numbers.
    !> @param cbscl - Master Table B scale factors.
    !> @param cbsref - Master Table B reference values.
    !> @param cbbw - Master Table B bit widths.
    !> @param cbunit - Master Table B units.
    !> @param cbmnem - Master Table B mnemonics.
    !> @param cbelem - Master Table B element names.
    !> @param nmtd - Number of master Table D entries.
    !> @param idfxyn - WMO bit-wise representations of master Table D FXY numbers.
    !> @param cdseq - Master Table D sequence names.
    !> @param cdmnem - Master Table D mnemonics.
    !> @param ndelem - Number of child descriptors for master Table D sequence.
    !> @param idefxy - WMO bit-wise representations of child descriptors for master Table D sequence.
    !> @param maxcd - Maximum number of child descriptors for a master Table D sequence.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine cpmstabs_c( nmtb, ibfxyn, cbscl, cbsref, cbbw, cbunit, cbmnem, cbelem, &
                           nmtd, idfxyn, cdseq, cdmnem, ndelem, idefxy, maxcd ) bind(C, name='cpmstabs')
      use iso_c_binding
      integer(c_int), intent(in) :: ibfxyn(*), idfxyn(*), ndelem(*), idefxy(*)
      integer(c_int), intent(in), value :: nmtb, nmtd, maxcd
      character(kind=c_char), intent(in) :: cbscl(4,*), cbsref(12,*), cbbw(4,*), cbunit(24,*), &
                                            cbmnem(8,*), cbelem(120,*), cdseq(120,*), cdmnem(8,*)
    end subroutine cpmstabs_c

    !> @fn bufrlib::inittbf_c::inittbf_c()
    !> Initialize memory for internal storage of master Code/Flag table entries.
    !>
    !> Wraps inittbf() function.
    !>
    !> @author J. Ator @date 2017-11-03
    subroutine inittbf_c() bind(C, name='inittbf')
      use iso_c_binding
    end subroutine inittbf_c

    !> @fn bufrlib::strtbfe_c::strtbfe_c(ifxyn,ival,meaning,lmeaning,idfxy,idval)
    !> Store a new master Code/Flag table entry.
    !>
    !> Wraps strtbfe() function.
    !>
    !> @param ifxyn - WMO bit-wise representation of FXY number for which ival is a defined
    !> code or flag table entry.
    !> @param ival  - Code figure or bit number.
    !> @param meaning - Meaning associated with ifxyn and ival.
    !> @param lmeaning - Length (in bytes) of meaning.
    !> @param idfxy - WMO bit-wise representation of FXY number upon which ifxyn and ival
    !> depend (if any), or else set to a value of (-1).
    !> @param idval - Code figure or bit number associated with idfxy and upon which ifxyn
    !> and ival depend (if any), or else set to (-1) whenever idfxy is also set to (-1).
    !>
    !> @author J. Ator @date 2017-11-03
    subroutine strtbfe_c(ifxyn,ival,meaning,lmeaning,idfxy,idval) bind(C, name='strtbfe')
      use iso_c_binding
      integer(c_int), intent(in), value :: ifxyn, ival, lmeaning, idfxy, idval
      character(kind=c_char), intent(in) :: meaning(*)
    end subroutine strtbfe_c

    !> @fn bufrlib::sorttbf_c::sorttbf_c()
    !> Sort entries within the master Code/Flag table.
    !>
    !> Wraps sorttbf() function.
    !>
    !> @author J. Ator @date 2017-11-03
    subroutine sorttbf_c() bind(C, name='sorttbf')
      use iso_c_binding
    end subroutine sorttbf_c

    !> @fn bufrlib::srchtbf_c::srchtbf_c(ifxyi,ivali,ifxyd,mxfxyd,ivald,meaning,mxmng,lnmng,iret)
    !> Search for a specified master Code/Flag table entry.
    !>
    !> Wraps srchtbf() function.
    !>
    !> @param ifxyi - WMO bit-wise representation of FXY number to search for.
    !> @param ivali - Value (code figure or bit number) associated with ifxyi.
    !> @param ifxyd - Dependence indicator:
    !> - On input, ifxyd[0] is set to the WMO bit-wise representation of the FXY
    !> number upon which ifxyi and ivali depend, or else set to (-1) if ifxyi
    !> and ivali do not depend on the value associated with any other FXY number.
    !> - On output, if the initial search of the master Code/Flag table was
    !> unsuccessful, <b>and</b> if ifxyd[0] and ivald were both set to (-1) on
    !> input, <b>and</b> if a second search of the table determines that the
    !> meaning of ifxyi and ivali indeed depends on one or more other FXY numbers,
    !> then the WMO bit-wise representations of those FXY numbers are returned within
    !> the first iret elements of ifxyd.
    !> @param ivald - Value (code figure or bit number) associated with the FXY
    !> number in ifxyd[0]; set to (-1) whenever ifxyd[0] is also set to (-1).
    !> @param mxfxyd - Number of elements in ifxyd array; used by the function to
    !> ensure that it doesn't overflow the array.
    !> @param mxmng - Number of elements in meaning array; used by the function to
    !> ensure that it doesn't overflow the string.
    !> @param meaning - Meaning corresponding to ifxyi and ivali (and to ifxyd[0]
    !> and ivald, if specified on input).
    !> @param lnmng - Length (in bytes) of string returned in CMEANG.
    !> @param iret - Return code:
    !> -  0 = Meaning found and stored in meaning string.
    !> - -1 = Meaning not found.
    !> - >0 = Meaning not found, <b>and</b> ifxyd[0] and ivald were both set to (-1)
    !> on input, <b>and</b> the meaning of ifxyi and ivali depends on the the value
    !> associated with one of the FXY numbers whose WMO bit-wise representation is
    !> stored in the first iret elements of ifxyd.
    !>
    !> @author J. Ator @date 2017-11-03
    subroutine srchtbf_c(ifxyi,ivali,ifxyd,mxfxyd,ivald,meaning,mxmng,lnmng,iret) bind(C, name='srchtbf')
      use iso_c_binding
      integer(c_int), intent(in), value :: ifxyi, ivali, mxfxyd, ivald, mxmng
      integer(c_int), intent(inout) :: ifxyd
      integer(c_int), intent(out) :: lnmng, iret
      character(kind=c_char), intent(out) :: meaning(*)
    end subroutine srchtbf_c

    !> @fn bufrlib::restd_c::restd_c(lun,tddesc,nctddesc,ctddesc)
    !> Standardize a local Table D descriptor.
    !>
    !> Wraps restd() function.
    !>
    !> @param lun - File ID.
    !> @param tddesc - WMO bit-wise representation of FXY value for local Table D descriptor.
    !> @param nctddesc - Number of WMO-standard child descriptors returned in cttdesc.
    !> @param ctddesc - Array of WMO-standard child descriptors equivalent to tddesc.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine restd_c(lun, tddesc, nctddesc, ctddesc) bind(C, name='restd')
      use iso_c_binding
      integer(c_int), intent(in), value :: lun, tddesc
      integer(c_int), intent(out) :: nctddesc, ctddesc(*)
    end subroutine restd_c

    !> @fn bufrlib::stseq_c::stseq_c(lun,irepct,idn,nemo,cseq,cdesc,ncdesc)
    !> Store information about a standard Table D descriptor.
    !>
    !> Wraps stseq() function.
    !>
    !> @param lun - File ID.
    !> @param irepct - Replication sequence counter for the current master table.
    !> @param idn - WMO bit-wise representation of FXY value for standard Table D descriptor.
    !> @param nemo - Mnemonic corresponding to idn.
    !> @param cseq - Description corresponding to idn.
    !> @param cdesc - Array of WMO-standard child descriptors equivalent to idn.
    !> @param ncdesc - Number of WMO-standard child descriptors in cdesc.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine stseq_c(lun,irepct,idn,nemo,cseq,cdesc,ncdesc) bind(C, name='stseq')
      use iso_c_binding
      integer(c_int), intent(in), value :: lun, idn, ncdesc
      character(kind=c_char), intent(in) :: nemo(*), cseq(*)
      integer(c_int), intent(inout) :: irepct
      integer(c_int), intent(out) :: cdesc(*)
    end subroutine stseq_c

    !> @fn bufrlib::bort_exit_c::bort_exit_c()
    !> Abort the application program.
    !>
    !> Wraps bort_exit() function.
    !>
    !> @author J. Ator @date 2023-04-07
    subroutine bort_exit_c() bind(C, name='bort_exit')
      use iso_c_binding
    end subroutine bort_exit_c

  end interface

end module bufrlib
