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
    !> @returns 1-dimensional index.
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

    !> @fn bufrlib::bort_goto_target_c::bort_goto_target_c()
    !> Return to the previously-set target location after a bort error.
    !>
    !> Wraps bort_goto_target() function.
    !>
    !> @author J. Ator @date 2025-08-20
    subroutine bort_goto_target_c() bind(C, name='bort_goto_target')
      use iso_c_binding
    end subroutine bort_goto_target_c

    !> @fn bufrlib::catch_bort_openbf_c::catch_bort_openbf_c(lunit,cio,lundx,cio_str_len)
    !> Catch any bort error inside of subroutine openbf().
    !>
    !> Wraps catch_bort_openbf() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param cio - Flag indicating how lunit is to be used by the software
    !> @param lundx - Fortran logical unit number containing DX BUFR table information
    !> @param cio_str_len - Length of cio string
    !>
    !> @author J. Ator @date 2025-09-05
    subroutine catch_bort_openbf_c(lunit,cio,lundx,cio_str_len) bind(C, name='catch_bort_openbf')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, lundx, cio_str_len
      character(kind=c_char), intent(inout) :: cio(*)
    end subroutine catch_bort_openbf_c

    !> @fn bufrlib::catch_bort_closbf_c::catch_bort_closbf_c(lunit)
    !> Catch any bort error inside of subroutine closbf().
    !>
    !> Wraps catch_bort_closbf() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !>
    !> @author J. Ator @date 2025-09-05
    subroutine catch_bort_closbf_c(lunit) bind(C, name='catch_bort_closbf')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit
    end subroutine catch_bort_closbf_c

    !> @fn bufrlib::catch_bort_status_c::catch_bort_status_c(lunit,lun,il,im)
    !> Catch any bort error inside of subroutine status().
    !>
    !> Wraps catch_bort_status() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param lun - File ID associated with lunit
    !> @param il - File status
    !> @param im - Message status
    !>
    !> @author J. Ator @date 2025-10-24
    subroutine catch_bort_status_c(lunit,lun,il,im) bind(C, name='catch_bort_status')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: lun, il, im
    end subroutine catch_bort_status_c

    !> @fn bufrlib::catch_bort_readmg_c::catch_bort_readmg_c(lunxx,subset,jdate,subset_str_len,iret)
    !> Catch any bort error inside of subroutine readmg().
    !>
    !> Wraps catch_bort_readmg() function.
    !>
    !> @param lunxx - Absolute value is Fortran logical unit number for BUFR file
    !> @param subset - Table A mnemonic for type of BUFR message that was read
    !> @param jdate - Date-time stored within Section 1 of BUFR message that was read
    !> @param subset_str_len - Allocated length of subset string
    !> @param iret - Return code
    !>
    !> @author J. Ator @date 2025-08-20
    subroutine catch_bort_readmg_c(lunxx,subset,jdate,subset_str_len,iret) bind(C, name='catch_bort_readmg')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunxx, subset_str_len
      character(kind=c_char), intent(out) :: subset(*)
      integer(c_int), intent(out) :: jdate, iret
    end subroutine catch_bort_readmg_c

    !> @fn bufrlib::catch_bort_openmb_c::catch_bort_openmb_c(lunit,subset,subset_str_len,jdate)
    !> Catch any bort error inside of subroutine openmb().
    !>
    !> Wraps catch_bort_openmb() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param subset - Table A mnemonic for type of BUFR message to be written
    !> @param subset_str_len - Length of subset string
    !> @param jdate - Date-time to be written into Section 1 of BUFR message
    !>
    !> @author J. Ator @date 2025-10-20
    subroutine catch_bort_openmb_c(lunit,subset,subset_str_len,jdate) bind(C, name='catch_bort_openmb')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, subset_str_len, jdate
      character(kind=c_char), intent(inout) :: subset(*)
    end subroutine catch_bort_openmb_c

    !> @fn bufrlib::catch_bort_openmg_c::catch_bort_openmg_c(lunit,subset,subset_str_len,jdate)
    !> Catch any bort error inside of subroutine openmg().
    !>
    !> Wraps catch_bort_openmg() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param subset - Table A mnemonic for type of BUFR message to be written
    !> @param subset_str_len - Length of subset string
    !> @param jdate - Date-time to be written into Section 1 of BUFR message
    !>
    !> @author J. Ator @date 2025-10-20
    subroutine catch_bort_openmg_c(lunit,subset,subset_str_len,jdate) bind(C, name='catch_bort_openmg')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, subset_str_len, jdate
      character(kind=c_char), intent(inout) :: subset(*)
    end subroutine catch_bort_openmg_c

    !> @fn bufrlib::catch_bort_readns_c::catch_bort_readns_c(lunit,subset,jdate,subset_str_len,iret)
    !> Catch any bort error inside of subroutine readns().
    !>
    !> Wraps catch_bort_readns() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param subset - Table A mnemonic for type of BUFR message that was read
    !> @param jdate - Date-time stored within Section 1 of BUFR message that was read
    !> @param subset_str_len - Allocated length of subset string
    !> @param iret - Return code
    !>
    !> @author J. Ator @date 2025-08-20
    subroutine catch_bort_readns_c(lunit,subset,jdate,subset_str_len,iret) bind(C, name='catch_bort_readns')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, subset_str_len
      character(kind=c_char), intent(out) :: subset(*)
      integer(c_int), intent(out) :: jdate, iret
    end subroutine catch_bort_readns_c

    !> @fn bufrlib::catch_bort_readsb_c::catch_bort_readsb_c(lunit,iret)
    !> Catch any bort error inside of subroutine readsb().
    !>
    !> Wraps catch_bort_readsb() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param iret - Return code
    !>
    !> @author J. Ator @date 2025-08-20
    subroutine catch_bort_readsb_c(lunit,iret) bind(C, name='catch_bort_readsb')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: iret
    end subroutine catch_bort_readsb_c

    !> @fn bufrlib::catch_bort_writsb_c::catch_bort_writsb_c(lunit)
    !> Catch any bort error inside of subroutine writsb().
    !>
    !> Wraps catch_bort_writsb() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !>
    !> @author J. Ator @date 2025-10-20
    subroutine catch_bort_writsb_c(lunit) bind(C, name='catch_bort_writsb')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit
    end subroutine catch_bort_writsb_c

    !> @fn bufrlib::catch_bort_writsa_c::catch_bort_writsa_c(lunxx,bufr_len,bufr,nbufr)
    !> Catch any bort error inside of subroutine writsa().
    !>
    !> Wraps catch_bort_writsa() function.
    !>
    !> @param lunxx - Absolute value is Fortran logical unit number for BUFR file
    !> @param bufr_len - Allocated length of bufr array
    !> @param bufr - BUFR message
    !> @param nbufr - Number of integers returned in bufr array, or 0 if no message was returned
    !>
    !> @author J. Ator @date 2025-10-20
    subroutine catch_bort_writsa_c(lunxx,bufr_len,bufr,nbufr) bind(C, name='catch_bort_writsa')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunxx, bufr_len
      integer(c_int), intent(out) :: bufr(*), nbufr
    end subroutine catch_bort_writsa_c

    !> @fn bufrlib::catch_bort_ufbint_c::catch_bort_ufbint_c(lunin,usr,i1,i2,iret,cstr,cstr_len)
    !> Catch any bort error inside of subroutine ufbint().
    !>
    !> Wraps catch_bort_ufbint() function.
    !>
    !> @param lunin - Absolute value is Fortran logical unit number for BUFR file
    !> @param usr - Data values
    !> @param i1 - First dimension of usr
    !> @param i2 - Second dimension of usr
    !> @param iret - Number of replications of cstr that were read/written from/to the data subset
    !> @param cstr - String of mnemonics to read/write from/to the data subset
    !> @param cstr_len - Length of cstr
    !>
    !> @author J. Ator @date 2025-09-22
    subroutine catch_bort_ufbint_c(lunin,usr,i1,i2,iret,cstr,cstr_len) bind(C, name='catch_bort_ufbint')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunin, i1, i2, cstr_len
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(inout) :: cstr(*)
      real(c_double), intent(inout) :: usr(i1,*)
    end subroutine catch_bort_ufbint_c

    !> @fn bufrlib::catch_bort_ufbrep_c::catch_bort_ufbrep_c(lunin,usr,i1,i2,iret,cstr,cstr_len)
    !> Catch any bort error inside of subroutine ufbrep().
    !>
    !> Wraps catch_bort_ufbrep() function.
    !>
    !> @param lunin - Absolute value is Fortran logical unit number for BUFR file
    !> @param usr - Data values
    !> @param i1 - First dimension of usr
    !> @param i2 - Second dimension of usr
    !> @param iret - Number of replications of cstr that were read/written from/to the data subset
    !> @param cstr - String of mnemonics to read/write from/to the data subset
    !> @param cstr_len - Length of cstr
    !>
    !> @author J. Ator @date 2025-10-06
    subroutine catch_bort_ufbrep_c(lunin,usr,i1,i2,iret,cstr,cstr_len) bind(C, name='catch_bort_ufbrep')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunin, i1, i2, cstr_len
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(inout) :: cstr(*)
      real(c_double), intent(inout) :: usr(i1,*)
    end subroutine catch_bort_ufbrep_c

    !> @fn bufrlib::catch_bort_ufbstp_c::catch_bort_ufbstp_c(lunin,usr,i1,i2,iret,cstr,cstr_len)
    !> Catch any bort error inside of subroutine ufbstp().
    !>
    !> Wraps catch_bort_ufbstp() function.
    !>
    !> @param lunin - Absolute value is Fortran logical unit number for BUFR file
    !> @param usr - Data values
    !> @param i1 - First dimension of usr
    !> @param i2 - Second dimension of usr
    !> @param iret - Number of replications of cstr that were read/written from/to the data subset
    !> @param cstr - String of mnemonics to read/write from/to the data subset
    !> @param cstr_len - Length of cstr
    !>
    !> @author J. Ator @date 2025-10-24
    subroutine catch_bort_ufbstp_c(lunin,usr,i1,i2,iret,cstr,cstr_len) bind(C, name='catch_bort_ufbstp')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunin, i1, i2, cstr_len
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(inout) :: cstr(*)
      real(c_double), intent(inout) :: usr(i1,*)
    end subroutine catch_bort_ufbstp_c

    !> @fn bufrlib::catch_bort_ufbevn_c::catch_bort_ufbevn_c(lunit,usr,i1,i2,i3,iret,cstr,cstr_len)
    !> Catch any bort error inside of subroutine ufbevn().
    !>
    !> Wraps catch_bort_ufbevn() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param usr - Data values
    !> @param i1 - First dimension of usr
    !> @param i2 - Second dimension of usr
    !> @param i3 - Third dimension of usr
    !> @param iret - Number of replications of cstr that were read from the data subset
    !> @param cstr - String of mnemonics to read from the data subset
    !> @param cstr_len - Length of cstr
    !>
    !> @author J. Ator @date 2025-11-05
    subroutine catch_bort_ufbevn_c(lunit,usr,i1,i2,i3,iret,cstr,cstr_len) bind(C, name='catch_bort_ufbevn')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, i1, i2, i3, cstr_len
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(inout) :: cstr(*)
      real(c_double), intent(out) :: usr(i1,i2,*)
    end subroutine catch_bort_ufbevn_c

    !> @fn bufrlib::catch_bort_drfini_c::catch_bort_drfini_c(lunit,mdrf,ndrf,drftag,drftag_len)
    !> Catch any bort error inside of subroutine drfini().
    !>
    !> Wraps catch_bort_drfini() function.
    !>
    !> @param lunit - Fortran logical unit number to write to
    !> @param mdrf - Array of delayed replication factors
    !> @param ndrf - Number of delayed replication factors in mdrf
    !> @param drftag - Table D mnemonic
    !> @param drftag_len - Length of drftag
    !>
    !> @author Jeff Ator @date 2025-10-28
    subroutine catch_bort_drfini_c(lunit,mdrf,ndrf,drftag,drftag_len) bind(C, name='catch_bort_drfini')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, ndrf, drftag_len
      integer(c_int), intent(in) :: mdrf(*)
      character(kind=c_char), intent(inout) :: drftag(*)
    end subroutine catch_bort_drfini_c

    !> @fn bufrlib::catch_bort_ufbseq_c::catch_bort_ufbseq_c(lunin,usr,i1,i2,iret,cstr,cstr_len)
    !> Catch any bort error inside of subroutine ufbseq().
    !>
    !> Wraps catch_bort_ufbseq() function.
    !>
    !> @param lunin - Absolute value is Fortran logical unit number for BUFR file
    !> @param usr - Data values
    !> @param i1 - First dimension of usr
    !> @param i2 - Second dimension of usr
    !> @param iret - Number of replications of cstr that were read/written from/to the data subset
    !> @param cstr - Mnemonic describing sequence to read/write from/to the data subset
    !> @param cstr_len - Length of cstr
    !>
    !> @author J. Ator @date 2025-10-06
    subroutine catch_bort_ufbseq_c(lunin,usr,i1,i2,iret,cstr,cstr_len) bind(C, name='catch_bort_ufbseq')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunin, i1, i2, cstr_len
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(inout) :: cstr(*)
      real(c_double), intent(inout) :: usr(i1,*)
    end subroutine catch_bort_ufbseq_c

    !> @fn bufrlib::catch_bort_readlc_c::catch_bort_readlc_c(lunit,cstr,cstr_len,chr,chr_len,nchr)
    !> Catch any bort error inside of subroutine readlc().
    !>
    !> Wraps catch_bort_readlc() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param cstr - Mnemonic of long character string to read from data subset
    !> @param cstr_len - Length of cstr
    !> @param chr - Long character string corresponding to cstr
    !> @param chr_len - Allocated length of chr
    !> @param nchr - Number of characters returned in chr
    !>
    !> @author J. Ator @date 2025-10-15
    subroutine catch_bort_readlc_c(lunit,cstr,cstr_len,chr,chr_len,nchr) bind(C, name='catch_bort_readlc')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, cstr_len, chr_len
      integer(c_int), intent(out) :: nchr
      character(kind=c_char), intent(inout) :: cstr(*)
      character(kind=c_char), intent(out) :: chr(*)
    end subroutine catch_bort_readlc_c

    !> @fn bufrlib::catch_bort_writlc_c::catch_bort_writlc_c(lunit,cstr,cstr_len,cchr,cchr_len)
    !> Catch any bort error inside of subroutine writlc().
    !>
    !> Wraps catch_bort_writlc() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param cstr - Mnemonic of long character string to write to data subset
    !> @param cstr_len - Length of cstr
    !> @param cchr - Long character string corresponding to cstr
    !> @param cchr_len - Length of cchr
    !>
    !> @author J. Ator @date 2025-10-24
    subroutine catch_bort_writlc_c(lunit,cstr,cstr_len,cchr,cchr_len) bind(C, name='catch_bort_writlc')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, cstr_len, cchr_len
      character(kind=c_char), intent(inout) :: cstr(*), cchr(*)
    end subroutine catch_bort_writlc_c

    !> @fn bufrlib::catch_bort_ufbcnt_c::catch_bort_ufbcnt_c(lunit,kmsg,ksub)
    !> Catch any bort error inside of subroutine ufbcnt().
    !>
    !> Wraps catch_bort_ufbcnt() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param kmsg - Message number
    !> @param ksub - Subset number
    !>
    !> @author J. Ator @date 2025-11-05
    subroutine catch_bort_ufbcnt_c(lunit,kmsg,ksub) bind(C, name='catch_bort_ufbcnt')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: kmsg, ksub
    end subroutine catch_bort_ufbcnt_c

    !> @fn bufrlib::catch_bort_ufbqcd_c::catch_bort_ufbqcd_c(lunit,cnemo,iqcd,cnemo_len)
    !> Catch any bort error inside of subroutine ufbqcd().
    !>
    !> Wraps catch_bort_ufbqcd() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param cnemo - Mnemonic associated with a Category 63 Table D descriptor
    !> @param iqcd - Y value of descriptor associated with mnemonic
    !> @param cnemo_len - Length of cnemo
    !>
    !> @author J. Ator @date 2025-11-05
    subroutine catch_bort_ufbqcd_c(lunit,cnemo,iqcd,cnemo_len) bind(C, name='catch_bort_ufbqcd')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, cnemo_len
      integer(c_int), intent(out) :: iqcd
      character(kind=c_char), intent(inout) :: cnemo(*)
    end subroutine catch_bort_ufbqcd_c

    !> @fn bufrlib::catch_bort_ufbqcp_c::catch_bort_ufbqcp_c(lunit,iqcp,cnemo,cnemo_len,ncn)
    !> Catch any bort error inside of subroutine ufbqcp().
    !>
    !> Wraps catch_bort_ufbqcp() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param iqcp - Y value of a Category 63 Table D descriptor
    !> @param cnemo - Mnemonic associated with iqcp
    !> @param cnemo_len - Allocated length of cnemo string
    !> @param ncn - Number of characters returned in cnemo
    !>
    !> @author J. Ator @date 2025-11-05
    subroutine catch_bort_ufbqcp_c(lunit,iqcp,cnemo,cnemo_len,ncn) bind(C, name='catch_bort_ufbqcp')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, iqcp, cnemo_len
      integer(c_int), intent(out) :: ncn
      character(kind=c_char), intent(out) :: cnemo(*)
    end subroutine catch_bort_ufbqcp_c

    !> @fn bufrlib::catch_bort_getcfmng_c::catch_bort_getcfmng_c(lunit,cnemoi,lcni,ivali,cnemod,lcnd,ivald,cmeang_c,lcmgc,lnmng,iret)
    !> Catch any bort error inside of subroutine getcfmng().
    !>
    !> Wraps catch_bort_getcfmng() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param cnemoi - Mnemonic to search for
    !> @param lcni - Length of cnemoi
    !> @param ivali - Value associated with cnemoi
    !> @param cnemod - Optional second mnemonic upon which cnemoi may depend
    !> @param lcnd - Length of cnemod
    !> @param ivald - Value associated with cnemod
    !> @param cmeang_c - Meaning associated with cnemoi and ivali (and possibly cnemod and ivald as well)
    !> @param lcmgc - Allocated length of cmeang_c
    !> @param lnmng - Number of characters returned in cmeang_c
    !> @param iret - Return code from call to getcfmng_f
    !>
    !> @author J. Ator @date 2025-11-05
    subroutine catch_bort_getcfmng_c(lunit,cnemoi,lcni,ivali,cnemod,lcnd,ivald,cmeang_c,lcmgc,lnmng,iret) &
        bind(C, name='catch_bort_getcfmng')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, lcni, ivali, lcnd, ivald, lcmgc
      integer(c_int), intent(out) :: lnmng, iret
      character(kind=c_char), intent(inout) :: cnemoi(*), cnemod(*)
      character(kind=c_char), intent(out) :: cmeang_c(*)
    end subroutine catch_bort_getcfmng_c

    !> @fn bufrlib::catch_bort_upftbv_c::catch_bort_upftbv_c(lunit,cnemo,lcn,val,ibit,mxib,nib)
    !> Catch any bort error inside of subroutine upftbv().
    !>
    !> Wraps catch_bort_upftbv() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param cnemo - Mnemonic with flag table units
    !> @param lcn - Length of cnemo
    !> @param val - Value corresponding to cnemo
    !> @param ibit - Bit numbers which were set to "On" in val
    !> @param mxib - Allocated size of ibit
    !> @param nib - Number of bit numbers returned in ibit
    !>
    !> @author J. Ator @date 2025-11-05
    subroutine catch_bort_upftbv_c(lunit,cnemo,lcn,val,ibit,mxib,nib) bind(C, name='catch_bort_upftbv')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, lcn, mxib
      integer(c_int), intent(out) :: ibit(*), nib
      real(c_double), value, intent(in) :: val
      character(kind=c_char), intent(inout) :: cnemo(*)
    end subroutine catch_bort_upftbv_c

    !> @fn bufrlib::catch_bort_ufbtab_c::catch_bort_ufbtab_c(lunin,tab,i1,i2,iret,cstr,cstr_len)
    !> Catch any bort error inside of subroutine ufbtab().
    !>
    !> Wraps catch_bort_ufbtab() function.
    !>
    !> @param lunin - Absolute value is Fortran logical unit number for BUFR file
    !> @param tab - Data values
    !> @param i1 - First dimension of tab
    !> @param i2 - Second dimension of tab
    !> @param iret - Number of data subsets returned
    !> @param cstr - String of mnemonics to read from each data subset
    !> @param cstr_len - Length of cstr
    !>
    !> @author J. Ator @date 2025-11-13
    subroutine catch_bort_ufbtab_c(lunin,tab,i1,i2,iret,cstr,cstr_len) bind(C, name='catch_bort_ufbtab')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunin, i1, i2, cstr_len
      integer(c_int), intent(inout) :: iret
      character(kind=c_char), intent(inout) :: cstr(*)
      real(c_double), intent(inout) :: tab(i1,*)
    end subroutine catch_bort_ufbtab_c

    !> @fn bufrlib::catch_bort_ufbpos_c::catch_bort_ufbpos_c(lunit,irec,isub,subset,jdate,subset_str_len)
    !> Catch any bort error inside of subroutine ufbpos().
    !>
    !> Wraps catch_bort_ufbpos() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param irec - Ordinal number of message to be read
    !> @param isub - Ordinal number of subset to be read from (irec)th message
    !> @param subset - Table A mnemonic for type of BUFR message that was read
    !> @param jdate - Date-time stored within Section 1 of BUFR message that was read
    !> @param subset_str_len - Allocated length of subset string
    !>
    !> @author J. Ator @date 2025-11-13
    subroutine catch_bort_ufbpos_c(lunit,irec,isub,subset,jdate,subset_str_len) bind(C, name='catch_bort_ufbpos')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, subset_str_len, irec, isub
      character(kind=c_char), intent(out) :: subset(*)
      integer(c_int), intent(out) :: jdate
    end subroutine catch_bort_ufbpos_c

    !> @fn bufrlib::catch_bort_datelen_c::catch_bort_datelen_c(len)
    !> Catch any bort error inside of subroutine datelen().
    !>
    !> Wraps catch_bort_datelen() function.
    !>
    !> @param len - Length of Section 1 date-time values to be output by all future calls to message-reading subroutines
    !>
    !> @author J. Ator @date 2025-11-14
    subroutine catch_bort_datelen_c(len) bind(C, name='catch_bort_datelen')
      use iso_c_binding
      integer(c_int), value, intent(in) :: len
    end subroutine catch_bort_datelen_c

    !> @fn bufrlib::catch_bort_iupvs01_c::catch_bort_iupvs01_c(lunit,s01mnem,s01mnem_str_len,iret)
    !> Catch any bort error inside of function iupvs01().
    !>
    !> Wraps catch_bort_iupvs01() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param s01mnem - Mnemonic for value to be read from Section 0 or Section 1 of BUFR message
    !> @param s01mnem_str_len - Length of s01mnem string
    !> @param iret - Value corresponding to s01mnem
    !>
    !> @author J. Ator @date 2025-11-14
    !>
    subroutine catch_bort_iupvs01_c(lunit,s01mnem,s01mnem_str_len,iret) bind(C, name='catch_bort_iupvs01')
      use iso_c_binding
      integer(c_int), intent(in), value :: lunit, s01mnem_str_len
      integer(c_int), intent(out) :: iret
      character(kind=c_char), intent(inout) :: s01mnem(*)
    end subroutine catch_bort_iupvs01_c

    !> @fn bufrlib::catch_bort_nmsub_c::catch_bort_nmsub_c(lunit,iret)
    !> Catch any bort error inside of function nmsub().
    !>
    !> Wraps catch_bort_nmsub() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param iret - Number of data subsets
    !>
    !> @author J. Ator @date 2025-11-14
    subroutine catch_bort_nmsub_c(lunit,iret) bind(C, name='catch_bort_nmsub')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: iret
    end subroutine catch_bort_nmsub_c

    !> @fn bufrlib::catch_bort_pkvs01_c::catch_bort_pkvs01_c(s01mnem,s01mnem_str_len,ival)
    !> Catch any bort error inside of subroutine pkvs01().
    !>
    !> Wraps catch_bort_pkvs01() function.
    !>
    !> @param s01mnem - Mnemonic for value to be written into Section 0 or Section 1 of BUFR message
    !> @param s01mnem_str_len - Length of s01mnem string
    !> @param ival - Value corresponding to s01mnem
    !>
    !> @author J. Ator @date 2025-11-14
    !>
    subroutine catch_bort_pkvs01_c(s01mnem,s01mnem_str_len,ival) bind(C, name='catch_bort_pkvs01')
      use iso_c_binding
      integer(c_int), intent(in), value :: s01mnem_str_len, ival
      character(kind=c_char), intent(inout) :: s01mnem(*)
    end subroutine catch_bort_pkvs01_c

    !> @fn bufrlib::catch_bort_datebf_c::catch_bort_datebf_c(lunit,mear,mmon,mday,mour,idate)
    !> Catch any bort error inside of subroutine datebf().
    !>
    !> Wraps catch_bort_datebf() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param mear - Year stored within Section 1 of first data message
    !> @param mmon - Month stored within Section 1 of first data message
    !> @param mday - Day stored within Section 1 of first data message
    !> @param mour - Hour stored within Section 1 of first data message
    !> @param idate - Date-time stored within Section 1 of first data message
    !>
    !> @author J. Ator @date 2025-11-18
    subroutine catch_bort_datebf_c(lunit,mear,mmon,mday,mour,idate) bind(C, name='catch_bort_datebf')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: mear, mmon, mday, mour, idate
    end subroutine catch_bort_datebf_c

    !> @fn bufrlib::catch_bort_dumpbf_c::catch_bort_dumpbf_c(lunit,jdate,jdump)
    !> Catch any bort error inside of subroutine dumpbf().
    !>
    !> Wraps catch_bort_dumpbf() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param jdate - Dump center date-time stored within Section 1 of first "dummy" message
    !> @param jdump - Dump initiation date-time stored within Section 1 of second "dummy" message
    !>
    !> @author J. Ator @date 2025-11-18
    subroutine catch_bort_dumpbf_c(lunit,jdate,jdump) bind(C, name='catch_bort_dumpbf')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit
      integer(c_int), intent(out) :: jdate(*), jdump(*)
    end subroutine catch_bort_dumpbf_c

    !> @fn bufrlib::catch_bort_minimg_c::catch_bort_minimg_c(lunit,mini)
    !> Catch any bort error inside of subroutine minimg().
    !>
    !> Wraps catch_bort_minimg() function.
    !>
    !> @param lunit - Fortran logical unit number for BUFR file
    !> @param mini - Minutes value
    !>
    !> @author J. Ator @date 2025-11-18
    subroutine catch_bort_minimg_c(lunit,mini) bind(C, name='catch_bort_minimg')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lunit, mini
    end subroutine catch_bort_minimg_c

    !> @fn bufrlib::catch_bort_upds3_c::catch_bort_upds3_c(mbay,lcds3,cds3,nds3)
    !> Catch any bort error inside of subroutine upds3().
    !>
    !> Wraps catch_bort_upds3() function.
    !>
    !> @param mbay - BUFR message
    !> @param lcds3 - Allocated length of cds3
    !> @param ccds3 - Data descriptor sequence within Section 3 of mbay
    !> @param nds3 - Number of descriptors returned in cds3
    !>
    !> @author J. Ator @date 2025-11-18
    subroutine catch_bort_upds3_c(mbay,lcds3,ccds3,nds3) bind(C, name='catch_bort_upds3')
      use iso_c_binding
      integer(c_int), value, intent(in) :: lcds3
      integer(c_int), intent(in) :: mbay(*)
      integer(c_int), intent(out) :: nds3
      character(kind=c_char), intent(out) :: ccds3(6,*)
    end subroutine catch_bort_upds3_c

    !> @fn bufrlib::catch_bort_pkbs1_c::catch_bort_pkbs1_c(ival,mbay,s1mnem,s1mnem_str_len)
    !> Catch any bort error inside of subroutine pkbs1().
    !>
    !> Wraps catch_bort_pkbs1() function.
    !>
    !> @param ival - Value corresponding to s1mnem
    !> @param mbay - BUFR message
    !> @param s1mnem - Mnemonic for value to be written into Section 1 of BUFR message
    !> @param s1mnem_str_len - Length of s1mnem string
    !>
    !> @author J. Ator @date 2025-11-18
    !>
    subroutine catch_bort_pkbs1_c(ival,mbay,s1mnem,s1mnem_str_len) bind(C, name='catch_bort_pkbs1')
      use iso_c_binding
      integer(c_int), intent(in), value :: s1mnem_str_len, ival
      integer(c_int), intent(inout) :: mbay(*)
      character(kind=c_char), intent(inout) :: s1mnem(*)
    end subroutine catch_bort_pkbs1_c

  end interface

end module bufrlib
