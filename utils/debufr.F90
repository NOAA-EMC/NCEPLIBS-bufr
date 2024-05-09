!> @file
!> @brief Fortran language code for debufr utility.
!>
!> @author J. Ator @date 2009-07-01

!> This module is used within the debufr utility to share information between subroutine fdebufr_c() and subroutine
!> openbt(), since the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr
!> software.
!>
!> @author J. Ator @date 2009-07-01
module Share_Table_Info

  !> Directory containing DX BUFR tables to be used for decoding.
  character(len=:), allocatable :: tbldir_f

  !> Length (in characters) of tbldir_f.
  integer ltbd

  !> Fortran logical unit number to use for referencing a DX table.
  integer ludx
end module Share_Table_Info

!> This subroutine reads, decodes, and generates a verbose output listing of the contents of every BUFR message from
!> within the input file that was previously opened via a call to function cobfl() with io = 'r'.
!>
!> @param ofile - File to contain verbose output listing of contents of each decoded BUFR message
!> @param lenof - Length of ofile string
!> @param tbldir - Directory containing DX and/or master BUFR tables to be used for decoding
!> @param lentd  - Length of tbldir string
!> @param tblfil - File containing DX BUFR table information to be used for decoding
!>   - 'NULLFILE' = No such file will be used
!> @param lentf  - Length of tblfil string
!> @param prmstg - String of up to 20 comma-separated PARAMETER=VALUE pairs to be used to dynamically allocate memory
!> within the NCEPLIBS-bufr software, overriding the default VALUE that would otherwise be used for each such PARAMETER
!>   - 'NULLPSTG' = No such pairs will be used
!> @param lenps - Length of prmstg string
!> @param basic - Indicator as to whether only "basic" information in Sections 0-3 should be decoded from each BUFR message:
!>   - 'Y' = Yes
!>   - 'N' = No
!> @param forcemt - Indicator as to whether master BUFR tables should be used for decoding, regardless of whether the input
!> file contains any embedded DX BUFR table messages:
!>   - 'Y' = Yes
!>   - 'N' = No
!> @param cfms - Indicator as to whether code and flag table meanings should be read from master BUFR tables and included
!> in the print output:
!>   - 'Y' = Yes
!>   - 'N' = No
!>
!> @remarks
!> - See NCEPLIBS-bufr function isetprm() for a complete list of parameters that can be dynamically sized via prmstg.
!> - Fortran logical unit numbers 51, 90, 91, 92 and 93 are reserved for use within this subroutine.
!>
!> @author J. Ator @date 2009-07-01
subroutine fdebufr_c ( ofile, lenof, tbldir, lentd, tblfil, lentf, prmstg, lenps, basic, forcemt, cfms ) &
    bind ( C, name = 'fdebufr_f' )

  use iso_c_binding
  use bufr_interface
  use Share_Table_Info

  implicit none

  integer, parameter :: mxbf = 2500000
  integer, parameter :: mxbfd4 = mxbf/4
  integer, parameter :: mxds3 = 500
  integer, parameter :: mxprms = 20

  character(kind=c_char,len=1), intent(in) :: ofile(*), tbldir(*), tblfil(*), prmstg(*)

  character(len=:), allocatable :: ofile_f, tblfil_f, prmstg_f

  integer(c_int), value, intent(in) :: lenof, lentd, lentf, lenps

  character(c_char), value, intent(in) :: basic, forcemt, cfms

  integer*4 :: isetprm, idxmsg, iupbs01, iupbs3, ireadsb
  integer*4 :: nbyt, ierr

  logical exists

  character*120 cmorgc, cmgses, cmmtyp, cmmsbt, cmmsbti
  character*20  ptag ( mxprms ), pvtag(2), cprmnm
  character*8   cmgtag
  character*6   cds3 ( mxds3 )
  character     opened, usemt, bfmg ( mxbf ), basic_f, forcemt_f, cfms_f

  integer ibfmg ( mxbfd4 ), lunit, nmsg, nsub, nsubt, ii, jj, nds3, nptag, npvtag, ipval, lcprmnm, ier, imgdt, ierme, &
          iogce, lcmorgc, ierorgc, igses, lcmgses, iergses, iryr, irmo, irdy, irhr, irmi, irtret, &
          mtyp, lcmmtyp, iermtyp, msbt, lcmmsbt, iermsbt, msbti, lcmmsbti, iermsbti, iersn

  equivalence ( bfmg (1), ibfmg (1) )

  ! Initialize the values in the Share_Table_Info module.

  ludx = 93
  ltbd = lentd
  allocate( character(len=lentd) :: tbldir_f )
  tbldir_f = transfer ( tbldir(1:lentd), tbldir_f )

  ! Copy the other input C strings into Fortran strings.

  allocate( character(len=lenof) :: ofile_f )
  ofile_f = transfer ( ofile(1:lenof), ofile_f )
  allocate( character(len=lentf) :: tblfil_f )
  tblfil_f = transfer ( tblfil(1:lentf), tblfil_f )
  allocate( character(len=lenps) :: prmstg_f )
  prmstg_f = transfer ( prmstg(1:lenps), prmstg_f )
  basic_f = basic
  forcemt_f = forcemt
  cfms_f = cfms

  ! Open the output file.

  open ( unit = 51, file = ofile_f )

  ! Note that in the below open statement we just need to specify a dummy placeholder file.

  lunit = 92
  open ( unit = lunit, file = '/dev/null' )

  call datelen ( 10 )

  ! Initialize some other values.

  nmsg = 0
  nsubt = 0

  opened = 'N'
  usemt = 'N'

  do while ( .true. )

    ! Get the next message from the input BUFR file.

    call crbmg_c ( bfmg, mxbf, nbyt, ierr )

    if ( ierr .ne. 0 ) then

      if ( ierr .eq. -1 ) then
        write ( 51, fmt = '( /, A, I7, A, I9, A )') 'Reached end of BUFR file; it contained a total of', nmsg, &
          ' messages and', nsubt, ' subsets'
      else
        write ( 51, fmt = '( /, A, I4 )' ) 'Error while reading BUFR file; the return code from CRBMG = ', ierr
      end if

      if ( ( basic_f .eq. 'N' ) .and. ( opened .eq. 'Y' ) ) then
        write (51, fmt = '( /, A, / )' ) 'Here is the DX table that was generated:'
        call dxdump ( lunit, 51 )
      end if

      ! Close the output file, deallocate memory, and return.

      close ( 51 )
      deallocate ( ofile_f )
      deallocate ( tbldir_f )
      deallocate ( tblfil_f )
      deallocate ( prmstg_f )
      return
    end if

    if ( opened .eq. 'N' ) then

      if ( ( isetprm ( 'MAXCD', mxds3 ) .ne. 0 ) .or. ( isetprm ( 'MXMSGL', mxbf ) .ne. 0 ) .or. &
           ( isetprm ( 'MAXSS', 300000 ) .ne. 0 ) .or. ( isetprm ( 'NFILES', 2 ) .ne. 0 ) ) then
        print *, 'Error: Bad return from isetprm'
        return
      end if

      ! Process any dynamic allocation parameters that were passed in on the command line.

      if ( prmstg_f(1:8) .ne. 'NULLPSTG' ) then
        call parstr ( prmstg_f, ptag, mxprms, nptag, ',', .false. )
        if ( nptag .gt. 0 ) then
          do ii = 1, nptag
            call parstr ( ptag(ii), pvtag, 2, npvtag, '=', .false. )
            if ( npvtag .eq. 2 ) then
              call strsuc ( pvtag(1), cprmnm, lcprmnm )
              call strnum ( pvtag(2), ipval, iersn )
              if ( ( lcprmnm .gt. 0 ) .and. ( iersn .ne. -1 ) ) then
                if ( isetprm ( cprmnm(1:lcprmnm), ipval ) .ne. 0 ) then
                  print *, 'Error: Bad return from isetprm for parameter: ', cprmnm(1:lcprmnm)
                  return
                end if
              end if
            end if
          end do
        end if
      end if

      ! Decide how to process the file.

      if ( ( idxmsg ( ibfmg ) .eq. 1 ) .and. ( forcemt_f .eq. 'N' ) ) then

        ! The first message in the file is a DX dictionary message, so assume there's an embedded table at the
        ! front of the file, and use this table to decode it.

        call openbf ( lunit, 'INUL', lunit )
      else if ( ( tblfil_f(1:8) .ne. 'NULLFILE' ) .and. ( forcemt_f .eq. 'N' ) ) then

        ! A DX dictionary tables file was specified on the command line, so use it to decode the BUFR file.

        inquire ( file = tblfil_f, exist = exists )
        if ( .not. exists ) then
          print *, 'Error: Could not find file ', tblfil_f
          return
        end if
        open ( unit = 91, file = tblfil_f, iostat = ier )
        if ( ier .ne. 0 ) then
          print *, 'Error: Could not open file ', tblfil_f
          return
        endif
        call openbf ( lunit, 'IN', 91 )
      else

        ! Decode the file using the master tables in tbldir_f.

        usemt = 'Y'
        call openbf ( lunit, 'SEC3', lunit )
      end if

      opened = 'Y'

      call mtinfo ( tbldir_f, 90, 91 )
      if ( cfms_f .eq. 'Y' ) then
        print *, 'in debufr.F90, calling codflg with Y'
        call codflg ( 'Y' )
      endif
    end if

    if ( basic_f .eq. 'N' ) then

      ! Pass the message to the decoder.

      call readerme ( ibfmg, lunit, cmgtag, imgdt, ierme )
    end if

    ! If this is a DX dictionary message, then don't generate any output unless master tables are being used for decoding.

    if ( ( idxmsg ( ibfmg ) .ne. 1 ) .or. ( usemt .eq. 'Y' ) ) then

      nmsg = nmsg + 1

      write ( 51, fmt = '( /, A, I7 )' ) 'Found BUFR message #', nmsg

      ! Decode and output the data from Section 0.

      write ( 51, fmt= '( /, A, I9 )' ) '        Message length:   ', iupbs01 ( ibfmg, 'LENM' )
      write ( 51, fmt= '( A, I4 )' ) '      Section 0 length:        ', iupbs01 ( ibfmg, 'LEN0' )
      write ( 51, fmt= '( A, I4 )' ) '          BUFR edition:        ', iupbs01 ( ibfmg, 'BEN' )

      ! Decode and output the data from Section 1.

      write ( 51, fmt= '( /, A, I4 )' ) '      Section 1 length:        ', iupbs01 ( ibfmg, 'LEN1' )
      write ( 51, fmt= '( A, I4 )' ) '          Master table:        ', iupbs01 ( ibfmg, 'BMT' )

      iogce = iupbs01 ( ibfmg, 'OGCE' )
      igses = iupbs01 ( ibfmg, 'GSES' )
      if ( ( basic_f .eq. 'Y' ) .or. ( cfms_f .eq. 'N' ) ) then
        write ( 51, fmt= '( A, I5 )' ) '    Originating center:       ', iogce
        write ( 51, fmt= '( A, I4 )' ) ' Originating subcenter:        ', igses
      else
        print *, 'in debufr.F90, calling getcfmng for ORIGC'
        call getcfmng ( lunit, 'ORIGC', iogce, ' ', -1, cmorgc, lcmorgc, ierorgc )
        if ( ierorgc .eq. 0 ) then
          write ( 51, fmt= '( A, I5, 3A )' ) '    Originating center:       ', iogce, ' (= ', cmorgc(1:lcmorgc), ')'
        else
          write ( 51, fmt= '( A, I5 )' ) '    Originating center:       ', iogce
        end if
        print *, 'in debufr.F90, calling getcfmng for GSES'
        call getcfmng ( lunit, 'GSES', igses, 'ORIGC', iogce, cmgses, lcmgses, iergses )
        if ( iergses .eq. 0 ) then
          write ( 51, fmt= '( A, I4, 3A )' ) ' Originating subcenter:        ', igses, ' (= ', cmgses(1:lcmgses), ')'
        else
          write ( 51, fmt= '( A, I4 )' ) ' Originating subcenter:        ', igses
        end if
      end if

      write ( 51, fmt= '( A, I4 )' ) ' Update sequence numbr:        ',  iupbs01 ( ibfmg, 'USN' )

      if ( iupbs01 ( ibfmg, 'ISC2' ) .eq. 1 ) then
        write ( 51, fmt = '( A )')  '    Section 2 present?: Yes'
      else
        write ( 51, fmt = '( A )')  '    Section 2 present?: No'
      end if

      mtyp = iupbs01 ( ibfmg, 'MTYP' )
      msbt = iupbs01 ( ibfmg, 'MSBT' )
      msbti = iupbs01 ( ibfmg, 'MSBTI' )
      if ( ( basic_f .eq. 'Y' ) .or. ( cfms_f .eq. 'N' ) ) then
        write ( 51, fmt= '( A, I4 )' ) '         Data category:        ', mtyp
        write ( 51, fmt= '( A, I4 )' ) '     Local subcategory:        ', msbt
        write ( 51, fmt= '( A, I4 )' ) ' Internatl subcategory:        ', msbti
      else
        print *, 'in debufr.F90, calling getcfmng for TABLAT'
        call getcfmng ( lunit, 'TABLAT', mtyp, ' ', -1, cmmtyp, lcmmtyp, iermtyp )
        if ( iermtyp .eq. 0 ) then
          write ( 51, fmt= '( A, I4, 3A )' ) '         Data category:        ', mtyp, ' (= ', cmmtyp(1:lcmmtyp), ')'
        else
          write ( 51, fmt= '( A, I4 )' ) '         Data category:        ', mtyp
        end if
        print *, 'in debufr.F90, calling getcfmng for TABLASL'
        call getcfmng ( lunit, 'TABLASL', msbt, 'TABLAT', mtyp, cmmsbt, lcmmsbt, iermsbt )
        if ( ( iermsbt .eq. 0 ) .and. ( iogce .eq. 7 ) ) then
          write ( 51, fmt= '( A, I4, 3A )' ) '     Local subcategory:        ', msbt, ' (= ', cmmsbt(1:lcmmsbt), ')'
        else
          write ( 51, fmt= '( A, I4 )' ) '     Local subcategory:        ', msbt
        end if
        print *, 'in debufr.F90, calling getcfmng for TABLASS'
        call getcfmng ( lunit, 'TABLASS', msbti, 'TABLAT', mtyp, cmmsbti, lcmmsbti, iermsbti )
        if ( iermsbti .eq. 0 ) then
          write ( 51, fmt= '( A, I4, 3A )' ) ' Internatl subcategory:        ', msbti, ' (= ', cmmsbti(1:lcmmsbti), ')'
        else
          write ( 51, fmt= '( A, I4 )' ) ' Internatl subcategory:        ', msbti
        end if
      end if

      write ( 51, fmt= '( A, I4 )' ) '  Master table version:        ', iupbs01 ( ibfmg, 'MTV' )
      write ( 51, fmt= '( A, I4 )' ) '   Local table version:        ', iupbs01 ( ibfmg, 'MTVL' )
      write ( 51, fmt= '( A, I4 )' ) '                  Year:        ', iupbs01 ( ibfmg, 'YEAR' )
      write ( 51, fmt= '( A, I4 )' ) '                 Month:        ', iupbs01 ( ibfmg, 'MNTH' )
      write ( 51, fmt= '( A, I4 )' ) '                   Day:        ', iupbs01 ( ibfmg, 'DAYS' )
      write ( 51, fmt= '( A, I4 )' ) '                  Hour:        ', iupbs01 ( ibfmg, 'HOUR' )
      write ( 51, fmt= '( A, I4 )' ) '                Minute:        ', iupbs01 ( ibfmg, 'MINU' )
      write ( 51, fmt= '( A, I4 )' ) '                Second:        ', iupbs01 ( ibfmg, 'SECO' )
      if ( ( iogce .eq. 7 ) .and. ( igses .eq. 3 ) ) then
        call rtrcptb ( ibfmg, iryr, irmo, irdy, irhr, irmi, irtret )
        if ( irtret .eq. 0 ) then
          write ( 51, fmt= '( A, I4 )' ) '   NCEP tank rcpt year:        ', iryr
          write ( 51, fmt= '( A, I4 )' ) '  NCEP tank rcpt month:        ', irmo
          write ( 51, fmt= '( A, I4 )' ) '    NCEP tank rcpt day:        ', irdy
          write ( 51, fmt= '( A, I4 )' ) '   NCEP tank rcpt hour:        ', irhr
          write ( 51, fmt= '( A, I4 )' ) ' NCEP tank rcpt minute:        ', irmi
        end if
      end if

      ! Decode and output the data from Section 3.

      nsub = iupbs3 ( ibfmg, 'NSUB' )
      write ( 51, fmt= '( /, A, I4 )' ) ' Number of data subsets:        ', nsub
      nsubt = nsubt + nsub

      if ( iupbs3 ( ibfmg, 'IOBS' ) .eq. 1 ) then
        write ( 51, fmt = '( A )') '     Data are observed?: Yes'
      else
        write ( 51, fmt = '( A )') '     Data are observed?: No'
      end if

      if ( iupbs3 ( ibfmg, 'ICMP' ) .eq. 1 ) then
        write ( 51, fmt = '( A )') '   Data are compressed?: Yes'
      else
        write ( 51, fmt = '( A )') '   Data are compressed?: No'
      end if

      call upds3 ( ibfmg, mxds3, cds3, nds3 )
      write ( 51, fmt= '( A, I5 )' ) '  Number of descriptors:       ', nds3
      do jj = 1, nds3
        write ( 51, fmt = '( 5X, I4, A, A6)' ) jj, ": ", cds3 ( jj )
      end do

      if ( ( basic_f .eq. 'N' ) .and. ( ierme .ge. 0 ) ) then

        ! Decode and output the data from Section 4.

        write ( 51, fmt = '( /, A, I7, 3A, I10, A, I6, A )' ) &
          'BUFR message #', nmsg, ' of type ', cmgtag, ' and date ', imgdt, ' contains ', nsub, ' subsets:'
        do while ( ireadsb ( lunit ) .eq. 0 )
          call ufdump ( lunit, 51 )
        end do
      end if

      write  ( 51, fmt = '( /, A, I7 )' ) 'End of BUFR message #', nmsg
      write  ( 51, fmt = '( /, 120("-"))' )
    end if

  end do

  return
end subroutine fdebufr_c

!> This subroutine overrides the placeholder subroutine of the same name within the NCEPLIBS-bufr distribution package.
!>
!> Given the data category for a BUFR message, this subroutine opens the appropriate DX BUFR tables file to use in
!> reading/decoding the message.
!>
!> @author J. Ator
!> @date 2012-12-07
!>
!> @param mtyp - Data category of BUFR message
!> @param lundx - Fortran logical unit number for DX BUFR tables file to use in reading/decoding the message
!>   - 0 = No such file is available
!>
!> @author J. Ator @date 2012-12-07
subroutine openbt ( lundx, mtyp )

  use Share_Table_Info

  implicit none

  character*11 bftab
  character*275 bftabfil

  integer, intent(in) :: mtyp
  integer, intent(out) :: lundx

  logical exists

  write ( bftab, '("bufrtab.",i3.3)' ) mtyp
  bftabfil = tbldir_f(1:ltbd) // '/' // bftab

  inquire ( file = bftabfil, exist = exists )
  if ( exists ) then
    lundx = ludx
    close ( lundx )
    open ( unit = lundx, file = bftabfil )
  else
    lundx = 0
  end if

  return
end subroutine openbt
