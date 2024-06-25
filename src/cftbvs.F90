!> @file
!> @brief Convert between code or flag table values and actual data values.
!>
!> @author J. Ator @date 2005-11-29

!> Compute the numerical value equivalent to the setting of bit #(ibit) within a flag table of nbits bits.
!>
!> If the computation fails for any reason, then the function returns the current placeholder value for "missing" data.
!>
!> @param nbits - Total number of bits in flag table
!> @param ibit - Number of bit to be set
!>
!> @returns pkftbv - real*8: Value equivalent to the setting of bit #(ibit) within a flag table of nbits bits
!>
!> @remarks
!> - This function is the logical inverse of subroutine upftbv().
!> - According to the WMO standard, bits within a bit field are numbered from left to right, so bit #1 is always the
!> high-order (i.e. most significant) bit in any bit field.
!>
!> @author J. Ator @date 2005-11-29
recursive real*8 function pkftbv(nbits,ibit) result(r8val)

  use modv_vars, only: im8b, bmiss

  implicit none

  integer, intent(in) :: nbits, ibit
  integer my_nbits, my_ibit

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(nbits,my_nbits,1)
    call x84(ibit,my_ibit,1)
    r8val=pkftbv(my_nbits,my_ibit)

    im8b=.true.
    return
  endif

  if((nbits<=0).or.(ibit<=0).or.(ibit>nbits)) then
    r8val = bmiss
  else
    r8val = (2.)**(nbits-ibit)
  endif

  return
end function pkftbv

!> Given a Table B mnemonic with flag table units and a corresponding numerical data value, this subroutine determines
!> the bit settings equivalent to that numerical value.
!>
!> This subroutine is the logical inverse of function pkftbv().
!>
!> According to the WMO standard, bits within a bit field are numbered from left to right, so bit #1 is always the
!> high-order i.e. most significant) bit in any bit field.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param nemo - Table B mnemonic with flag table units
!> @param val - Value corresponding to nemo
!> @param mxib - Dimensioned size (in integers) of ibit in the calling program; used by the subroutine
!> to ensure that it doesn't overflow the ibit array
!> @param ibit - Bit numbers which were set to "On" (i.e. set to "1") in val
!> @param nib - Number of bit numbers returned in ibit
!>
!> @author J. Ator @date 2005-11-29
recursive subroutine upftbv(lunit,nemo,val,mxib,ibit,nib)

  use modv_vars, only: im8b

  use moda_tababd

  implicit none

  integer, intent(in) :: lunit, mxib
  integer, intent(out) :: ibit(*), nib
  integer my_lunit, my_mxib, lun, il, im, idn, i, n, nbits, iersn

  character*(*), intent(in) :: nemo
  character*128 bort_str
  character tab

  real*8, intent(in) :: val
  real*8 r8val, r82i

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(mxib,my_mxib,1)
    call upftbv( my_lunit, nemo, val, my_mxib*2, ibit, nib )
    call x48(ibit(1),ibit(1),nib)
    call x48(nib,nib,1)

    im8b=.true.
    return
  endif

  ! Perform some sanity checks.

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: UPFTBV - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')

  call nemtab(lun,nemo,idn,tab,n)
  if(n==0) then
    write(bort_str,'("BUFRLIB: UPFTBV - MNEMONIC ",A," NOT FOUND IN TABLE B")') nemo
    call bort(bort_str)
  endif
  if(tabb(n,lun)(71:74)/='FLAG') then
    write(bort_str,'("BUFRLIB: UPFTBV - MNEMONIC ",A," IS NOT A FLAG TABLE")') nemo
    call bort(bort_str)
  endif

  ! Figure out which bits are set.

  nib = 0
  r8val = val
  call strnum(tabb(n,lun)(110:112),nbits,iersn)
  do i=(nbits-1),0,-1
    r82i = (2.)**i
    if(abs(r8val-r82i)<(0.005)) then
      nib = nib + 1
      if(nib>mxib) call bort('BUFRLIB: UPFTBV - IBIT ARRAY OVERFLOW')
      ibit(nib) = nbits-i
      return
    elseif(r82i<r8val) then
      nib = nib + 1
      if(nib>mxib) call bort('BUFRLIB: UPFTBV - IBIT ARRAY OVERFLOW')
      ibit(nib) = nbits-i
      r8val = r8val - r82i
    endif
  enddo

  return
end subroutine upftbv

!> Decode the meaning of a numerical value from a code or flag table.
!>
!> Search for a specified Table B mnemonic and associated
!> value (code figure or bit number) within the master Code/Flag tables,
!> and if found return the associated meaning as a character string.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param nemoi - Mnemonic to search for
!> @param ivali - Value (code figure or bit number) associated with nemoi
!> @param nemod - Optional second mnemonic upon which the values nemoi and ivali depend; set to all blank characters if the
!> meanings of nemoi and ivali do not depend on the value of any other mnemonic
!> @param ivald - Value (code figure or bit number) associated with nemod; set to (-1) whenever nemod is set to all blank characters
!> @param cmeang - If the initial search of the master Code/Flag tables was successful, then this string contains the meaning
!> corresponding to nemoi and ivali (and to nemod and ivald, if specified). However, if the initial search was unsuccessful,
!> <b>and</b> if no optional second mnemonic and associated value were specified on input, <b>and</b> if a second search of the
!> table determines that the meaning of nemoi and ivali indeed depends on one or more other possible second mnemonics, then
!> those possible second mnemonics are returned within this string, as a series of iret successive 8-byte substrings.
!> An example of this scenario is included below within the Remarks.
!> @param lnmng - Length (in bytes) of string returned in cmeang
!> @param iret - Return code
!>    -  0 = meaning found and stored in cmeang string
!>    - -1 = meaning not found
!>    - >0 = meaning not found, <b>and</b> nemod and ivald were not specified on input, <b>and</b> the meaning of nemoi and ivali
!>      depends on the value of one of the mnemonics stored in the first iret 8-byte substrings of cmeang
!>
!> As noted above, this subroutine first does an initial search of
!> the master Code/Flag tables based on the mnemonics and values provided.
!> The input parameters nemoi and ivali specify the mnemonic and
!> corresponding numerical code or flag table value for which the meaning
!> is sought, and the optional secondary parameters nemod and ivald are
!> specified when needed to differentiate between multiple possible
!> results. An example of this particular scenario is included below
!> within the Remarks.  Otherwise, if the meaning of nemod and ivald
!> does not depend on the value associated with any other mnemonic, then
!> nemod should be set to a field of all blank characters, and ivald
!> should be set to a value of (-1).
!>
!> Subroutine codflg() must be called with a cf value of 'Y' prior to
!> calling this subroutine, in order to ensure that master Code/Flag
!> tables have been read into internal memory.
!>
!> This subroutine can be called at any time after a BUFR message
!> has been read into internal arrays by one of the NCEPLIBS-bufr
!> [message-reading subroutines](@ref hierarchy), and it
!> can be called for any code or flag table mnemonic defined within that
!> particular message.  In most cases, this means that the mnemonic must
!> be contained within the subset definition (Section 3) of that message.
!> The only exceptions to this rule are for originating centers,
!> originating subcenters, data types and data subtypes, since those can
!> also be contained within the identification section (Section 1) of a
!> BUFR message.
!>
!> It is the user's responsibility to provide sufficient allocated
!> space in cmeang for the returned meaning string; otherwise, the
!> returned string will be truncated.
!>
!> @remarks
!> - An example of when secondary mnemonics nemod and ivald would be
!> required is when a user is searching for the meaning of a numerical
!> code table value for an originating sub-center (i.e. mnemonic GSES).
!> The meaning of any originating sub-center value depends on the identity
!> of the originating center for which the sub-center in question is a
!> member, so in order for the subroutine to locate and return the proper
!> one, information about the originating center must also be provided. So
!> in this case the user would input GSES and the associated numerical
!> value as nemoi and ivali, respectively, but the user would also need to
!> specify an appropriate originating center mnemonic (e.g. GCLONG, OGCE
!> or ORIGC) and associated value from the same BUFR message as input
!> parameters nemod and ivald, respectively, and then the subroutine will
!> be able to locate and return the appropriate meaning string. Otherwise,
!> if this information was not provided, the subroutine would return with
!> an iret value of 3, and with each of the mnemonics GCLONG, OGCE and
!> ORIGC contained in successive 8-byte substrings of cmeang (and with a
!> corresponding value of 24 returned for lnmng), as a hint to the user
!> that more information needs to be input to the subroutine in order to
!> achieve the desired result.
!>
!> @author J. Ator @date 2018-01-11
recursive subroutine getcfmng ( lunit, nemoi, ivali, nemod, ivald, cmeang, lnmng, iret )

  use bufrlib

  use modv_vars, only: im8b

  use moda_tababd
  use moda_tablef

  implicit none

  integer, intent(in) :: lunit, ivali, ivald
  integer, intent(out) :: lnmng, iret
  integer ifxyd(10), my_lunit, my_ivali, my_ivald, lun, il, im, itmp, ii, ifxyi, lcmg, n, ntg, iret2, ierbd, ifxy, ireadmt

  character*(*), intent(in) :: nemoi, nemod
  character*(*), intent(out) :: cmeang
  character*128 bort_str
  character*8 nemo, my_nemoi, my_nemod
  character tab

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(ivali,my_ivali,1)
    call x84(ivald,my_ivald,1)
    call getcfmng(my_lunit,nemoi,my_ivali,nemod,my_ivald,cmeang,lnmng,iret)
    call x48(lnmng,lnmng,1)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  call status ( lunit, lun, il, im )
  if ( il == 0 ) call bort('BUFRLIB: GETCFMNG - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if ( il > 0 ) call bort('BUFRLIB: GETCFMNG - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if ( im == 0 ) call bort('BUFRLIB: GETCFMNG - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')

  ! Make sure the appropriate code/flag information has already been read into internal memory.

  if ( cdmf /= 'Y' ) call bort('BUFRLIB: GETCFMNG - TO USE THIS SUBROUTINE, MUST '// &
    'FIRST CALL SUBROUTINE CODFLG WITH INPUT ARGUMENT SET TO Y')

  itmp = ireadmt ( lun )

  ! Check the validity of the input mnemonic(s).  Include special handling for originating centers, originating subcenters, data
  ! types and data subtypes, since those can be reported in Section 1 of a BUFR message as well as in Section 3, so if a user
  ! requests those mnemonics we can't necessarily assume they came from within Section 3.

  lcmg = len ( cmeang )

  my_nemoi = '        '
  do ii = 1, min ( 8, len( nemoi ) )
    my_nemoi(ii:ii) = nemoi(ii:ii)
  end do
  my_nemod = '        '
  do ii = 1, min ( 8, len( nemod ) )
    my_nemod(ii:ii) = nemod(ii:ii)
  end do
  if ( my_nemoi(1:4) == 'GSES' ) then
    if ( ( my_nemod(1:6) == 'GCLONG' ) .or. ( my_nemod(1:4) == 'OGCE' ) .or. ( my_nemod(1:5) == 'ORIGC' ) ) then
      ifxyi = ifxy ( '001034' )
      ifxyd(1) = ifxy ( '001035' )
    else
      lnmng = min ( 24, lcmg )
      if ( lnmng == 24 ) then
        iret = 3
        cmeang(1:24) = 'GCLONG  OGCE    ORIGC   '
      else
        iret = -1
      end if
      return
    end if
  else if ( my_nemoi(1:6) == 'GCLONG' ) then
    ifxyi = ifxy ( '001031' )
    ifxyd(1) = (-1)
  else if ( my_nemoi(1:4) == 'OGCE' ) then
    ifxyi = ifxy ( '001033' )
    ifxyd(1) = (-1)
  else if ( my_nemoi(1:5) == 'ORIGC' ) then
    ifxyi = ifxy ( '001035' )
    ifxyd(1) = (-1)
  else if ( ( my_nemoi(1:7) == 'TABLASS' ) .or. ( my_nemoi(1:7) == 'TABLASL' ) ) then
    if ( ( my_nemod(1:6) == 'TABLAT' ) ) then
      if ( my_nemoi(1:7) == 'TABLASS' ) then
        ifxyi = ifxy ( '055021' )
      else
        ifxyi = ifxy ( '055022' )
      endif
      ifxyd(1) = ifxy ( '055020' )
    else
      lnmng = min ( 8, lcmg )
      if ( lnmng == 8 ) then
        iret = 1
        cmeang(1:8) = 'TABLAT  '
      else
        iret = -1
      end if
      return
    end if
  else if ( my_nemoi(1:6) == 'TABLAT' ) then
    ifxyi = ifxy ( '055020' )
    ifxyd(1) = (-1)
  else
    call parstr ( my_nemoi, nemo, 1, ntg, ' ', .true. )
    call nemtab ( lun, nemo, ifxyi, tab, n )
    if ( ( n == 0 ) .or. ( tab /= 'B' ) ) then
      write(bort_str,'("BUFRLIB: GETCFMNG - MNEMONIC ",A," NOT FOUND IN TABLE B")') nemo
      call bort(bort_str)
    endif
    if ( ( tabb ( n, lun )(71:74) /= 'CODE' ) .and. ( tabb ( n, lun )(71:74) /= 'FLAG' ) ) then
      write(bort_str,'("BUFRLIB: GETCFMNG - MNEMONIC ",A," IS NOT A CODE OR FLAG TABLE")') nemo
      call bort(bort_str)
    endif
    if ( my_nemod(1:1) /= ' ' ) then
      call parstr ( my_nemod, nemo, 1, ntg, ' ', .true. )
      call nemtab ( lun, nemo, ifxyd(1), tab, n )
      if ( ( n == 0 ) .or. ( tab /= 'B' ) ) then
        write(bort_str,'("BUFRLIB: GETCFMNG - MNEMONIC ",A," NOT FOUND IN TABLE B")') nemo
        call bort(bort_str)
      endif
      if ( ( tabb ( n, lun )(71:74) /= 'CODE' ) .and. ( tabb ( n, lun )(71:74) /= 'FLAG' ) ) then
        write(bort_str,'("BUFRLIB: GETCFMNG - MNEMONIC ",A," IS NOT A CODE OR FLAG TABLE")') nemo
        call bort(bort_str)
      endif
    else
      ifxyd(1) = (-1)
    end if
  end if

  ! Search the internal table for the requested meaning.

  call srchtbf_c ( ifxyi, ivali, ifxyd(1), 10, ivald, cmeang, lcmg, lnmng, iret )
  if ( iret <= 0 ) return

  ! The meaning of this value is dependent on the value of another mnemonic in the report.

  iret2 = iret
  lnmng = 0
  iret = 0
  do ii = 1, iret2
    call numtbd ( lun, ifxyd(ii), nemo, tab, ierbd )
    if ( ( ierbd > 0 ) .and. ( tab == 'B' ) .and. ( lcmg >= ( lnmng + 8 ) ) ) then
      iret = iret + 1
      cmeang(lnmng+1:lnmng+8) = nemo
      lnmng = lnmng + 8
    end if
  end do
  if ( iret == 0 ) iret = -1

  return
end subroutine getcfmng

!> Given a mnemonic associated with a category 63 Table D
!> descriptor from an NCEP prepbufr file, return the
!> corresponding event program code.
!>
!> The event program code is equivalent to the Y value of the
!> category 63 (i.e. X=63) Table D descriptor.  Knowledge of this value
!> is especially useful for application programs which are writing data
!> events to NCEP prepbufr files.
!>
!> Logical unit lunit should have already been opened via a previous
!> call to subroutine openbf().
!>
!> This subroutine is the logical inverse of subroutine ufbqcp().
!>
!> @param lunit - Fortran logical unit number for NCEP prepbufr file
!> @param nemo - Mnemonic associated with a category 63 (i.e. X=63) Table D descriptor
!> @param iqcd - Y value of descriptor associated with nemo
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbqcd(lunit,nemo,iqcd)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: lunit
  integer, intent(out) :: iqcd
  integer my_lunit, lun, il, im, idn, iret

  character*(*), intent(in) :: nemo
  character*128 bort_str
  character*6 fxy, adn30
  character tab

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call ufbqcd(my_lunit,nemo,iqcd)
    call x48(iqcd,iqcd,1)
    im8b=.true.
    return
  endif

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: UFBQCD - BUFR FILE IS CLOSED, IT MUST BE OPEN')

  call nemtab(lun,nemo,idn,tab,iret)
  if(tab/='D') then
    write(bort_str,'("BUFRLIB: UFBQCD - INPUT MNEMONIC ",A," NOT DEFINED AS A SEQUENCE DESCRIPTOR IN BUFR TABLE")') nemo
    call bort(bort_str)
  endif

  fxy = adn30(idn,6)
  if(fxy(2:3)/='63') then
    write(bort_str,'("BUFRLIB: UFBQCD - BUFR TABLE SEQ. DESCRIPTOR '// &
      'ASSOC. WITH INPUT MNEMONIC ",A," HAS INVALID CATEGORY ",A," - CATEGORY MUST BE 63")') nemo, fxy(2:3)
    call bort(bort_str)
  endif
  read(fxy(4:6),'(I3)') iqcd

  return
end subroutine ufbqcd

!> Given an event program code, which is equivalent to the Y value
!> of a category 63 Table D descriptor from an NCEP
!> prepbufr file, return the corresponding mnemonic.
!>
!> Logical unit lunit should have already been opened via a previous
!> call to subroutine openbf().
!>
!> This subroutine is the logical inverse of subroutine ufbqcd().
!>
!> @param lunit - Fortran logical unit number for NCEP prepbufr file
!> @param iqcp - Y value of a category 63 (i.e. X=63) Table D descriptor
!> @param nemo - Mnemonic associated with iqcp
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbqcp(lunit,iqcp,nemo)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: lunit, iqcp
  integer my_lunit, my_iqcp, lun, il, im, idn, iret, ifxy

  character*(*), intent(out) :: nemo
  character tab

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(iqcp,my_iqcp,1)
    call ufbqcp(my_lunit,my_iqcp,nemo)
    im8b=.true.
    return
  endif

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: UFBQCP - BUFR FILE IS CLOSED, IT MUST BE OPEN')

  idn = ifxy('363000')+iqcp
  call numtab(lun,idn,nemo,tab,iret)

  return
end subroutine ufbqcp
