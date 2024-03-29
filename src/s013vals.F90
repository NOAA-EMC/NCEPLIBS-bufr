!> @file
!> @brief Read and write data values within Sections 0, 1, and 3 of BUFR messages
!>
!> @author J. Ator @date 2005-11-29

!> Get the location of a specified value within Section 1 of a BUFR message.
!>
!> The location and availability of any particular value within Section 1 of a BUFR message can vary depending on the edition
!> number used to encode the message.  This subroutine will work for BUFR edition 2, 3, or 4.
!>
!> @param[in] s1mnem - character*(*): Value whose location within Section 1 is to be determined:
!>   - 'LEN1'  = Length (in bytes) of Section 1
!>   - 'BMT'   = BUFR master table
!>   - 'OGCE'  = Originating center
!>   - 'GSES'  = Originating subcenter
!>   - 'USN'   = Update sequence number
!>   - 'ISC2'  = Flag indicating absence/presence of (optional) Section 2 in BUFR message:
!>      - 0 = Section 2 absent
!>      - 1 = Section 2 present
!>   - 'MTYP'  = Data category
!>   - 'MSBTI' = Data subcategory (international)
!>   - 'MSBT'  = Data subcategory (local)
!>   - 'MTV'   = Version number of master table
!>   - 'MTVL'  = Version number of local tables
!>   - 'YCEN'  = Year of century (1-100)
!>   - 'CENT'  = Century (e.g., 20 for years 1901-2000, 21 for years 2001-2100)
!>   - 'YEAR'  = Year (4-digit)
!>   - 'MNTH'  = Month
!>   - 'DAYS'  = Day
!>   - 'HOUR'  = Hour
!>   - 'MINU'  = Minute
!>   - 'SECO'  = Second
!> @param[in] iben - integer: BUFR edition number
!> @param[out] isbyt - integer: Number of starting byte within Section 1 which contains value corresponding to s1mnem
!> @param[out] iwid - integer: Width (in bits) of value corresponding to s1mnem, counting from the first bit of the
!> byte pointed to by isbyt
!> @param[out] iret - integer: Return code:
!>   - 0 = normal return
!>   - -1 = s1mnem is invalid for BUFR edition iben
!>
!> @remarks
!> - s1mnem = 'GSES' is only valid for iben = 3 or 4
!> - s1mnem = 'YCEN' or 'CENT' is only valid for iben = 2 or 3
!> - s1mnem = 'YEAR', 'SECO', or 'MSBTI' is only valid for iben = 4
!>
!> @author J. Ator @date 2005-11-29
recursive subroutine gets1loc(s1mnem,iben,isbyt,iwid,iret)

  use modv_vars, only: im8b

  implicit none

  character*(*), intent(in) :: s1mnem

  integer, intent(in) :: iben
  integer, intent(out) :: isbyt, iwid, iret
  integer my_iben

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(iben,my_iben,1)
    call gets1loc(s1mnem,my_iben,isbyt,iwid,iret)
    call x48(isbyt,isbyt,1)
    call x48(iwid,iwid,1)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  iret = 0
  iwid = 8

  if(s1mnem.eq.'LEN1') then
    isbyt = 1
    iwid = 24
  else if(s1mnem.eq.'BMT') then
    isbyt = 4
  else if(s1mnem.eq.'OGCE') then
    if(iben.eq.3) then
      isbyt = 6
    else
      ! Note that this location is actually the same for both edition 2 and edition 4 of BUFR
      isbyt = 5
      iwid = 16
    endif
  else if(s1mnem.eq.'GSES') then
    if(iben.eq.3) then
      isbyt = 5
    else if(iben.eq.4) then
      isbyt = 7
      iwid = 16
    else
      iret = -1
    endif
  else if(s1mnem.eq.'USN') then
    if(iben.eq.4) then
      isbyt = 9
    else
      isbyt = 7
    endif
  else if(s1mnem.eq.'ISC2') then
    iwid = 1
    if(iben.eq.4) then
      isbyt = 10
    else
      isbyt = 8
    endif
  else if(s1mnem.eq.'MTYP') then
    if(iben.eq.4) then
      isbyt = 11
    else
      isbyt = 9
    endif
  else if(s1mnem.eq.'MSBTI') then
    if(iben.eq.4) then
      isbyt = 12
    else
      iret = -1
    endif
  else if(s1mnem.eq.'MSBT') then
    if(iben.eq.4) then
      isbyt = 13
    else
      isbyt = 10
    endif
  else if(s1mnem.eq.'MTV') then
    if(iben.eq.4) then
      isbyt = 14
    else
      isbyt = 11
    endif
  else if(s1mnem.eq.'MTVL') then
    if(iben.eq.4) then
      isbyt = 15
    else
      isbyt = 12
    endif
  else if(s1mnem.eq.'YEAR') then
    if(iben.eq.4) then
      isbyt = 16
      iwid = 16
    else
      iret = -1
    endif
  else if(s1mnem.eq.'YCEN') then
    if(iben.lt.4) then
      isbyt = 13
    else
      iret = -1
    endif
  else if(s1mnem.eq.'CENT') then
    if(iben.lt.4) then
      isbyt = 18
    else
      iret = -1
    endif
  else if(s1mnem.eq.'MNTH') then
    if(iben.eq.4) then
      isbyt = 18
    else
      isbyt = 14
    endif
  else if(s1mnem.eq.'DAYS') then
    if(iben.eq.4) then
      isbyt = 19
    else
      isbyt = 15
    endif
  else if(s1mnem.eq.'HOUR') then
    if(iben.eq.4) then
      isbyt = 20
    else
      isbyt = 16
    endif
  else if(s1mnem.eq.'MINU') then
    if(iben.eq.4) then
      isbyt = 21
    else
      isbyt = 17
    endif
  else if(s1mnem.eq.'SECO') then
    if(iben.eq.4) then
      isbyt = 22
    else
      iret = -1
    endif
  else
    iret = -1
  endif

  return
end subroutine gets1loc

!> Read a specified value from within Section 0 or Section 1 of a BUFR message.
!>
!> This function will work on any BUFR message encoded using BUFR
!> edition 2, 3, or 4. It is similar to function iupvs01(), except
!> that it operates on a BUFR message passed in via a memory array,
!> whereas iupvs01() operates on the BUFR message that was read into
!> internal arrays via the most recent call to any of the other
!> [message-reading subroutines](@ref hierarchy) for a specified
!> Fortran logical unit.
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of mbay
!> - Values corresponding to s01mnem = 'GSES' can only be read from BUFR messages encoded using BUFR edition 3 or 4
!> - Values corresponding to s01mnem = 'YCEN' or 'CENT' can only be read from BUFR messages encoded using BUFR edition 2 or 3
!> - When reading from BUFR messages encoded using BUFR edition 2 or 3, values corresponding to s01mnem = 'YEAR' will be
!>   calculated internally using the values for 'YCEN' and 'CENT', or inferred using a windowing technique
!> - Values corresponding to s01mnem = 'SECO' or 'MSBTI' can only be read from BUFR messages encoded using BUFR edition 4
!>
!> @param[in] mbay  - integer(*): BUFR message
!> @param[in] s01mnem - character*(*): Value to be read from Section 0 or Section 1 of mbay
!> - 'LENM'  = Length (in bytes) of BUFR message
!> - 'LEN0'  = Length (in bytes) of Section 0
!> - 'LEN1'  = Length (in bytes) of Section 1
!> - 'BEN'   = BUFR edition number
!> - 'BMT'   = BUFR master table
!> - 'OGCE'  = Originating center
!> - 'GSES'  = Originating subcenter
!> - 'USN'   = Update sequence number
!> - 'ISC2'  = Flag indicating absence/presence of (optional) Section 2 in BUFR message:
!>   - 0 = Section 2 absent
!>   - 1 = Section 2 present
!> - 'MTYP'  = Data category
!> - 'MSBTI' = Data subcategory (international)
!> - 'MSBT'  = Data subcategory (local)
!> - 'MTV'   = Version number of master table
!> - 'MTVL'  = Version number of local tables
!> - 'YCEN'  = Year of century (1-100)
!> - 'CENT'  = Century (e.g., 20 for years 1901-2000, 21 for years 2001-2100)
!> - 'YEAR'  = Year (4-digit)
!> - 'MNTH'  = Month
!> - 'DAYS'  = Day
!> - 'HOUR'  = Hour
!> - 'MINU'  = Minute
!> - 'SECO'  = Second
!> @returns iupbs01 - integer: Value corresponding to s01mnem:
!> - -1 = s01mnem was invalid for the edition of BUFR message in mbay, or some other error occurred
!>
!> @author J. Ator @date 2005-11-29
recursive integer function iupbs01(mbay,s01mnem) result(iret)

  use modv_vars, only: im8b

  implicit none

  character*(*), intent(in) :: s01mnem

  integer, intent(in) :: mbay(*)
  integer ival, iupb, i4dy, len0, iben, isbyt, iwid, iretgs, iyoc, icen

  logical ok4cent

  ! This statement function checks whether its input value contains a valid century value.
  ok4cent(ival) = ((ival.ge.19).and.(ival.le.21))

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    iret = iupbs01(mbay,s01mnem)

    im8b=.true.
    return
  endif

  ! Handle some simple requests that do not depend on the BUFR edition number.

  if(s01mnem.eq.'LENM') then
    iret = iupb(mbay,5,24)
    return
  endif

  len0 = 8
  if(s01mnem.eq.'LEN0') then
    iret = len0
    return
  endif

  ! Get the BUFR edition number.

  iben = iupb(mbay,8,8)
  if(s01mnem.eq.'BEN') then
    iret = iben
    return
  endif

  ! Use the BUFR edition number to handle any other requests.

  call gets1loc(s01mnem,iben,isbyt,iwid,iretgs)
  if(iretgs.eq.0) then
    iret = iupb(mbay,len0+isbyt,iwid)
    if(s01mnem.eq.'CENT') then

      ! Test whether the returned value was a valid century value.

      if(.not.ok4cent(iret)) iret = -1
    endif
  else if( (s01mnem.eq.'YEAR') .and. (iben.lt.4) ) then

    ! Calculate the 4-digit year.

    iyoc = iupb(mbay,21,8)
    icen = iupb(mbay,26,8)

    ! Does icen contain a valid century value?

    if(ok4cent(icen)) then
      ! YES, so use it to calculate the 4-digit year. Note that, by international convention, the year 2000 was the 100th
      ! year of the 20th century, and the year 2001 was the 1st year of the 21st century
      iret = (icen-1)*100 + iyoc
    else
      ! NO, so use a windowing technique to determine the 4-digit year from the year of the century.
      iret = i4dy(mod(iyoc,100)*1000000)/10**6
    endif
  else
    iret = -1
  endif

  return
end function iupbs01

!> Read a specified value from within Section 3 of a BUFR message.
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of mbay
!>
!> @param[in] mbay - integer(*): BUFR message
!> @param[in] s3mnem - character*(*): Value to be read from Section 3 of mbay
!> - 'NSUB'  = Number of data subsets
!> - 'IOBS'  = Flag indicating whether the message contains observed data:
!>   - 0 = No
!>   - 1 = Yes
!> - 'ICMP'  = Flag indicating whether the message contains compressed data:
!>   - 0 = No
!>   - 1 = Yes
!>
!> @returns iupbs3 - integer: Value corresponding to s3mnem
!> - -1 = s3mnem was invalid
!>
!> @author J. Ator @date 2009-03-23
recursive integer function iupbs3(mbay,s3mnem) result(iret)

  use modv_vars, only: im8b

  implicit none

  character*(*), intent(in) :: s3mnem

  integer, intent(in) :: mbay(*)
  integer len0, len1, len2, len3, l4, l5, ipt, ival, imask, iupb

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    iret = iupbs3(mbay,s3mnem)

    im8b=.true.
    return
  endif

  ! Skip to the beginning of Section 3.

  call getlens(mbay,3,len0,len1,len2,len3,l4,l5)
  ipt = len0 + len1 + len2

  ! Unpack the requested value.

  if(s3mnem.eq.'NSUB') then
    iret = iupb(mbay,ipt+5,16)
  else if( (s3mnem.eq.'IOBS') .or. (s3mnem.eq.'ICMP') ) then
    ival = iupb(mbay,ipt+7,8)
    if(s3mnem.eq.'IOBS') then
      imask = 128
    else
      imask = 64
    endif
    iret = min(1,iand(ival,imask))
  else
    iret = -1
  endif

  return
end function iupbs3

!> Read a specified value from within Section 0 or 1 of a BUFR message.
!>
!> This function will work on any BUFR message encoded using BUFR
!> edition 2, 3, or 4. It is similar to function iupbs01(), except
!> that iupbs01() operates on a BUFR message passed in via a memory
!> array, whereas this function operates on the BUFR message that was
!> read into internal arrays via the most recent call to any of the
!> other [message-reading subroutines](@ref hierarchy) for a specified
!> Fortran logical unit.
!>
!> @remarks
!> - Values corresponding to s01mnem = 'GSES' can only be read from BUFR messages encoded using BUFR edition 3 or 4
!> - Values corresponding to s01mnem = 'YCEN' or 'CENT' can only be read from BUFR messages encoded using BUFR edition 2 or 3
!> - When reading from BUFR messages encoded using BUFR edition 2 or 3, values corresponding to s01mnem = 'YEAR' will be
!>   calculated internally using the values for 'YCEN' and 'CENT', or inferred using a windowing technique
!> - Values corresponding to s01mnem = 'SECO' or 'MSBTI' can only be read from BUFR messages encoded using BUFR edition 4
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[in] s01mnem - character*(*): Value to be read from Section 0 or Section 1 of BUFR message in internal arrays for lunit:
!> - 'LENM'  = Length (in bytes) of BUFR message
!> - 'LEN0'  = Length (in bytes) of Section 0
!> - 'LEN1'  = Length (in bytes) of Section 1
!> - 'BEN'   = BUFR edition number
!> - 'BMT'   = BUFR master table
!> - 'OGCE'  = Originating center
!> - 'GSES'  = Originating subcenter
!> - 'USN'   = Update sequence number
!> - 'ISC2'  = Flag indicating absence/presence of (optional) Section 2 in BUFR message:
!>   - 0 = Section 2 absent
!>   - 1 = Section 2 present
!> - 'MTYP'  = Data category
!> - 'MSBTI' = Data subcategory (international)
!> - 'MSBT'  = Data subcategory (local)
!> - 'MTV'   = Version number of master table
!> - 'MTVL'  = Version number of local tables
!> - 'YCEN'  = Year of century (1-100)
!> - 'CENT'  = Century (e.g., 20 for years 1901-2000, 21 for years 2001-2100)
!> - 'YEAR'  = Year (4-digit)
!> - 'MNTH'  = Month
!> - 'DAYS'  = Day
!> - 'HOUR'  = Hour
!> - 'MINU'  = Minute
!> - 'SECO'  = Second
!>
!> @returns - integer: Value corresponding to s01mnem:
!> - -1 = s01mnem was invalid for the edition of BUFR message in internal arrays for lunit, or some other error occurred
!>
!> @author J. Ator @date 2005-11-29
recursive integer function iupvs01(lunit,s01mnem) result(iret)

  use modv_vars, only: im8b

  use moda_bitbuf

  implicit none

  character*(*), intent(in) :: s01mnem

  integer, intent(in) :: lunit
  integer my_lunit, lun, ilst, imst, iupbs01

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    iret=iupvs01(my_lunit,s01mnem)

    im8b=.true.
    return
  endif

  iret = -1

  ! Check the file status

  call status(lunit,lun,ilst,imst)
  if(ilst.eq.0) call bort('BUFRLIB: IUPVS01 - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(ilst.gt.0) call bort('BUFRLIB: IUPVS01 - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(imst.eq.0) call bort('BUFRLIB: IUPVS01 - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')

  ! Unpack the requested value

  iret = iupbs01(mbay(1,lun),s01mnem)

  return
end function iupvs01

!> Write a specified value into a specified location within Section 1 of a BUFR message, overwriting the value previously
!> stored in that location.
!>
!> This subroutine will work on any BUFR message encoded using BUFR
!> edition 2, 3, or 4.  It is similar to subroutine pkvs01(), except
!> that it operates on a BUFR message passed in via a memory array,
!> whereas pkvs01() operates on BUFR messages stored internally within
!> the software.
!>
!> @param[in] ival - integer: Value to be stored
!> @param[in,out] mbay - integer(*): BUFR message
!> @param[in] s1mnem - character*(*): Location in Section 1 of mbay within which to store ival:
!>    - 'BMT'   = BUFR master table
!>    - 'OGCE'  = Originating center
!>    - 'GSES'  = Originating subcenter
!>    - 'USN'   = Update sequence number
!>    - 'MTYP'  = Data category
!>    - 'MSBTI' = Data subcategory (international)
!>    - 'MSBT'  = Data subcategory (local)
!>    - 'MTV'   = Version number of master table
!>    - 'MTVL'  = Version number of local tables
!>    - 'YCEN'  = Year of century (1-100)
!>    - 'CENT'  = Century (e.g., 20 for years 1901-2000, 21 for years 2001-2100)
!>    - 'YEAR'  = Year (4-digit)
!>    - 'MNTH'  = Month
!>    - 'DAYS'  = Day
!>    - 'HOUR'  = Hour
!>    - 'MINU'  = Minute
!>    - 'SECO'  = Second
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of mbay
!> - Values corresponding to s1mnem = 'GSES' can only be stored within BUFR messages encoded using BUFR edition 3 or 4
!> - Values corresponding to s1mnem = 'YCEN' or 'CENT' can only be stored within BUFR messages encoded using BUFR edition 2 or 3.
!> - Values corresponding to s1mnem = 'YEAR', 'SECO' or 'MSBTI' can only be stored within BUFR messages encoded using BUFR edition 4
!>
!> @authors J. Ator, D. Keyser @date 2005-11-29
recursive subroutine pkbs1(ival,mbay,s1mnem)

  use modv_vars, only: im8b

  implicit none

  character*(*), intent(in) :: s1mnem

  integer, intent(in) :: ival
  integer, intent(inout) :: mbay(*)
  integer my_ival, iben, isbyt, iwid, iret, iupbs01, ibit

  character*128 bort_str

  ! Check for I8 integers.

  if (im8b) then
    im8b = .false.

    call x84(ival,my_ival,1)
    call pkbs1(my_ival,mbay,s1mnem)

    im8b = .true.
    return
  end if

  iben = iupbs01(mbay,'BEN')

  ! Determine where to store the value.

  call gets1loc(s1mnem,iben,isbyt,iwid,iret)
  if ( (iret.eq.0) .and. &
      ( (s1mnem.eq.'USN') .or. (s1mnem.eq.'BMT') .or. (s1mnem.eq.'OGCE') .or. (s1mnem.eq.'GSES') .or. (s1mnem.eq.'MTYP') .or. &
       (s1mnem.eq.'MSBTI') .or. (s1mnem.eq.'MSBT') .or. (s1mnem.eq.'MTV') .or. (s1mnem.eq.'MTVL') .or. (s1mnem.eq.'YCEN') .or.&
       (s1mnem.eq.'CENT') .or. (s1mnem.eq.'YEAR') .or. (s1mnem.eq.'MNTH') .or. (s1mnem.eq.'DAYS') .or. (s1mnem.eq.'HOUR') .or.&
       (s1mnem.eq.'MINU') .or. (s1mnem.eq.'SECO') ) ) then
    ! Store the value.
    ibit = (iupbs01(mbay,'LEN0')+isbyt-1)*8
    call pkb(ival,iwid,mbay,ibit)
  else
    write(bort_str,'("BUFRLIB: PKBS1 - CANNOT OVERWRITE LOCATION CORRESPONDING TO MNEMONIC (",A,") WITHIN BUFR EDITION '// &
      '(",I1,")")') s1mnem, iben
    call bort(bort_str)
  endif

  return
end subroutine pkbs1

!> Specify a value to be written into a specified location within Section 0 or Section 1 of all BUFR messages output by
!> future calls to other [message-writing subroutines](@ref hierarchy) and [subset-writing subroutines](@ref hierarchy).
!>
!> This subroutine is similar to subroutine pkbs1(), except that
!> pkbs1() operates on a single BUFR message passed in via a memory
!> array.  Alternatively, whenever this subroutine is called, the
!> specified IVAL will be written into all BUFR messages output by all
!> future calls to other [message-writing subroutines](@ref hierarchy)
!> and [subset-writing subroutines](@ref hierarchy),
!> for all Fortran logical units that are open for
!> output within the application program, unless a subsequent call is
!> made to this subroutine with the same value of s01mnem in order to
!> reset the corresponding IVAL again.  Otherwise, if this subroutine
!> is never called for a particular value of s01mnem, then a default
!> value is used for the corresponding ival, as set within subroutine
!> msgini(), cmsgini() or dxmini().
!>
!> @param[in] ival - integer: Value to be stored
!> @param[in] s01mnem - character*(*): Location where ival is to be stored within Section 0 or Section 1 of all future
!> output BUFR messages:
!>   - 'BEN'   = BUFR edition number
!>   - 'BMT'   = BUFR master table
!>   - 'OGCE'  = Originating center
!>   - 'GSES'  = Originating subcenter
!>   - 'USN'   = Update sequence number
!>   - 'MTYP'  = Data category
!>   - 'MSBTI' = Data subcategory (international)
!>   - 'MSBT'  = Data subcategory (local)
!>   - 'MTV'   = Version number of master table
!>   - 'MTVL'  = Version number of local tables
!>   - 'YCEN'  = Year of century (1-100)
!>   - 'CENT'  = Century (e.g., 20 for years 1901-2000,
!>               21 for years 2001-2100)
!>   - 'YEAR'  = Year (4-digit)
!>   - 'MNTH'  = Month
!>   - 'DAYS'  = Day
!>   - 'HOUR'  = Hour
!>   - 'MINU'  = Minute
!>   - 'SECO'  = Second
!>
!> @remarks
!> - A separate call to this subroutine must be made for each value of s01mnem that is to be set within Section 0 or
!> Section 1 of all future output BUFR messages.
!> - A call to this subroutine with s01mnem = 'BEN' and ival = 4 will force all future output BUFR messages to be encoded
!> using BUFR edition 4; otherwise, messages will be encoded using BUFR edition 3 by default.
!> - Values corresponding to s01mnem = 'YCEN' or 'CENT' can only be stored within BUFR messages encoded using BUFR edition 3.
!> - Values corresponding to s01mnem = 'YEAR', 'SECO' or 'MSBTI' can only be stored within BUFR messages encoded using BUFR
!> edition 4.
!>
!> @author J. Ator @date 2005-11-29
recursive subroutine pkvs01(s01mnem,ival)

  use modv_vars, only: im8b, mxs01v

  use moda_s01cm

  implicit none

  character*(*), intent(in) :: s01mnem

  integer, intent(in) :: ival
  integer my_ival, i

  character*128 bort_str

  ! check for i8 integers

  if(im8b) then
    im8b=.false.

    call x84(ival,my_ival,1)
    call pkvs01(s01mnem,my_ival)

    im8b=.true.
    return
  endif

  ! Confirm that the arrays needed by this subroutine have already been allocated (and if not, go ahead and allocate them now),
  ! since it's possible for this subroutine to be called before the first call to subroutine openbf().

  if ( ( .not. allocated(cmnem) ) .or. ( .not. allocated(ivmnem) ) ) then
    call openbf(0,'FIRST',0)
  endif

  ! If an ival has already been assigned for this particular s01mnem, then overwrite that entry in module @ref moda_s01cm
  ! using the new ival.

  if(ns01v.gt.0) then
    do i=1,ns01v
      if(s01mnem.eq.cmnem(i)) then
        ivmnem(i) = ival
        return
      endif
    enddo
  endif

  ! Otherwise, use the next available unused entry in module @ref moda_s01cm.

  if(ns01v.ge.mxs01v) then
    write(bort_str,'("BUFRLIB: PKVS01 - CANNOT OVERWRITE MORE THAN ",I2," DIFFERENT LOCATIONS WITHIN SECTION 0 '// &
      'OR SECTION 1")') mxs01v
    call bort(bort_str)
  endif

  ns01v = ns01v + 1
  cmnem(ns01v) = s01mnem
  ivmnem(ns01v) = ival

  return
end subroutine pkvs01

!> Read the Section 3 descriptors from the BUFR message in mbay(1,lun), then use the BUFR master tables to generate the
!> necessary information for those descriptors within the internal BUFR table arrays.
!>
!> @param[in] lun - integer: file ID
!>
!> @author J. Ator @date 2009-03-23
subroutine reads3 ( lun )

  use bufrlib

  use modv_vars, only: maxnc, mxcnem

  use moda_sc3bfr
  use moda_bitbuf
  use moda_dscach
  use moda_s3list

  implicit none

  integer, intent(in) :: lun
  integer iprt, irepct, ireadmt, igettdi, itmp, ncds3, ii, jj, ifxy, igetntbi, n, idn

  character*6 numb, adn30
  character*55 cseq
  character*128 errstr

  logical incach

  common /quiet/ iprt

  save irepct

  ! Check whether the appropriate BUFR master table information has already been read into internal memory for this message.

  if ( ireadmt ( lun ) .eq. 1 ) then
    ! NO (i.e. we just had to read in new master table information for this message), so reset some corresponding values in
    ! other parts of the library.
    call dxinit ( lun, 0 )
    itmp = igettdi ( 0 )
    irepct = 0
    ncnem = 0
  endif

  ! Unpack the list of Section 3 descriptors from the message.

  call upds3 ( mbay(1,lun), maxnc, cds3, ncds3 )
  do ii = 1, ncds3
    ids3(ii) = ifxy( cds3(ii) )
  enddo

  ! Is the list of Section 3 descriptors already in the cache?

  ! The cache is a performance-enhancing device which saves time when the same descriptor sequences are encountered over and
  ! over within the calling program.  Time is saved because the below calls to subroutines stseq_c() and makestab() are
  ! bypassed whenever a list is already in the cache.

  incach = .false.
  if ( ncnem .gt. 0 ) then
    ii = 1
    do while ( (.not.incach) .and. (ii.le.ncnem) )
      if ( ncds3 .eq. ndc(ii) ) then
        jj = 1
        incach = .true.
        do while ( (incach) .and. (jj.le.ncds3) )
          if ( ids3(jj) .eq. idcach(ii,jj) ) then
            jj = jj + 1
          else
            incach = .false.
          endif
        enddo
        if (incach) then

          ! The list is already in the cache, so store the corresponding Table A mnemonic into module @ref moda_sc3bfr and return.

          if ( iprt .ge. 2 ) then
            call errwrt('+++++++++++++++++++++++++++++++++++++++++++++++++')
            errstr = 'BUFRLIB: READS3 - RE-USED CACHE LIST FOR ' // cnem(ii)
            call errwrt(errstr)
            call errwrt('+++++++++++++++++++++++++++++++++++++++++++++++++')
            call errwrt(' ')
          endif
          tamnem(lun) = cnem(ii)
          return
        endif
      endif
      ii = ii + 1
    enddo
  endif

  ! Get the next available index within the internal Table A.

  n = igetntbi ( lun, 'A' )

  ! Generate a Table A mnemonic and sequence description.

  write ( tamnem(lun), '(A5,I3.3)') 'MSTTB', n
  cseq = 'TABLE A MNEMONIC ' // tamnem(lun)

  ! Store the Table A mnemonic and sequence into the cache.

  ncnem = ncnem + 1
  if ( ncnem .gt. mxcnem ) call bort('BUFRLIB: READS3 - MXCNEM OVERFLOW')
  cnem(ncnem) = tamnem(lun)
  ndc(ncnem) = ncds3
  do jj = 1, ncds3
    idcach(ncnem,jj) = ids3(jj)
  enddo
  if ( iprt .ge. 2 ) then
    call errwrt('+++++++++++++++++++++++++++++++++++++++++++++++++')
    errstr = 'BUFRLIB: READS3 - STORED CACHE LIST FOR ' // cnem(ncnem)
    call errwrt(errstr)
    call errwrt('+++++++++++++++++++++++++++++++++++++++++++++++++')
    call errwrt(' ')
  endif

  ! Get an FXY value to use with this Table A mnemonic.

  idn = igettdi ( lun )
  numb = adn30 ( idn, 6 )

  ! Store all of the information for this mnemonic within the internal Table A.

  call stntbia ( n, lun, numb, tamnem(lun), cseq )

  ! Store all of the information for this sequence within the internal Tables B and D.

  call stseq_c ( lun, irepct, idn, tamnem(lun), cseq, ids3, ncds3 )

  ! Update the jump/link table.

  call makestab

  return
end subroutine reads3

!> Read the sequence of data descriptors contained within Section 3 of a BUFR message.
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of mbay
!> - This subroutine does not recursively resolve any Table D descriptors from within Section 3; rather, what is returned in
!> CDS3 is the exact list of data descriptors as it appears within Section 3 of mbay
!>
!> @param[in]  mbay - integer(*): BUFR message
!> @param[in]  lcds3 - integer: Dimensioned size (in integers) of cds3 in the calling program; used by the subroutine to
!> ensure that it doesn't overflow the cds3 array
!> @param[out] cds3 - character*6(*): Data descriptor sequence within Section 3 of mbay
!> @param[out] nds3 - integer: Number of data descriptors in cds3
!>
!> @author J. Ator @date 2003-11-04
recursive subroutine upds3(mbay,lcds3,cds3,nds3)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: mbay(*), lcds3
  integer, intent(out) :: nds3
  integer my_lcds3, len0, len1, len2, len3, l4, l5, ipt, jj, iupb

  character*6, intent(out) :: cds3(*)
  character*6 adn30

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(lcds3,my_lcds3,1)
    call upds3(mbay,my_lcds3,cds3,nds3)
    call x48(nds3,nds3,1)

    im8b=.true.
    return
  endif

  ! Skip to the beginning of Section 3.

  call getlens(mbay,3,len0,len1,len2,len3,l4,l5)
  ipt = len0 + len1 + len2

  ! Unpack the Section 3 descriptors.

  nds3 = 0
  do jj = 8,(len3-1),2
   nds3 = nds3 + 1
   if(nds3.gt.lcds3) call bort('BUFRLIB: UPDS3 - OVERFLOW OF OUTPUT DESCRIPTOR ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
   cds3(nds3) = adn30(iupb(mbay,ipt+jj,16),6)
  enddo

  return
end subroutine upds3

!> Specify the format of Section 1 date-time values that will be output by future calls to
!> any of the NCEPLIBS-bufr [message-reading subroutines](@ref hierarchy).
!>
!> This subroutine can be called at any time from within the
!> application program, and the specified value for len will remain
!> in effect for all future calls to any of the NCEPLIBS-bufr subroutines
!> which read BUFR messages, unless a subsequent call is made to this
!> subroutine to reset the value of len again.  If this subroutine is
!> never called, a default value of 8 is used for len, as set within
!> subroutine bfrini().
!>
!> @param[in] len - integer: Length of Section 1 date-time values to be output by all future calls to
!> message-reading subroutines:
!>  -  8 = YYMMDDHH format with 2-digit year (the default)
!>  - 10 = YYYYMMDDHH format with 4-digit year
!>
!> @author J. Woollen @date 1998-07-08
recursive subroutine datelen(len)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: len
  integer my_len, lendat

  character*128 bort_str

  common /dateln/ lendat

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(len,my_len,1)
    call datelen(my_len)

    im8b=.true.
    return
  endif

  if(len.ne.8 .and. len.ne.10) then
    write(bort_str,'("BUFRLIB: DATELEN - INPUT ARGUMENT IS",I4," - IT MUST BE EITHER 8 OR 10")') len
    call bort(bort_str)
  endif
  lendat = len

  return
end subroutine datelen

!> Get the Section 1 date-time from the first data message of a BUFR file, bypassing any messages
!> at the beginning of the file which may contain embedded DX BUFR table information.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[out] mear - integer: Year stored within Section 1 of first data message, in format of either YY or YYYY,
!> depending on the most recent call to subroutine datelen()
!> @param[out] mmon - integer: Month stored within Section 1 of first data message
!> @param[out] mday - integer: Day stored within Section 1 of first data message
!> @param[out] mour - integer: Hour stored within Section 1 of first data message
!> @param[out] idate - integer: Date-time stored within Section 1 of first data message, in format of either
!> YYMMDDHH or YYYYMMDDHH, depending on the most recent call to subroutine datelen()
!>   -1 = First data message could not be found in BUFR file
!>
!> Logical unit lunit must already be associated with a filename on the local system, typically via a Fortran "OPEN" statement.
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine datebf(lunit,mear,mmon,mday,mour,idate)

  use modv_vars, only: im8b

  use moda_mgwa

  implicit none

  integer, intent(in) :: lunit
  integer, intent(out) :: mear, mmon, mday, mour, idate
  integer my_lunit, iprt, lun, jl, jm, ier, idx, idxmsg, igetdate

  character*128 errstr

  common /quiet/ iprt

  !  Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call datebf(my_lunit,mear,mmon,mday,mour,idate)
    call x48(mear,mear,1)
    call x48(mmon,mmon,1)
    call x48(mday,mday,1)
    call x48(mour,mour,1)
    call x48(idate,idate,1)

    im8b=.true.
    return
  endif

  ! Initialization, in case openbf() hasn't been called yet.

  if ( .not. allocated(mgwa) ) call openbf(lunit,'FIRST',lunit)

  ! See if the file is already open to the library (a no-no!).

  call status(lunit,lun,jl,jm)
  if(jl.ne.0) call bort ('BUFRLIB: DATEBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED')

  ! Read to the first data message and pick out the date.

  call openbf(lunit,'INX',lunit)
  idx = 1
  do while (idx==1)
    call rdmsgw(lunit,mgwa,ier)
    if(ier.lt.0) then
      if (iprt.ge.1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: DATEBF - SECTION 1 DATE COULD NOT BE LOCATED - RETURN WITH IDATE = -1'
        call errwrt(errstr)
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        call errwrt(' ')
      endif
      idate = -1
      call closbf(lunit)
      return
    endif
    idx = idxmsg(mgwa)
  end do
  idate = igetdate(mgwa,mear,mmon,mday,mour)
  call closbf(lunit)

  return
end subroutine datebf

!> Get the date-time from within Section 1 of a BUFR message.
!>
!> The function will work on any BUFR message encoded using BUFR edition 2, 3, or 4.
!>
!> @param[in] mbay - integer(*): BUFR message
!> @param[out] iyr - integer: Year stored within Section 1 of mbay, in format of either YY or YYYY,
!> depending on the most recent call to subroutine datelen()
!> @param[out] imo - integer: Month stored within Section 1 of mbay
!> @param[out] idy - integer: Day stored within Section 1 of mbay
!> @param[out] ihr - integer: Hour stored within Section 1 of mbay
!> @returns igetdate - integer: Date-time stored within Section 1 of mbay, in format of either YYMMDDHH or YYYYMMDDHH,
!> depending on the most recent call to subroutine datelen()
!>
!> @remarks
!> - The start of the BUFR message (i.e. the string 'BUFR') must be aligned on the first 4 bytes of mbay
!>
!> @author J. Ator @date 2005-11-29
recursive integer function igetdate(mbay,iyr,imo,idy,ihr) result(iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: mbay(*)
  integer, intent(out) :: iyr, imo, idy, ihr
  integer lendat, iupbs01

  common /dateln/ lendat

  ! Check for I8 integers.

  if(im8b) then
     im8b=.false.

     iret=igetdate(mbay,iyr,imo,idy,ihr)
     call x48(iyr,iyr,1)
     call x48(imo,imo,1)
     call x48(idy,idy,1)
     call x48(ihr,ihr,1)

     im8b=.true.
     return
  endif

  iyr = iupbs01(mbay,'YEAR')
  imo = iupbs01(mbay,'MNTH')
  idy = iupbs01(mbay,'DAYS')
  ihr = iupbs01(mbay,'HOUR')
  if(lendat.ne.10) iyr = mod(iyr,100)
  iret = (iyr*1000000) + (imo*10000) + (idy*100) + ihr

  return
end function igetdate

!> Convert a date-time with a 2-digit year (YYMMDDHH) to a date-time with a 4-digit year (YYYYMMDDHH) using a
!> windowing technique.
!>
!> All 2-digit years greater than 40 are assumed to have a 4-digit
!> year beginning with 19 (i.e. 1941-1999), and all 2-digit years less
!> than or equal to 40 are assumed to have a 4-digit year beginning
!> with 20 (i.e. 2000-2040).  If the input date-time already contains
!> a 4-digit year, then the function simply returns that value.
!>
!> @param[in] idate - integer: Date-time in format of either YYMMDDHH (2-digit year) or YYYYMMDDHH (4-digit year)
!> @returns i4dy - integer: Date-time in format of YYYYMMDDHH (4-digit year)
!>
!> @author J. Woollen @date 1998-07-08
recursive integer function i4dy(idate) result(iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: idate
  integer my_idate, iy

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(idate,my_idate,1)
    iret=i4dy(my_idate)

    im8b=.true.
    return
  endif

  if(idate.lt.10**8) then
    iy = idate/10**6
    if(iy.gt.40) then
       iret = idate + 19*100000000
    else
       iret = idate + 20*100000000
    endif
  else
    iret = idate
  endif

  return
end function i4dy
