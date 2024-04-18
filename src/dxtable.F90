!> @file
!> @brief Read or write DX BUFR table information.
!>
!> @author Woollen @date 1994-01-06

!> Initialize modules @ref moda_tababd and @ref moda_msgcwd with DX BUFR (dictionary) tables. These tables are needed
!> to read, write, initialize or append a BUFR file.
!>
!> The modules are initialized from either:
!> 1. an external, user-supplied BUFR dictionary table file (i.e., a BUFR mnemonic table), or
!> 2. the BUFR file indicated by lunit, or
!> 3. another currently opened BUFR file.
!>
!> If the modules are initialized by the BUFR file indicated by lunit,
!> then it must have been opened for input processing and positioned at a
!> dictionary table message somewhere in the file.
!>
!> Once initialzed, the dictionary arrays are associated with the BUFR
!> file indicated by lunit, until the file is closed with closbf().
!>
!> @param lunit - Fortran logical unit number for BUFR file being read, written, initialized or appended
!> @param lun - file ID associated with LUNIT
!> @param lundx - Fortran logical unit number containing dictionary table information to be used in reading/writing from/to
!> lunit (depending on the case); may be set equal to lunit if dictionary table information is already embedded in lunit
!> (but only if lunit is being read)
!>
!> @author Woollen @date 1994-01-06
subroutine readdx(lunit,lun,lundx)

  implicit none

  integer, intent(in) :: lunit, lun, lundx
  integer iprt, lud, ildx, imdx

  character*128 errstr

  common /quiet/ iprt

  ! Get the status of unit lundx

  call status(lundx,lud,ildx,imdx)

  ! Read a dictionary table from the indicated source

  if (lunit.eq.lundx) then
    ! Source is input BUFR file in lunit
    if(iprt.ge.2) then
      call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I3,A)' ) 'BUFRLIB: READDX - READING BUFR DICTIONARY TABLE FROM ', &
        'INPUT BUFR FILE IN UNIT ', lundx, ' INTO INTERNAL ARRAYS'
      call errwrt(errstr)
      call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      call errwrt(' ')
    endif
    rewind lunit
    call rdbfdx(lunit,lun)
  elseif(ildx.eq.-1) then
    ! Source is input BUFR file in lundx; BUFR file in lunit may be input or output
    if(iprt.ge.2) then
      call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I3,A,A,I3)' ) 'BUFRLIB: READDX - COPYING BUFR DCTY TBL FROM INTERNAL ', &
        'ARRAYS ASSOC. W/ INPUT UNIT ', lundx, ' TO THOSE ASSOC. W/ UNIT ', lunit
      call errwrt(errstr)
      call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      call errwrt(' ')
    endif
    call cpbfdx(lud,lun)
    call makestab
  elseif(ildx.eq.1) then
    ! Source is output BUFR file in lundx; BUFR file in lunit may be input or output
    if(iprt.ge.2) then
      call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I3,A,A,I3)' ) 'BUFRLIB: READDX - COPYING BUFR DCTY TBL FROM INTERNAL ', &
        'ARRAYS ASSOC. W/ OUTPUT UNIT ', lundx, ' TO THOSE ASSOC. W/ UNIT ', lunit
      call errwrt(errstr)
      call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      call errwrt(' ')
    endif
    call cpbfdx(lud,lun)
    call makestab
  elseif(ildx.eq.0) then
    ! Source is user-supplied character table in lundx; BUFR file in lunit may be input or output
    if(iprt.ge.2) then
      call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I3,A)' ) 'BUFRLIB: READDX - READING BUFR DICTIONARY TABLE FROM ', &
        'USER-SUPPLIED TEXT FILE IN UNIT ', lundx, ' INTO INTERNAL ARRAYS'
      call errwrt(errstr)
      call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
      call errwrt(' ')
    endif
    rewind lundx
    call rdusdx(lundx,lun)
  else
    call bort('BUFRLIB: READDX - CANNOT DETERMINE SOURCE OF INPUT DICTIONARY TABLE')
  endif

  return
end subroutine readdx

!> Beginning at the current file pointer location within lunit,
!> read a complete DX BUFR table into internal memory arrays
!> in module @ref moda_tababd.  A DX BUFR table consists of one or more
!> consecutive DX BUFR messages.
!>
!> This subroutine performs a function similar to
!> rdusdx(), except that rdusdx() reads from a file containing
!> a user-supplied DX BUFR table in character format. See rdusdx()
!> for a description of the arrays that are filled
!> in module @ref moda_tababd.
!>
!> This subroutine performs a function similar to
!> cpdxmm(), except that cpdxmm() writes to the internal memory
!> arrays in module @ref moda_msgmem, for use with a file of BUFR messages that
!> is being read and stored into internal memory via subroutine ufbmem().
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param lun - File ID associated with lunit
!>
!> @author Woollen @date 1994-01-06
subroutine rdbfdx(lunit,lun)

  use bufrlib

  use moda_mgwa

  implicit none

  integer, intent(in) :: lunit, lun
  integer iprt, ict, ier, idxmsg, iupbs3

  character*128 errstr

  logical done

  common /quiet/ iprt

  call dxinit(lun,0)

  ict = 0
  done = .false.

  ! Read a complete dictionary table from lunit, as a set of one or more DX dictionary messages.

  do while ( .not. done )
    call rdmsgw ( lunit, mgwa, ier )
    if ( ier .eq. -1 ) then
      ! Don't abort for an end-of-file condition, since it may be possible for a file to end with dictionary messages.
      ! Instead, backspace the file pointer and let the calling routine diagnose the end-of-file condition and deal with
      ! it as it sees fit.
      call backbufr_c(lun)
      done = .true.
    else if ( ier .eq. -2 ) then
      call bort('BUFRLIB: RDBFDX - ERROR READING A BUFR DICTIONARY MESSAGE')
    else if ( idxmsg(mgwa) .ne. 1 ) then
      ! This is a non-DX dictionary message.  Assume we've reached the end of the dictionary table, and backspace lunit
      ! so that the next read (e.g. in the calling routine) will get this same message.
      call backbufr_c(lun)
      done = .true.
    else if ( iupbs3(mgwa,'NSUB') .eq. 0 ) then
      ! This is a DX dictionary message, but it doesn't contain any actual dictionary information.  Assume we've reached
      ! the end of the dictionary table.
      done = .true.
    else
      ! Store this message into module @ref moda_tababd.
      ict = ict + 1
      call stbfdx(lun,mgwa)
    endif
  enddo

  if ( iprt .ge. 2 ) then
    call errwrt('+++++++++++++++++++++++++++++++++++++++++++++++++')
    write ( unit=errstr, fmt='(A,I3,A)' ) 'BUFRLIB: RDBFDX - STORED NEW DX TABLE CONSISTING OF (', ict, ') MESSAGES;'
    call errwrt(errstr)
    errstr = 'WILL APPLY THIS TABLE TO ALL SUBSEQUENT DATA IN FILE UNTIL NEXT DX TABLE IS FOUND'
    call errwrt(errstr)
    call errwrt('+++++++++++++++++++++++++++++++++++++++++++++++++')
    call errwrt(' ')
  endif

  call makestab

  return
end subroutine rdbfdx

!> Read and parse a file containing a user-supplied DX BUFR table in character format, then store
!> this information into internal arrays in module @ref moda_tababd.
!>
!> This subroutine performs a function similar to subroutine rdbfdx(),
!> except that rdbfdx() reads the DX BUFR table directly from messages at
!> the beginning of an input BUFR file.
!>
!> @param lundx - Fortran logical unit number for user-supplied DX BUFR table in character format
!> @param lun - File ID
!>
!> @author Woollen @date 1994-01-06
subroutine rdusdx(lundx,lun)

  use moda_tababd

  implicit none

  integer, intent(in) :: lundx, lun
  integer ios, iret, n, numbck, nemock, igetntbi

  character*128 bort_str1
  character*156 bort_str2
  character*80 card
  character*8 nemo
  character*6 numb, nmb2

  ! Initialize the dictionary table control word partition arrays with apriori Table B and D entries

  call dxinit(lun,1)
  rewind lundx

  ! Read user cards until there are no more

  do while (.true.)

    read(lundx, '(A80)', iostat = ios) card
    if (ios.ne.0) then
      call makestab
      return
    endif

    if(card(1: 1).eq.       '*') cycle  ! comment line
    if(card(3:10).eq.'--------') cycle  ! separation line
    if(card(3:10).eq.'        ') cycle  ! blank line
    if(card(3:10).eq.'MNEMONIC') cycle  ! header line
    if(card(3:10).eq.'TABLE  D') cycle  ! header line
    if(card(3:10).eq.'TABLE  B') cycle  ! header line

    if(card(12:12).eq.'|' .and. card(21:21).eq.'|') then

      ! Parse a descriptor definition card
      nemo = card(3:10)  ! nemo is the (up to) 8-character mnemonic
      iret=nemock(nemo)
      if(iret.eq.-2) then
        write(bort_str1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') card
        write(bort_str2,'(18X,"MNEMONIC ",A," IN USER DICTIONARY HAS INVALID CHARACTERS")') nemo
        call bort2(bort_str1,bort_str2)
      endif
      numb = card(14:19) ! numb is the 6-character FXY value corresponding to nemo
      nmb2 = numb
      if(nmb2(1:1).eq.'A') nmb2(1:1) = '3'
      iret=numbck(nmb2)
      if(iret.eq.-1) then
        write(bort_str1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') card
        write(bort_str2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
          'DICTIONARY HAS AN INVALID FIRST CHARACTER (F VALUE) - MUST BE A, 0 OR 3")') numb
        call bort2(bort_str1,bort_str2)
      endif
      if(iret.eq.-2) then
        write(bort_str1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') card
        write(bort_str2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
          'DICTIONARY HAS NON-NUMERIC VALUES IN CHARACTERS 2-6 (X AND Y VALUES)")') numb
        call bort2(bort_str1,bort_str2)
      endif
      if(iret.eq.-3) then
        write(bort_str1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') card
        write(bort_str2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
          'DICTIONARY HAS INVALID NUMBER IN CHARACTERS 2-3 (X VALUE) - MUST BE BETWEEN 00 AND 63")') numb
        call bort2(bort_str1,bort_str2)
      endif
      if(iret.eq.-4) then
        write(bort_str1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') card
        write(bort_str2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
          'DICTIONARY HAS INVALID NUMBER IN CHARACTERS 4-6 (Y VALUE) - MUST BE BETWEEN 000 AND 255")') numb
        call bort2(bort_str1,bort_str2)
      endif

      if(numb(1:1).eq.'A') then
        ! Table A descriptor found
        n = igetntbi ( lun, 'A' )
        call stntbia ( n, lun, numb, nemo, card(23:) )
        if ( idna(n,lun,1) .eq. 11 ) then
          write(bort_str1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') card
          write(bort_str2,'(18X,"USER-DEFINED MESSAGE TYPE ""011"" IS RESERVED FOR DICTIONARY MESSAGES")')
          call bort2(bort_str1,bort_str2)
        endif
        ! Replace "A" with "3" so Table D descriptor will be found in card as well (see below).
        numb(1:1) = '3'
      endif

      if(numb(1:1).eq.'0') then
        ! Table B descriptor found
        call stntbi ( igetntbi(lun,'B'), lun, numb, nemo, card(23:) )
        cycle
      endif

      if(numb(1:1).eq.'3') then
        ! Table D descriptor found
        call stntbi ( igetntbi(lun,'D'), lun, numb, nemo, card(23:) )
        cycle
      endif

      write(bort_str1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') card
      write(bort_str2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
        'DICTIONARY HAS AN INVALID FIRST CHARACTER (F VALUE) - MUST BE A, 0 OR 3")') numb
      call bort2(bort_str1,bort_str2)

    endif

    if(card(12:12).eq.'|' .and. card(19:19).ne.'|') then
      ! Parse a sequence definition card
      call seqsdx(card,lun)
      cycle
    endif

    if(card(12:12).eq.'|' .and. card(19:19).eq.'|') then
      ! Parse an element definition card
      call elemdx(card,lun)
      cycle
    endif

    ! Can't figure out what kind of card it is
    write(bort_str1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') card
    write(bort_str2,'(18X,"THIS CARD HAS A BAD FORMAT - IT IS NOT RECOGNIZED BY THIS SUBROUTINE")')
    call bort2(bort_str1,bort_str2)
  enddo

end subroutine rdusdx

!> Decode the Table D sequence information from a mnemonic definition card that was previously read from a
!> user-supplied DX BUFR table in character format by subroutine rdusdx(), then add this information to the
!> already-existing entry for that mnemonic within the internal BUFR Table D arrays in module @ref moda_tababd.
!>
!> @param card - Mnemonic definition card that was read from a user-supplied DX BUFR table
!> @param lun - File ID
!>
!> @author Woollen @date 1994-01-06
subroutine seqsdx(card,lun)

  implicit none

  integer, intent(in) :: lun
  integer idnr, lens, maxtgs, maxtag, ntag, idn, jdn, iseq, irep, i, j, n, itab, iret, ier, numr, nemock

  character*128 bort_str1, bort_str2
  character*80 seqs
  character*80, intent(in) :: card
  character*12 atag, tags(250)
  character*8 nemo, nema, nemb
  character*6 adn30, clemon
  character*3 typs
  character reps, tab

  common /reptab/ idnr(5,2), typs(5,2), reps(5,2), lens(5)

  data maxtgs /250/
  data maxtag /12/

  ! Find the sequence tag in Table D and parse the sequence string

  nemo = card( 3:10)
  seqs = card(14:78)

  ! Note that an entry for this mnemonic should already exist within the internal BUFR Table D array tabd(*,LUN); this entry
  ! should have been created by subroutine rdusdx() when the mnemonic and its associated FXY value and description were
  ! initially defined within a card read from the "Descriptor Definition" section at the top of the user-supplied DX BUFR
  ! table in character format.  Now, we need to retrieve the positional index for that  entry within tabd(*,lun) so that we
  ! can access the entry and then add the decoded sequence information to it.

  call nemtab(lun,nemo,idn,tab,iseq)
  if(tab.ne.'D') then
    write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
    write(bort_str2,'(18X,"MNEMONIC ",A," IS NOT A TABLE D ENTRY (UNDEFINED, TAB=",A,")")') nemo,tab
    call bort2(bort_str1,bort_str2)
  endif
  call parstr(seqs,tags,maxtgs,ntag,' ',.true.)
  if(ntag.eq.0) then
    write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
    write(bort_str2,'(18X,"TABLE D SEQUENCE (PARENT) MNEMONIC ",A," DOES NOT CONTAIN ANY CHILD MNEMONICS")') nemo
    call bort2(bort_str1,bort_str2)
  endif

  do n=1,ntag
    atag = tags(n)
    irep = 0

    ! Check for a replicator

    outer: do i=1,5
      if(atag(1:1).eq.reps(i,1)) then
        ! Note that reps(*,*), which contains all of the symbols used to denote all of the various replication schemes that
        ! are possible within a user-supplied BUFR dictionary table in character format, was previously defined within
        ! subroutine bfrini().
        do j=2,maxtag
          if(atag(j:j).eq.reps(i,2)) then
            ! Note that subroutine strnum() will return numr = 0 if the string passed to it contains all blanks
            ! (as *should* be the case whenever i = 2 '(' ')', 3 '{' '}', 4 '[' ']', or 5 '<' '>').
            ! However, when i = 1 '"' '"', then subroutine strnum() will return numr = (the number of replications for
            ! the mnemonic using F=1 "regular" (i.e. non-delayed) replication).
            call strnum(atag(j+1:maxtag),numr,ier)
            if(i.eq.1 .and. numr.le.0) then
              write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
              write(bort_str2,'(9X,"TBL D MNEM. ",A," CONTAINS REG. REPL. '// &
                'CHILD MNEM. ",A," W/ INVALID # OF REPLICATIONS (",I3,") AFTER 2ND QUOTE")') nemo,tags(n),numr
              call bort2(bort_str1,bort_str2)
            endif
            if(i.eq.1 .and. numr.gt.255) then
              write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
              write(bort_str2,'(18X,"TBL D MNEM. ",A," CONTAINS REG. REPL. '// &
                'CHILD MNEM. ",A," W/ # OF REPLICATIONS (",I3,") > LIMIT OF 255")') nemo,tags(n),numr
              call bort2(bort_str1,bort_str2)
            endif
            if(i.ne.1 .and. numr.ne.0) then
              write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
              write(bort_str2,'(18X,"TBL D MNEM. ",A," CONTAINS DELAYED REPL. '// &
                'CHILD MNEM. ",A," W/ # OF REPL. (",I3,") SPECIFIED - A NO-NO")') nemo,tags(n),numr
              call bort2(bort_str1,bort_str2)
            endif
            atag = atag(2:j-1)
            irep = i
            exit outer
          endif
        enddo
        write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
        write(bort_str2,'(18X,"TABLE D SEQUENCE (PARENT) MNEMONIC ",A,'// &
          '" CONTAINS A BADLY FORMED CHILD MNEMONIC ",A)') nemo,tags(n)
        call bort2(bort_str1,bort_str2)
      endif
    enddo outer

    ! Check for a valid tag

    iret=nemock(atag)
    if(iret.eq.-1) then
      write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
      write(bort_str2,'(18X,"TABLE D (PARENT) MNEMONIC ",A," CONTAINS'// &
        ' A CHILD MNEMONIC ",A," NOT BETWEEN 1 & 8 CHARACTERS")') nemo,tags(n)
      call bort2(bort_str1,bort_str2)
    endif
    if(iret.eq.-2) then
      write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
      write(bort_str2,'(18X,"TABLE D (PARENT) MNEMONIC ",A," CONTAINS'// &
        ' A CHILD MNEMONIC ",A," WITH INVALID CHARACTERS")') nemo,tags(n)
      call bort2(bort_str1,bort_str2)
    endif
    call nemtab(lun,atag,idn,tab,iret)
    if(iret.gt.0) then
      ! Note that the next code line checks that we are not trying to replicate a Table B mnemonic (which is currently not
      ! allowed).  The logic works because, for replicated mnemonics, irep = i = (the index within reps(*,*) of the symbol
      ! associated with the type of replication in question (e.g. "{, "<", etc.))
      if(tab.eq.'B' .and. irep.ne.0) then
        write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
        write(bort_str2,'(18X,"TABLE D (PARENT) MNEMONIC ",A," CONTAINS'// &
          ' A REPLICATED CHILD TABLE B MNEMONIC ",A," - A NO-NO")') nemo,tags(n)
        call bort2(bort_str1,bort_str2)
      endif
      if(atag(1:1).eq.'.') then
        ! This mnemonic is a "following value" mnemonic (i.e. it relates to the mnemonic that immediately follows it within
        ! the user-supplied character-format BUFR dictionary table sequence), so confirm that it contains, as a substring,
        ! this mnemonic that immediately follows it.
        if(n.eq.ntag) then
          write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
          write(bort_str2,'(18X,"TBL D (PARENT) MNEM. ",A," CONTAINS A '// &
            '''FOLLOWING VALUE'' MNEMONIC WHICH IS LAST IN THE STRING")') nemo
          call bort2(bort_str1,bort_str2)
        endif
        nemb = tags(n+1)(1:8)
        call numtab(lun,idn,nema,tab,itab)
        call nemtab(lun,nemb,jdn,tab,iret)
        call rsvfvm(nema,nemb)
        if(nema.ne.atag) then
          write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
          write(bort_str2,'(18X,"TBL D (PARENT) MNEM. ",A," CONTAINS AN '// &
            'INVALID ''FOLLOWING VALUE'' MNEMONIC ",A,"(SHOULD BE ",A,")")') nemo,tags(n),nema
          call bort2(bort_str1,bort_str2)
        endif
        if(tab.ne.'B') then
          write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
          write(bort_str2,'(18X,"TBL D (PARENT) MNEM. ",A,", THE MNEM. ",'// &
            'A," FOLLOWING A ''FOLLOWING VALUE'' MNEM. IS NOT A TBL B ENTRY")') nemo,nemb
          call bort2(bort_str1,bort_str2)
        endif
      endif
    else
      write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
      write(bort_str2,'(18X,"TABLE D SEQUENCE (PARENT) MNEMONIC ",A,'// &
        '" CONTAINS A CHILD MNEMONIC ",A," NOT FOUND IN ANY TABLE")') nemo,tags(n)
      call bort2(bort_str1,bort_str2)
    endif

    ! Write the descriptor string into the tabd array, but first look for a replication descriptor
    if(irep.gt.0) call pktdd(iseq,lun,idnr(irep,1)+numr,iret)
    if(iret.lt.0) then
      clemon = adn30(idnr(irep,1)+numr,6)
      write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
      write(bort_str2,'(9X,"TBL D (PARENT) MNEM. ",A," - BAD RETURN '// &
        'FROM PKTDD TRYING TO STORE REPL. DESC. ",A,", SEE PREV. WARNING MSG")') nemo,clemon
      call bort2(bort_str1,bort_str2)
    endif
    call pktdd(iseq,lun,idn,iret)
    if(iret.lt.0) then
      write(bort_str1,'("BUFRLIB: SEQSDX - CARD READ IN IS: ",A)') card
      write(bort_str2,'(9X,"TBL D (PARENT) MNEM. ",A," - BAD RETURN '// &
        'FROM PKTDD TRYING TO STORE CHILD MNEM. ",A,", SEE PREV. WARNING MSG")') nemo,tags(n)
      call bort2(bort_str1,bort_str2)
    endif

  enddo

  return
end subroutine seqsdx

!> Decode the scale factor, reference value, bit width and units (i.e., the "elements") from a Table B mnemonic
!> definition card that was previously read from a user-supplied DX BUFR table file in character format by
!> subroutine rdusdx(). These decoded values are then added to the already-existing entry for that mnemonic
!> within the internal BUFR Table B array tabb(*,lun) in module @ref moda_tababd.
!>
!> @param card - Mnemonic definition card that was read from a user-supplied DX BUFR table
!> @param lun - File ID
!>
!> @author Woollen @date 1994-01-06
subroutine elemdx(card,lun)

  use moda_tababd

  implicit none

  integer, intent(in) :: lun
  integer idsn, iele, iret

  character*128 bort_str1, bort_str2
  character*80, intent(in) :: card
  character*24 unit
  character*11 refr, refr_orig
  character*8 nemo
  character*4 scal, scal_orig
  character*3 bitw, bitw_orig
  character sign, tab

  ! Capture the various elements characteristics

  nemo = card( 3:10)
  scal = card(14:17)
  refr = card(21:31)
  bitw = card(35:37)
  unit = card(41:64)
  ! Make sure the units are all capitalized
  call capit(unit)

  ! Find the element tag in Table B.  Note that an entry for this mnemonic should already exist within the internal
  ! BUFR Table B array tabb(*,lun).  We now need to retrieve the positional index for that entry within tabb(*,lun)
  ! so that we can access the entry and then add the scale factor, reference value, bit width, and units to it.

  call nemtab(lun,nemo,idsn,tab,iele)
  if(tab.ne.'B') then
    write(bort_str1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') card
    write(bort_str2,'(18X,"MNEMONIC ",A," IS NOT A TABLE B ENTRY (UNDEFINED, TAB=",A,")")') nemo,tab
    call bort2(bort_str1,bort_str2)
  endif

  ! Left justify and store characteristics

  unit = adjustl(unit)
  if(unit.eq.' ') then
    write(bort_str1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') card
    write(bort_str2,'(18X,"UNITS FIELD IS EMPTY")')
    call bort2(bort_str1,bort_str2)
  endif
  tabb(iele,lun)(71:94) = unit

  scal_orig=scal
  call jstnum(scal,sign,iret)
  if(iret.ne.0) then
    write(bort_str1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') card
    write(bort_str2,'(18X,"PARSED SCALE VALUE (=",A,") IS NOT NUMERIC")') scal_orig
    call bort2(bort_str1,bort_str2)
  endif
  tabb(iele,lun)(95:95) = sign
  tabb(iele,lun)(96:98) = scal(1:3)

  refr_orig=refr
  call jstnum(refr,sign,iret)
  if(iret.ne.0) then
    write(bort_str1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') card
    write(bort_str2,'(18X,"PARSED REFERENCE VALUE (=",A,") IS NOT NUMERIC")') refr_orig
    call bort2(bort_str1,bort_str2)
  endif
  tabb(iele,lun)( 99: 99) = sign
  tabb(iele,lun)(100:109) = refr(1:10)

  bitw_orig=bitw
  call jstnum(bitw,sign,iret)
  if(iret.ne.0 .or. sign.eq.'-') then
    write(bort_str1,'("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)') card
    write(bort_str2,'(18X,"PARSED BIT WIDTH VALUE (=",A,") IS NOT NUMERIC")') bitw_orig
    call bort2(bort_str1,bort_str2)
  endif
  tabb(iele,lun)(110:112) = bitw

  return
end subroutine elemdx

!> Clear out the internal arrays (in module @ref moda_tababd) holding the DX BUFR table, then optionally
!> initialize the table with apriori Table B and D entries.
!>
!> @param lun - File ID
!> @param ioi - Switch:
!> - 0 do not initialize the table with apriori Table B and D entries
!> - else initialize the table with apriori Table B and D entries
!>
!> @author Woollen @date 1994-01-06
subroutine dxinit(lun,ioi)

  use moda_tababd

  implicit none

  integer, intent(in) :: lun, ioi
  integer idnr, lens, ibct, ipd1, ipd2, ipd3, ipd4, ninib, ninid, n, i, iret, ifxy

  character*8 inib(6,5),inid(5)
  character*6 adn30
  character*3 typs
  character reps

  common /padesc/ ibct, ipd1, ipd2, ipd3, ipd4
  common /reptab/ idnr(5,2), typs(5,2), reps(5,2), lens(5)

  data inib  /'------','BYTCNT  ','BYTES  ','+0','+0','16', &
              '------','BITPAD  ','NONE   ','+0','+0','1 ', &
              '031000','DRF1BIT ','NUMERIC','+0','+0','1 ', &
              '031001','DRF8BIT ','NUMERIC','+0','+0','8 ', &
              '031002','DRF16BIT','NUMERIC','+0','+0','16'/
  data ninib /5/

  data inid  /'        ', &
              'DRP16BIT', &
              'DRP8BIT ', &
              'DRPSTAK ', &
              'DRP1BIT '/
  data ninid /5/

  ! Clear out a table partition

  ntba(lun) = 0
  do i=1,ntba(0)
    taba(i,lun) = ' '
    mtab(i,lun) = 0
  enddo

  ntbb(lun) = 0
  do i=1,ntbb(0)
    tabb(i,lun) = ' '
  enddo

  ntbd(lun) = 0
  do i=1,ntbd(0)
    tabd(i,lun) = ' '
    call pktdd(i,lun,0,iret)
  enddo

  if(ioi.eq.0) return

  ! Initialize table with apriori Table B and D entries

  inib(1,1) = adn30(ibct,6)
  inib(1,2) = adn30(ipd4,6)

  do i=1,ninib
    ntbb(lun) = ntbb(lun)+1
    idnb(i,lun) = ifxy(inib(1,i))
    tabb(i,lun)(  1:  6) = inib(1,i)(1:6)
    tabb(i,lun)(  7: 70) = inib(2,i)
    tabb(i,lun)( 71: 94) = inib(3,i)
    tabb(i,lun)( 95: 98) = inib(4,i)(1:4)
    tabb(i,lun)( 99:109) = inib(5,i)
    tabb(i,lun)(110:112) = inib(6,i)(1:3)
  enddo

  do i=2,ninid
    n = ntbd(lun)+1
    idnd(n,lun) = idnr(i,1)
    tabd(n,lun)(1: 6) = adn30(idnr(i,1),6)
    tabd(n,lun)(7:70) = inid(i)
    call pktdd(n,lun,idnr(1,1),iret)
    call pktdd(n,lun,idnr(i,2),iret)
    ntbd(lun) = n
  enddo

  return
end subroutine dxinit

!> Initialize a DX BUFR tables (dictionary) message, writing all the preliminary information into Sections 0, 1, 3, 4.
!> Subroutine wrdxtb() will be called later to add this information into the message.
!>
!> @param mbay - BUFR message
!> @param mbyt - Length (in bytes) of mbay
!> @param mb4 - Byte number in message of first byte in Section 4
!> @param mba - Byte number in message of fourth byte in Section 4
!> @param mbb - Byte number in message of fifth byte in Section 4
!> @param mbd - Byte number in message of sixth byte in Section 4
!>
!> @author Woollen @date 1994-01-06
subroutine dxmini(mbay,mbyt,mb4,mba,mbb,mbd)

  use modv_vars, only: mxmsgld4

  implicit none

  integer, intent(out) :: mbay(*), mbyt, mb4, mba, mbb, mbd
  integer maxdx, idxv, nxstr, ldxa, ldxb, ldxd, ld30, mtyp, msbt, mbit, ih, id, im, iy, i, nsub, idxs, ldxs, &
    nby0, nby1, nby2, nby3, nby4, nby5, iupm

  character*128 bort_str
  character*56 dxstr

  common /dxtab/ maxdx, idxv, nxstr(10), ldxa(10), ldxb(10), ldxd(10), ld30(10), dxstr(10)

  msbt = idxv

  ! Initialize the message

  mbit = 0
  do i=1,mxmsgld4
    mbay(i) = 0
  enddo

  ! For DX table messages, the Section 1 date is simply zeroed out.  Note that there is logic in function idxmsg()
  ! which relies on this.
  ih = 0
  id = 0
  im = 0
  iy = 0

  mtyp = 11  ! DX table messages are always type 11, per WMO BUFR Table A
  nsub = 1

  idxs = idxv+1
  ldxs = nxstr(idxs)

  nby0 = 8
  nby1 = 18
  nby2 = 0
  nby3 = 7 + nxstr(idxs) + 1
  nby4 = 7
  nby5 = 4
  mbyt = nby0+nby1+nby2+nby3+nby4+nby5

  if(mod(nby3,2).ne.0) call bort ('BUFRLIB: DXMINI - LENGTH OF SECTION 3 IS NOT A MULTIPLE OF 2')

  ! Section 0

  call pkc('BUFR' ,  4 , mbay,mbit)
  call pkb(  mbyt , 24 , mbay,mbit)
  call pkb(     3 ,  8 , mbay,mbit)

  ! Section 1

  call pkb(  nby1 , 24 , mbay,mbit)
  call pkb(     0 ,  8 , mbay,mbit)
  call pkb(     3 ,  8 , mbay,mbit)
  call pkb(     7 ,  8 , mbay,mbit)
  call pkb(     0 ,  8 , mbay,mbit)
  call pkb(     0 ,  8 , mbay,mbit)
  call pkb(  mtyp ,  8 , mbay,mbit)
  call pkb(  msbt ,  8 , mbay,mbit)
  call pkb(    36 ,  8 , mbay,mbit)
  call pkb(  idxv ,  8 , mbay,mbit)
  call pkb(    iy ,  8 , mbay,mbit)
  call pkb(    im ,  8 , mbay,mbit)
  call pkb(    id ,  8 , mbay,mbit)
  call pkb(    ih ,  8 , mbay,mbit)
  call pkb(     0 ,  8 , mbay,mbit)
  call pkb(     0 ,  8 , mbay,mbit)

  ! Section 3

  call pkb(  nby3 , 24 , mbay,mbit)
  call pkb(     0 ,  8 , mbay,mbit)
  call pkb(     1 , 16 , mbay,mbit)
  call pkb(  2**7 ,  8 , mbay,mbit)
  do i=1,ldxs
    call pkb(iupm(dxstr(idxs)(i:i),8),8,mbay,mbit)
  enddo
  call pkb(     0 ,  8 , mbay,mbit)

  ! Section 4

  mb4 = mbit/8+1
  call pkb(  nby4 , 24 , mbay,mbit)
  call pkb(     0 ,  8 , mbay,mbit)
  mba = mbit/8+1
  call pkb(     0 ,  8 , mbay,mbit)
  mbb = mbit/8+1
  call pkb(     0 ,  8 , mbay,mbit)
  mbd = mbit/8+1
  call pkb(     0 ,  8 , mbay,mbit)

  if(mbit/8+nby5.ne.mbyt) then
    write(bort_str,'("BUFRLIB: DXMINI - NUMBER OF BYTES STORED FOR '// &
      'A MESSAGE (",I6,") IS NOT THE SAME AS FIRST CALCULATED, MBYT (",I6)') mbit/8+nby5,mbyt
    call bort(bort_str)
  endif

  return
end subroutine dxmini

!> Write DX BUFR table (dictionary) messages to the beginning of an output BUFR file in lunit.
!>
!> The table messages are read from arrays in internal memory (module @ref moda_tababd).
!> An initial call to subroutine readdx() generates these internal arrays.
!>
!> @param lunit - Fortran logical unit number for BUFR file being written.
!> @param lun - File ID of open BUFR file.
!> @param lundx - Fortran logical unit number containing dictionary table information to be used (by readdx())
!> to create internal tables written to lunit; if set equal to lunit, this subroutine calls bort().
!>
!> @author Woollen @date 1994-01-06
subroutine writdx(lunit,lun,lundx)

  implicit none

  integer, intent(in) :: lunit, lun, lundx

  character*128 bort_str

  ! The table must be coming from an input file

  if(lunit.eq.lundx) then
    write(bort_str,'("BUFRLIB: WRITDX - FILES CONTAINING BUFR DATA '// &
      'AND DICTIONARY TABLE CANNOT BE THE SAME (HERE BOTH SHARE FORTRAN UNIT NUMBER ",I3,")")') lunit
    call bort(bort_str)
  endif

  ! Must first call readdx() to generate internal dictionary table arrays, before calling wrdxtb()

  call readdx(lunit,lun,lundx)
  call wrdxtb(lunit,lunit)

  return
end subroutine writdx

!> Generate one or more BUFR messages from the DX BUFR tables information associated with a given BUFR file,
!> then write out those messages to the same or possibly a different BUFR file.
!>
!> Logical units lundx and lunot should have already been
!> opened via previous calls to subroutine openbf(), and in
!> particular logical unit lunot must have been opened for
!> output operations.  lundx and lunot may be the same if it is
!> desired to append to lunot with DX BUFR messages generated
!> from its own internal tables.
!>
!> @param lundx - Fortran logical unit number associated with DX BUFR table information to be written out
!> @param lunot - Fortran logical unit number of BUFR file to which messages are to be written
!>
!> @author J. Ator @date 2009-03-23
recursive subroutine wrdxtb(lundx,lunot)

  use modv_vars, only: im8b

  use moda_tababd
  use moda_mgwa

  implicit none

  integer, intent(in) :: lundx, lunot
  integer maxdx, idxv, nxstr, ldxa, ldxb, ldxd, ld30, my_lundx, my_lunot, ldx, lot, il, im, lda, ldb, ldd, l30, nseq, &
    mbit, mbyt, mby4, mbya, mbyb, mbyd, i, j, jj, idn, lend, len0, len1, len2, l3, l4, l5, iupb, iupm

  common /dxtab/ maxdx, idxv, nxstr(10), ldxa(10), ldxb(10), ldxd(10), ld30(10), dxstr(10)

  character*56 dxstr
  character*6 adn30

  logical msgfull

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lundx,my_lundx,1)
    call x84(lunot,my_lunot,1)
    call wrdxtb(my_lundx,my_lunot)

    im8b=.true.
    return
  endif

  ! Check file statuses

  call status(lunot,lot,il,im)
  if(il.eq.0) call bort('BUFRLIB: WRDXTB - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il.lt.0) call bort('BUFRLIB: WRDXTB - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')

  call status(lundx,ldx,il,im)
  if(il.eq.0) call bort('BUFRLIB: WRDXTB - DX TABLE FILE IS CLOSED, IT MUST BE OPEN')

  ! If files are different, copy internal table information from lundx to lunot

  if(lundx.ne.lunot) call cpbfdx(ldx,lot)

  ! Generate and write out BUFR dictionary messages to lunot

  call dxmini(mgwa,mbyt,mby4,mbya,mbyb,mbyd)

  lda = ldxa(idxv+1)
  ldb = ldxb(idxv+1)
  ldd = ldxd(idxv+1)
  l30 = ld30(idxv+1)

  ! Table A information

  do i=1,ntba(lot)
    if(msgfull(mbyt,lda,maxdx).or.(iupb(mgwa,mbya,8).eq.255)) then
      call msgwrt(lunot,mgwa,mbyt)
      call dxmini(mgwa,mbyt,mby4,mbya,mbyb,mbyd)
    endif
    mbit = 8*(mby4-1)
    call pkb(iupb(mgwa,mby4,24)+lda,24,mgwa,mbit)
    mbit = 8*(mbya-1)
    call pkb(iupb(mgwa,mbya, 8)+  1, 8,mgwa,mbit)
    mbit = 8*(mbyb-1)
    call pkc(taba(i,lot),lda,mgwa,mbit)
    call pkb(          0,  8,mgwa,mbit)
    call pkb(          0,  8,mgwa,mbit)
    mbyt = mbyt+lda
    mbyb = mbyb+lda
    mbyd = mbyd+lda
  enddo

  ! Table B information

  do i=1,ntbb(lot)
    if(msgfull(mbyt,ldb,maxdx).or.(iupb(mgwa,mbyb,8).eq.255)) then
      call msgwrt(lunot,mgwa,mbyt)
      call dxmini(mgwa,mbyt,mby4,mbya,mbyb,mbyd)
    endif
    mbit = 8*(mby4-1)
    call pkb(iupb(mgwa,mby4,24)+ldb,24,mgwa,mbit)
    mbit = 8*(mbyb-1)
    call pkb(iupb(mgwa,mbyb, 8)+  1, 8,mgwa,mbit)
    mbit = 8*(mbyd-1)
    call pkc(tabb(i,lot),ldb,mgwa,mbit)
    call pkb(          0,  8,mgwa,mbit)
    mbyt = mbyt+ldb
    mbyd = mbyd+ldb
  enddo

  ! Table D information

  do i=1,ntbd(lot)
    nseq = iupm(tabd(i,lot)(ldd+1:ldd+1),8)
    lend = ldd+1 + l30*nseq
    if(msgfull(mbyt,lend,maxdx).or.(iupb(mgwa,mbyd,8).eq.255)) then
      call msgwrt(lunot,mgwa,mbyt)
      call dxmini(mgwa,mbyt,mby4,mbya,mbyb,mbyd)
    endif
    mbit = 8*(mby4-1)
    call pkb(iupb(mgwa,mby4,24)+lend,24,mgwa,mbit)
    mbit = 8*(mbyd-1)
    call pkb(iupb(mgwa,mbyd, 8)+   1, 8,mgwa,mbit)
    mbit = 8*(mbyt-4)
    call pkc(tabd(i,lot),ldd,mgwa,mbit)
    call pkb(       nseq,  8,mgwa,mbit)
    do j=1,nseq
      jj  = ldd+2 + (j-1)*2
      idn = iupm(tabd(i,lot)(jj:jj),16)
      call pkc(adn30(idn,l30),l30,mgwa,mbit)
    enddo
    mbyt = mbyt+lend
  enddo

  ! Write the unwritten (leftover) message.

  call msgwrt(lunot,mgwa,mbyt)

  ! Write out one additional (dummy) DX message containing zero subsets.  This will serve as a delimiter for this set of
  ! table messages within output unit lunot, just in case the next thing written to lunot ends up being another set of
  ! table messages.

  call dxmini(mgwa,mbyt,mby4,mbya,mbyb,mbyd)
  call getlens(mgwa,2,len0,len1,len2,l3,l4,l5)
  mbit = (len0+len1+len2+4)*8
  call pkb(0,16,mgwa,mbit)
  call msgwrt(lunot,mgwa,mbyt)

  return
end subroutine wrdxtb

!> Copy a DX BUFR tables message into the internal memory arrays in module @ref moda_tababd.
!>
!> @param lun - File ID
!> @param mesg - DX BUFR tables message
!>
!> @author J. Ator @date 2009-03-23
subroutine stbfdx(lun,mesg)

  use modv_vars, only: maxcd

  use moda_tababd

  implicit none

  integer, intent(in) :: lun, mesg(*)
  integer maxdx, idxv, nxstr, ldxa, ldxb, ldxd, ld30, ldxbd(10), ldxbe(10), ja, jb, idxs, i3, i, j, n, nd, ndd, idn, &
    jbit, len0, len1, len2, len3, l4, l5, lda, ldb, ldd, ldbd, ldbe, l30, ia, la, ib, lb, id, ld, iret, &
    ifxy, iupb, iupbs01, igetntbi, idn30

  character*128 bort_str
  character*128 tabb1, tabb2
  character*56 dxstr
  character*55 cseq
  character*50 dxcmp
  character*24 unit
  character*8 nemo
  character*6 numb, cidn

  common /dxtab/ maxdx, idxv, nxstr(10), ldxa(10), ldxb(10), ldxd(10), ld30(10), dxstr(10)

  data ldxbd /38, 70, 8*0/
  data ldxbe /42, 42, 8*0/

  ! Statement functions
  ja(i) = ia+1+lda*(i-1)
  jb(i) = ib+1+ldb*(i-1)

  ! Get some preliminary information from the message

  idxs = iupbs01(mesg,'MSBT')+1
  if(idxs.gt.idxv+1) idxs = iupbs01(mesg,'MTVL')+1
  if(ldxa(idxs).eq.0 .OR. ldxb(idxs).eq.0 .OR. ldxd(idxs).eq.0) call bort('BUFRLIB: STBFDX - UNEXPECTED DICTIONARY '// &
    'MESSAGE SUBTYPE OR LOCAL VERSION NUMBER (E.G., L.V.N. HIGHER THAN KNOWN)')

  call getlens(mesg,3,len0,len1,len2,len3,l4,l5)
  i3 = len0+len1+len2
  dxcmp = ' '
  jbit = 8*(i3+7)
  call upc(dxcmp,nxstr(idxs),mesg,jbit,.false.)
  if(dxcmp.ne.dxstr(idxs)) call bort('BUFRLIB: STBFDX - UNEXPECTED DICTIONARY MESSAGE CONTENTS')

  ! Section 4 - read definitions for Tables A, B and D

  lda  = ldxa (idxs)
  ldb  = ldxb (idxs)
  ldd  = ldxd (idxs)
  ldbd = ldxbd(idxs)
  ldbe = ldxbe(idxs)
  l30  = ld30 (idxs)

  ia = i3+len3+5
  la = iupb(mesg,ia,8)
  ib = ja(la+1)
  lb = iupb(mesg,ib,8)
  id = jb(lb+1)
  ld = iupb(mesg,id,8)

  ! Table A

  do i=1,la
    n = igetntbi(lun,'A')
    jbit = 8*(ja(i)-1)
    call upc(taba(n,lun),lda,mesg,jbit,.true.)
    numb = '   '//taba(n,lun)(1:3)
    nemo = taba(n,lun)(4:11)
    cseq = taba(n,lun)(13:67)
    call stntbia(n,lun,numb,nemo,cseq)
  enddo

  ! Table B

  do i=1,lb
    n = igetntbi(lun,'B')
    jbit = 8*(jb(i)-1)
    call upc(tabb1,ldbd,mesg,jbit,.true.)
    jbit = 8*(jb(i)+ldbd-1)
    call upc(tabb2,ldbe,mesg,jbit,.true.)
    tabb(n,lun) = tabb1(1:ldxbd(idxv+1))//tabb2(1:ldxbe(idxv+1))
    numb = tabb(n,lun)(1:6)
    nemo = tabb(n,lun)(7:14)
    call nenubd(nemo,numb,lun)
    idnb(n,lun) = ifxy(numb)
    unit = tabb(n,lun)(71:94)
    call capit(unit)
    tabb(n,lun)(71:94) = unit
    ntbb(lun) = n
  enddo

  ! Table D

  do i=1,ld
    n = igetntbi(lun,'D')
    jbit = 8*id
    call upc(tabd(n,lun),ldd,mesg,jbit,.true.)
    numb = tabd(n,lun)(1:6)
    nemo = tabd(n,lun)(7:14)
    call nenubd(nemo,numb,lun)
    idnd(n,lun) = ifxy(numb)
    nd = iupb(mesg,id+ldd+1,8)
    if(nd.gt.maxcd) then
      write(bort_str,'("BUFRLIB: STBFDX - NUMBER OF DESCRIPTORS IN '// &
        'TABLE D ENTRY ",A," IN BUFR TABLE (",I4,") EXCEEDS THE LIMIT (",I4,")")') nemo,nd,maxcd
      call bort(bort_str)
    endif
    do j=1,nd
      ndd = id+ldd+2 + (j-1)*l30
      jbit = 8*(ndd-1)
      call upc(cidn,l30,mesg,jbit,.true.)
      idn = idn30(cidn,l30)
      call pktdd(n,lun,idn,iret)
      if(iret.lt.0) call bort('BUFRLIB: STBFDX - BAD RETURN FROM BUFRLIB ROUTINE PKTDD, SEE PREVIOUS WARNING MESSAGE')
    enddo
    id = id+ldd+1 + nd*l30
    if(iupb(mesg,id+1,8).eq.0) id = id+1
    ntbd(lun) = n
  enddo

  return
end subroutine stbfdx

!> Check whether a BUFR message contains DX BUFR tables information that was generated by the NCEPLIBS-bufr software.
!>
!> @param mesg - BUFR message
!>
!> @returns idxmsg - Flag indicating whether mesg contains DX BUFR tables information:
!> - 0 = No
!> - 1 = Yes
!>
!> @author J. Ator @date 2009-03-23
integer function idxmsg( mesg ) result( iret )

  implicit none

  integer, intent(in) :: mesg(*)
  integer iupbs01

  ! Note that the following test relies upon logic within subroutine dxmini() which zeroes out the Section 1 date of
  ! all DX dictionary messages.

  if ( (iupbs01(mesg,'MTYP').eq.11) .and. &
         (iupbs01(mesg,'MNTH').eq.0) .and. (iupbs01(mesg,'DAYS').eq.0) .and. (iupbs01(mesg,'HOUR').eq.0) ) then
    iret = 1
  else
    iret = 0
  end if

  return
end function idxmsg

!> Get the next available index for storing an entry within a specified internal DX BUFR table.
!>
!> @param lun - File ID
!> @param ctb - Type of internal DX BUFR table for which to return the next available index:
!>    - 'A' = Table A
!>    - 'B' = Table B
!>    - 'D' = Table D
!> @returns igetntbi - integer: Next available index for storing an entry within ctb
!>
!> @author J. Ator @date 2009-03-23
integer function igetntbi ( lun, ctb ) result(iret)

  use moda_tababd

  implicit none

  integer, intent(in) :: lun
  integer imax

  character, intent(in) :: ctb
  character*128 bort_str

  if ( ctb .eq. 'A' ) then
    iret = ntba(lun) + 1
    imax = ntba(0)
  else if ( ctb .eq. 'B' ) then
    iret = ntbb(lun) + 1
    imax = ntbb(0)
  else  ! ctb .eq. 'D'
    iret = ntbd(lun) + 1
    imax = ntbd(0)
  endif
  if ( iret .gt. imax ) then
    write(bort_str,'("BUFRLIB: IGETNTBI - NUMBER OF INTERNAL TABLE",A1," ENTRIES EXCEEDS THE LIMIT (",I4,")")') ctb, imax
    call bort(bort_str)
  endif

  return
end function igetntbi
