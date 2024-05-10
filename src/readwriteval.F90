!> @file
!> @brief Read or write data values within a BUFR data subset.
!>
!> @authors J. Woollen, J. Ator @date 1994-01-06

!> Write a data value corresponding to
!> a specific occurrence of a mnemonic within a data subset, based on
!> its position relative to a different mnemonic within the subset.
!>
!> The subroutine first searches for a specific occurrence of a pivot
!> mnemonic, counting from the beginning of the subset.  From there,
!> it then searches in either a forward or backward direction for a
!> specific occurrence of a nearby mnemonic, and if found
!> stores the specified data value in the corresponding location
!> within the subset.
!>
!> Before calling this subroutine, a BUFR message should already be
!> opened and initialized for output via a previous call to one of the
!> NCEPLIBS-bufr [message-writing subroutines](@ref hierarchy).
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param tagpv - Pivot mnemonic; the subroutine will first search for the (ntagpv)th occurrence of this mnemonic, counting
!> from the beginning of the overall subset definition
!> @param ntagpv - Ordinal occurrence of tagpv to search for, counting from the beginning of the overall subset definition
!> @param tagnb - Nearby mnemonic; assuming tagpv is successfully found, the subroutine will then search nearby for the
!> (ntagnb)th occurrence of tagnb and store r8val as the corresponding value
!> @param ntagnb - Ordinal occurrence of tagnb to search for, counting from the location of tagpv within the overall subset
!> definition
!>  - If ntagnb is positive, the subroutine will search in a forward direction from the location of tagpv
!>  - If ntagnb is negative, the subroutine will search in a backwards direction from the location of tagpv
!> @param r8val - Value to be stored corresponding to (ntagnb)th occurrence of tagnb within the subset
!> @param iret - return code:
!>  - 0 = r8val was successfully stored
!>  - -1 = the (ntagnb)th occurence of mnemonic tagnb could not be found, or some other error occurred
!>
!> @author J. Ator @date 2016-07-29
recursive subroutine setvalnb ( lunit, tagpv, ntagpv, tagnb, ntagnb, r8val, iret )

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lunit, ntagpv, ntagnb
  integer, intent(out) :: iret
  integer my_lunit, my_ntagpv, my_ntagnb, lun, il, im, npv, nnb, ierft

  character*(*), intent(in) :: tagpv, tagnb

  real*8, intent(in) ::  r8val

  ! Check for I8 integers.
  if(im8b) then
    im8b=.false.
    call x84 ( lunit, my_lunit, 1 )
    call x84 ( ntagpv, my_ntagpv, 1 )
    call x84 ( ntagnb, my_ntagnb, 1 )
    call setvalnb ( my_lunit, tagpv, my_ntagpv, tagnb, my_ntagnb, r8val, iret )
    call x48 ( iret, iret, 1 )
    im8b=.true.
    return
  endif

  iret = -1

  ! Get lun from lunit.
  call status (lunit, lun, il, im )
  if ( il .le. 0 ) return
  if ( inode(lun) .ne. inv(1,lun) ) return

  ! Starting from the beginning of the subset, locate the (ntagpv)th occurrence of tagpv.
  call fstag( lun, tagpv, ntagpv, 1, npv, ierft )
  if ( ierft .ne. 0 ) return

  ! Now, starting from the (ntagpv)th occurrence of tagpv, search forward or backward for the (ntagnb)th occurrence of tagnb.
  call fstag( lun, tagnb, ntagnb, npv, nnb, ierft )
  if ( ierft .ne. 0 ) return

  iret = 0
  val(nnb,lun) = r8val

  return
end subroutine setvalnb

!> Read a data value corresponding to
!> a specific occurrence of a mnemonic within a data subset, based on
!> its position relative to a different mnemonic within the subset.
!>
!> The function first searches for a specific occurrence of a pivot
!> mnemonic, counting from the beginning of the subset.  From there,
!> it then searches in either a forward or backward direction for a
!> specific occurrence of a nearby mnemonic, and if found
!> returns the data value from the corresponding location
!> within the subset.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param tagpv - Pivot mnemonic; the subroutine will first search for the (ntagpv)th occurrence of this mnemonic, counting
!> from the beginning of the overall subset definition
!> @param ntagpv - Ordinal occurrence of tagpv to search for, counting from the beginning of the overall subset definition
!> @param tagnb - Nearby mnemonic; assuming tagpv is successfully found, the subroutine will then search nearby for the
!> (ntagnb)th occurrence of tagnb and return the corresponding value
!> @param ntagnb - Ordinal occurrence of tagnb to search for, counting from the location of tagpv within the overall subset
!> definition
!>  - If ntagnb is positive, the subroutine will search in a forward direction from the location of tagpv
!>  - If ntagnb is negative, the subroutine will search in a backwards direction from the location of tagpv
!> @returns getvalnb - Value corresponding to (ntagnb)th occurrence of tagnb
!>  - If for any reason this value cannot be located, then the current placeholder value for "missing" data will be returned
!>  instead
!>
!> The current placeholder value for "missing" data can be determined
!> via a separate call to function getbmiss().
!>
!> Before calling this function, a BUFR data subset should already be
!> open for reading via a previous call to one of the NCEPLIBS-bufr
!> [subset-reading subroutines](@ref hierarchy).
!>
!> @author J. Ator @date 2012-09-12
recursive real*8 function getvalnb ( lunit, tagpv, ntagpv, tagnb, ntagnb ) result ( r8val )

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lunit, ntagpv, ntagnb
  integer my_lunit, my_ntagpv, my_ntagnb, lun, il, im, npv, nnb, ierft

  character*(*), intent(in) :: tagpv, tagnb

  ! Check for I8 integers.
  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(ntagpv,my_ntagpv,1)
    call x84(ntagnb,my_ntagnb,1)
    r8val=getvalnb(my_lunit,tagpv,my_ntagpv,tagnb,my_ntagnb)
    im8b=.true.
    return
  endif

  r8val = bmiss

  ! Get lun from lunit.
  call status (lunit, lun, il, im )
  if ( il .ge. 0 ) return
  if ( inode(lun) .ne. inv(1,lun) ) return

  ! Starting from the beginning of the subset, locate the (ntagpv)th occurrence of tagpv.
  call fstag( lun, tagpv, ntagpv, 1, npv, ierft )
  if ( ierft .ne. 0 ) return

  ! Now, starting from the (ntagpv)th occurrence of tagpv, search forward or backward for the (ntagnb)th occurrence of tagnb.
  call fstag( lun, tagnb, ntagnb, npv, nnb, ierft )
  if ( ierft .ne. 0 ) return

  r8val = val(nnb,lun)

  return
end function getvalnb

!> Write a long character string (greater than 8 bytes) to a data subset.
!>
!> The data subset should have already been written into a BUFR message before
!> calling this subroutine to write a long character string into the subset.
!>
!> If there is more than one occurrence of str within the data subset
!> definition, then each occurrence can be written via a separate call
!> to this subroutine, and by appending the ordinal number of the
!> occurrence to STR in each case.  For example, if there are 5
!> occurrences of mnemonic LSTID within a given data subset definition,
!> then 5 separate calls should be made to this subroutine, once each
!> with STR set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and
!> 'LSTID#5'.  However, the first notation is superfluous, because
!> omitting the ordinal number always defaults to the first occurrence
!> of a particular string, so a user could just specify 'LSTID'
!> instead of 'LSTID#1'.
!>
!> Character strings which are 8 bytes or less in length can be
!> written by converting the string into a real*8 value within the
!> application program, and then using the real*8 usr array within a
!> call to one of the NCEPLIBS-bufr
!> [values-writing subroutines](@ref hierarchy)
!> prior to calling one of the
!> [subset-writing subroutines](@ref hierarchy)
!> for the data subset.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param chr - Value corresponding to str
!> @param str - Table B mnemonic of long character string to be written, possibly supplemented with an ordinal
!> occurrence notation
!>
!> @author J. Woollen @author J. Ator @date 2003-11-04
recursive subroutine writlc(lunit,chr,str)

  use modv_vars, only: im8b, mxlcc

  use moda_usrint
  use moda_msgcwd
  use moda_bitbuf
  use moda_tables
  use moda_comprs

  implicit none

  integer, intent(in) :: lunit
  integer my_lunit, maxtg, iprt, lun, il, im, ntg, nnod, kon, ii, n, node, ioid, ival, mbit, nbit, nbmp, nchr, nbyt, nsubs, &
    itagct, len0, len1, len2, len3, l4, l5, mbyte, iupbs3

  character*(*), intent(in) :: chr, str
  character*128 bort_str, errstr
  character*10 ctag
  character*14 tgs(10)

  real roid

  common /quiet/ iprt

  data maxtg /10/

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call writlc(my_lunit,chr,str)
    im8b=.true.
    return
  endiF

  ! Check the file status.
  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il.lt.0) call bort('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im.eq.0) call bort('BUFRLIB: WRITLC - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')

  ! Check for tags (mnemonics) in input string (there can only be one)
  call parstr(str,tgs,maxtg,ntg,' ',.true.)
  if(ntg.gt.1) then
    write(bort_str,'("BUFRLIB: WRITLC - THERE CANNOT BE MORE THAN '// &
      ' ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE",I4,")")') str,ntg
    call bort(bort_str)
  endif

  ! Check if a specific occurrence of the input string was requested; if not, then the default is to write the first occurrence
  call parutg(lun,1,tgs(1),nnod,kon,roid)
  if(kon.eq.6) then
    ioid=nint(roid)
    if(ioid.le.0) ioid = 1
    ctag = ' '
    ii = 1
    do while((ii.le.10).and.(tgs(1)(ii:ii).ne.'#'))
      ctag(ii:ii)=tgs(1)(ii:ii)
      ii = ii + 1
    enddo
  else
    ioid = 1
    ctag = tgs(1)(1:10)
  endif

  if(iupbs3(mbay(1,lun),'ICMP').gt.0) then
    ! The message is compressed
    n = 1
    itagct = 0
    call usrtpl(lun,n,n)
    do while (n+1.le.nval(lun))
      n = n+1
      node = inv(n,lun)
      if(itp(node).eq.1) then
        nbmp=int(matx(n,ncol))
        call usrtpl(lun,n,nbmp)
      elseif(ctag.eq.tag(node)) then
        itagct = itagct + 1
        if(itagct.eq.ioid) then
          if(itp(node).ne.3) then
            write(bort_str,'("BUFRLIB: WRITLC - MNEMONIC ",A," DOES NOT REPRESENT A CHARACTER ELEMENT (TYP=",A,")")') &
              ctag,typ(node)
            call bort(bort_str)
          endif
          catx(n,ncol)=' '
          ! The following statement enforces a limit of mxlcc characters per long character string when writing
          ! compressed messages. This limit keeps the array catx to a reasonable dimensioned size.
          nchr=min(mxlcc,ibt(node)/8)
          catx(n,ncol)=chr(1:nchr)
          call usrtpl(lun,1,1)
          return
        endif
      endif
    enddo
  else
    ! The message is not compressed. Locate the beginning of the data (Section 4) in the message.
    call getlens(mbay(1,lun),3,len0,len1,len2,len3,l4,l5)
    mbyte = len0 + len1 + len2 + len3 + 4
    nsubs = 1
    ! Find the most recently written subset in the message.
    do while(nsubs.lt.nsub(lun))
      ibit = mbyte*8
      call upb(nbyt,16,mbay(1,lun),ibit)
      mbyte = mbyte + nbyt
      nsubs = nsubs + 1
    enddo
    if(nsubs.ne.nsub(lun)) then
      if(iprt.ge.0) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: WRITLC - COULDN''T WRITE VALUE FOR ' // ctag // &
          ' INTO SUBSET, BECAUSE NO SUBSET WAS OPEN FOR WRITING'
        call errwrt(errstr)
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        call errwrt(' ')
      endif
      return
    endif
    ! Locate and write the long character string within this subset.
    itagct = 0
    mbit = mbyte*8 + 16
    nbit = 0
    n = 1
    call usrtpl(lun,n,n)
    do while (n+1.le.nval(lun))
      n = n+1
      node = inv(n,lun)
      mbit = mbit+nbit
      nbit = ibt(node)
      if(itp(node).eq.1) then
        call upbb(ival,nbit,mbit,mbay(1,lun))
        call usrtpl(lun,n,ival)
      elseif(ctag.eq.tag(node)) then
        itagct = itagct + 1
        if(itagct.eq.ioid) then
          if(itp(node).ne.3) then
            write(bort_str,'("BUFRLIB: WRITLC - MNEMONIC ",A," DOES NOT REPRESENT A CHARACTER ELEMENT (TYP=",A,")")') &
              ctag,typ(node)
            call bort(bort_str)
          endif
          nchr = nbit/8
          ibit = mbit
          do ii=1,nchr
            call pkc(' ',1,mbay(1,lun),ibit)
          enddo
          call pkc(chr,nchr,mbay(1,lun),mbit)
          call usrtpl(lun,1,1)
          return
        endif
      endif
    enddo
  endif

  ! If we made it here, then we couldn't find the requested string.
  if(iprt.ge.0) then
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    errstr = 'BUFRLIB: WRITLC - COULDN''T WRITE VALUE FOR ' // ctag // ' INTO SUBSET, BECAUSE IT WASN''T FOUND IN THE ' // &
      'SUBSET DEFINITION'
    call errwrt(errstr)
    errstr = '(' // ctag // ' MAY NOT BE IN THE BUFR TABLE(?))'
    call errwrt(errstr)
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    call errwrt(' ')
  endif

  return
end subroutine writlc

!> Read a long character string (greater than 8 bytes) from a data subset.
!>
!> The data subset should have already been read into internal arrays
!> via a previous call to one of the
!> [subset-reading subroutines](@ref hierarchy).
!>
!> If there is more than one occurrence of str within the data subset
!> definition, then each occurrence can be retrieved via a separate call
!> to this subroutine, and by appending the ordinal number of the
!> occurrence to str in each case.
!>
!> For example, if there are 5
!> occurrences of mnemonic LSTID within a given data subset definition,
!> then 5 separate calls should be made to this subroutine, once each
!> with STR set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and
!> 'LSTID#5'.
!>
!> Omitting the ordinal number always defaults to the first occurrence
!> of a particular string, so a user could just specify 'LSTID'
!> instead of 'LSTID#1'.
!>
!> @remarks
!> - Character strings which are 8 bytes or less in length can be read
!> using the real*8 usr array within a call to one of the NCEPLIBS-bufr
!> [values-reading subroutines](@ref hierarchy) and then converting the
!> corresponding real*8 value to character format within the
!> application program.
!> - If str is not found within the data subset definition, then chr is
!> returned with all bits set to 1, which is the standard WMO BUFR value
!> for "missing" data. Any chr value returned by this subroutine can be
!> checked for equivalence to this "missing" value via a call to
!> function icbfms().
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param chr - Value corresponding to STR
!> @param str - Table B mnemonic of long character string to be retrieved, possibly supplemented with
!> an ordinal occurrence notation
!>
!> @authors J. Woollen J. Ator @date 2003-11-04
recursive subroutine readlc(lunit,chr,str)

  use modv_vars, only: im8b

  use moda_usrint
  use moda_usrbit
  use moda_unptyp
  use moda_bitbuf
  use moda_tables
  use moda_rlccmn

  implicit none

  integer, intent(in) :: lunit
  integer my_lunit, maxtg, iprt, lchr, lun, il, im, ntg, nnod, kon, ii, n, nod, ioid, itagct, nchr, kbit

  character*(*), intent(in) :: str
  character*(*), intent(out) :: chr

  character*128 bort_str, errstr
  character*10 ctag
  character*14 tgs(10)

  real roid

  common /quiet/ iprt

  data maxtg /10/

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call readlc(my_lunit,chr,str)
    im8b=.true.
    return
  endif

  chr = ' '
  lchr=len(chr)

  ! Check the file status
  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: READLC - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: READLC - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) call bort('BUFRLIB: READLC - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')

  ! Check for tags (mnemonics) in input string (there can only be one)
  call parstr(str,tgs,maxtg,ntg,' ',.true.)
  if(ntg.gt.1) then
    write(bort_str,'("BUFRLIB: READLC - THERE CANNOT BE MORE THAN '// &
       'ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE ",I3,")")') str,ntg
    call bort(bort_str)
  endif

  ! Check if a specific occurrence of the input string was requested; if not, then the default is to return the
  ! first occurrence.
  call parutg(lun,0,tgs(1),nnod,kon,roid)
  if(kon.eq.6) then
    ioid=nint(roid)
    if(ioid.le.0) ioid = 1
    ctag = ' '
    ii = 1
    do while((ii.le.10).and.(tgs(1)(ii:ii).ne.'#'))
      ctag(ii:ii)=tgs(1)(ii:ii)
      ii = ii + 1
    enddo
  else
    ioid = 1
    ctag = tgs(1)(1:10)
  endif

  ! Locate and decode the long character string
  if(msgunp(lun).eq.0.or.msgunp(lun).eq.1) then
    ! The message is not compressed
    itagct = 0
    do n=1,nval(lun)
      nod = inv(n,lun)
      if(ctag.eq.tag(nod)) then
        itagct = itagct + 1
        if(itagct.eq.ioid) then
          if(itp(nod).ne.3) then
            write(bort_str,'("BUFRLIB: READLC - MNEMONIC ",A," DOES NOT '// &
              'REPRESENT A CHARACTER ELEMENT (ITP=",I2,")")') tgs(1),itp(nod)
            call bort(bort_str)
          endif
          nchr = nbit(n)/8
          if(nchr.gt.lchr) Then
            write(bort_str,'("BUFRLIB: READLC - MNEMONIC ",A," IS A '// &
            'CHARACTER STRING OF LENGTH",I4," BUT SPACE WAS PROVIDED FOR ONLY",I4, " CHARACTERS")') tgs(1),nchr,lchr
            call bort(bort_str)
          endif
          kbit = mbit(n)
          call upc(chr,nchr,mbay(1,lun),kbit,.true.)
          return
        endif
      endif
    enddo
  else
    ! The message is compressed
    if(nrst.gt.0) then
      itagct = 0
      do ii=1,nrst
        if(ctag.eq.crtag(ii)) then
          itagct = itagct + 1
          if(itagct.eq.ioid) then
            nchr = irnch(ii)
            if(nchr.gt.lchr) then
              write(bort_str,'("BUFRLIB: READLC - MNEMONIC ",A," IS A '// &
                'CHARACTER STRING OF LENGTH",I4," BUT SPACE WAS PROVIDED FOR ONLY",I4, " CHARACTERS")') tgs(1),nchr,lchr
              call bort(bort_str)
            endif
            kbit = irbit(ii)
            call upc(chr,nchr,mbay(1,lun),kbit,.true.)
            return
          endif
        endif
      enddo
    endif
  endif

  ! If we made it here, then we couldn't find the requested string.
  if(iprt.ge.0) then
    call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
    errstr = 'BUFRLIB: READLC - MNEMONIC ' // tgs(1) // &
      ' NOT LOCATED IN REPORT SUBSET - RETURN WITH MISSING STRING FOR CHARACTER DATA ELEMENT'
    call errwrt(errstr)
    call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
    call errwrt(' ')
  endif
  do ii=1,lchr
    call ipkm(chr(ii:ii),1,255)
  enddo

  return
end subroutine readlc

!> Read or write one or more data values from or to
!> the BUFR data subset that is currently open within the NCEPLIBS-bufr
!> internal arrays.
!>
!> The direction of the data transfer is determined by the context of abs(lunin):
!> - If abs(lunin) points to a file that was previously opened for
!>   input using subroutine openbf(), then data values are read from
!>   the current data subset.
!> - If abs(lunin) points to a file that was previously opened for
!>   output using subroutine openbf(), then data values are written to
!>   the current data subset.
!>
!> This subroutine is specifically designed for use with Table B
!> mnemonics which are part of a delayed-replication sequence, or for
!> which there is no replication at all. See also subroutines ufbrep(),
!> ufbseq() and ufbstp(), which can also be used to read/write one or
!> more data values from/to a data subset but are designed for
!> different use cases. A more detailed discussion of
!> these different use cases, including examples, is available in
!> [DX BUFR Tables](@ref ufbsubs).
!>
!> It is the user's responsibility to ensure that usr is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from or written to the data subset. Note also
!> that usr is an array of real*8 values; therefore, any data that are
!> to be written out as character (i.e. CCITT IA5) values in
!> BUFR must be converted from character into real*8 format within the
!> application program before calling this subroutine. Conversely,
!> when this subroutine is being used to read character values from a
!> data subset, the value that is returned will be in real*8 format
!> and must be converted back into character format by the application
!> program before it can be used as such. Alternatively, there are
!> different subroutines such as readlc() and writlc() which can be
!> used to read/write character data directly from/to a data subset
!> without the need to convert from/to real*8 format as an intermediate
!> step.
!>
!> Numeric (i.e. non-character) data values within usr are always in
!> the exact units specified for the corresponding mnemonic within the
!> relevant DX or master BUFR table, without any scale or reference
!> values applied. Specifically, this means that, when writing
!> data values into an output subset, the user only needs to store each
!> respective value into usr using the units specified within the table,
!> and the NCEPLIBS-bufr software will take care of any necessary scaling or
!> referencing of the value before it is actually encoded into BUFR.
!> Conversely, when reading data values from an input subset, the
!> values returned in usr are already de-scaled and de-referenced and,
!> thus, are already in the exact units that were defined for the
!> corresponding mnemonics within the table.
!>
!> "Missing" values in usr are always denoted by a unique
!> placeholder value. This placeholder value is initially set
!> to a default value of 10E10_8, but it can be reset to
!> any substitute value of the user's choice via a separate
!> call to subroutine setbmiss(). In any case, and whenever this
!> subroutine is used to read data values from an input subset, any
!> returned value in usr can be easily checked for equivalence to the
!> current placeholder value via a call to function ibfms(), and a
!> positive result means that the value for the corresponding mnemonic
!> was encoded as "missing" in BUFR (i.e. all bits set to 1) within the
!> original data subset. Conversely, whenever this subroutine
!> is used to write data values to an output subset, the current
!> placeholder value can be obtained via a separate call to function
!> getbmiss(), and the resulting value can then be stored into the
!> usr array whereever the user desires a BUFR "missing" value (i.e.
!> all bits set to 1) to be encoded for the corresponding mnemonic
!> within the output subset.
!>
!> @remarks
!> - If lunin < 0, and if abs(lunin) points to a file that is open
!> for output (writing BUFR), then the subroutine will treat the file
!> pointed to by abs(lunin) as though it was open for input (reading
!> BUFR). This is a special capability for use by some applications
!> that need to read certain values back out from a BUFR file during
!> the same time that it is in the process of being written to.
!> - If abs(lunin) points to a file that is open for input (reading
!> BUFR), str may contain a Table D mnemonic that is replicated using
!> either 8-bit or 16-bit delayed replication (as noted using
!> replication indicators {} or (), respectively, within the
!> assocated DX BUFR table), and the corresponding location in usr
!> will contain the total number of replications of that mnemonic
!> within the data subset. Note that, when using this option, the
!> applicable replication indicators must be included in str
!> along with the mnemonic itself, as shown in an example in the
!> discussion of [DX BUFR Tables](@ref ufbsubs).
!>
!> @param lunin - Absolute value is Fortran logical unit number for BUFR file
!> @param usr - Data values
!>  - If abs(lunin) was opened for input, then usr is output from this subroutine and
!>    contains data values that were read from the current data subset
!>  - If abs(lunin) was opened for output, then usr is input to this subroutine and
!>    contains data values that are to be written to the current data subset
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr
!>  - If abs(lunin) was opened for input, then i2 must be set equal to the second dimension
!>    of usr as allocated within the calling program
!>  - If abs(lunin) was opened for output, then i2 must be set equal to the number of replications
!>    of str that are to be written to the data subset
!> @param iret - Number of replications of str that were read/written from/to the data subset
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of data values
!> that will be read/written from/to the data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table B mnemonics)
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbint(lunin,usr,i1,i2,iret,str)

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd

  implicit none

  character*(*), intent(in) :: str
  character*128 bort_str1, bort_str2, errstr

  real*8, intent(inout) :: usr(i1,i2)

  integer, intent(in) :: lunin, i1, i2
  integer, intent(out) :: iret
  integer iprt, nnod, ncon, nods, nodc, ivls, kons, ifirst1, ifirst2, my_lunin, my_i1, my_i2, lunit, lun, il, im, io, i, j

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)
  common /quiet/ iprt

  data ifirst1 /0/, ifirst2 /0/

  save ifirst1, ifirst2

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunin,my_lunin,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbint(my_lunin,usr,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  iret = 0

  ! Check the file status and inode
  lunit = abs(lunin)
  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBINT - BUFR FILE IS CLOSED, IT MUST BE OPEN')
  if(im.eq.0) call bort('BUFRLIB: UFBINT - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')
  if(inode(lun).ne.inv(1,lun)) call bort('BUFRLIB: UFBINT - LOCATION OF INTERNAL TABLE FOR BUFR FILE DOES NOT AGREE ' // &
    'WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  io = min(max(0,il),1)
  if(lunit.ne.lunin) io = 0

  if(i1.le.0) then
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBINT - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2.le.0) then
    if(iprt.eq.-1) ifirst1 = 1
    if(io.eq.0 .or. ifirst1.eq.0 .or. iprt.ge.1)  then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBINT - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt.eq.0 .and. io.eq.1) then
        errstr = 'Note: Only the first occurrence of this WARNING message is printed, there may be more.  To output ' // &
          'all such messages,'
        call errwrt(errstr)
        errstr = 'modify your application program to add "CALL OPENBF(0,''QUIET'',1)" prior to the first call to a ' // &
          'BUFRLIB routine.'
        call errwrt(errstr)
      endif
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
      ifirst1 = 1
    endif
    return
  endif

  ! Parse or recall the input string
  call string(str,lun,i1,io)

  ! Initialize usr array preceeding an input operation
  if(io.eq.0) then
    do j=1,i2
      do i=1,I1
        usr(i,j) = bmiss
      enddo
    enddo
  endif

  ! Call the mnemonic reader/writer
  call ufbrw(lun,usr,i1,i2,io,iret)

  ! If incomplete write try to initialize replication sequence or return
  if(io.eq.1 .and. iret.ne.i2 .and. iret.ge.0) then
    call trybump(lun,usr,i1,i2,io,iret)
    if(iret.ne.i2) then
      write(bort_str1,'("BUFRLIB: UFBINT - MNEMONIC STRING READ IN IS: ",A)') str
      write(bort_str2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
        'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,") - INCOMPLETE WRITE")')  iret,i2
      call bort2(bort_str1,bort_str2)
    endif
  elseif(iret.eq.-1) then
    iret = 0
  endif

  if(iret.eq.0) then
    if(io.eq.0) then
      if(iprt.ge.1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: UFBINT - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
        call errwrt(errstr)
        call errwrt(str)
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        call errwrt(' ')
      endif
    else
      if(iprt.eq.-1) ifirst2 = 1
      if(ifirst2.eq.0 .or. iprt.ge.1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: UFBINT - NO SPECIFIED VALUES WRITTEN OUT, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
        call errwrt(errstr)
        call errwrt(str)
        call errwrt('MAY NOT BE IN THE BUFR TABLE(?)')
        if(iprt.eq.0) then
          errstr = 'Note: Only the first occurrence of this WARNING message is printed, there may be more.  To output ' // &
            'all such messages,'
          call errwrt(errstr)
          errstr = 'modify your application program to add "CALL OPENBF(0,''QUIET'',1)" prior to the first call ' // &
            'to a BUFRLIB routine.'
          call errwrt(errstr)
        endif
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        call errwrt(' ')
        ifirst2 = 1
      endif
    endif
  endif

  return
end subroutine ufbint

!> Read or write one or more data values from or to
!> the BUFR data subset that is currently open within the NCEPLIBS-bufr
!> internal arrays.
!>
!> The direction of the data transfer is determined by the context of abs(lunin):
!> - If abs(lunin) points to a file that was previously opened for
!>   input using subroutine openbf(), then data values are read from
!>   the current data subset.
!> - If abs(lunin) points to a file that was previously opened for
!>   output using subroutine openbf(), then data values are written to
!>   the current data subset.
!>
!> This subroutine is specifically designed for use with Table B
!> mnemonics which are part of a fixed (i.e. non-delayed) replication
!> sequence, or for mnemonics which are replicated by being directly
!> listed more than once within an overall subset definition.
!> See also subroutines ufbint(),
!> ufbseq() and ufbstp(), which can also be used to read/write one or
!> more data values from/to a data subset but are designed for
!> different use cases. A more detailed discussion of
!> these different use cases, including examples, is available in
!> [DX BUFR Tables](@ref ufbsubs).
!>
!> It is the user's responsibility to ensure that usr is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from or written to the data subset. Note also
!> that usr is an array of real*8 values; therefore, any data that are
!> to be written out as character (i.e. CCITT IA5) values in
!> BUFR must be converted from character into real*8 format within the
!> application program before calling this subroutine. Conversely,
!> when this subroutine is being used to read character values from a
!> data subset, the value that is returned will be in real*8 format
!> and must be converted back into character format by the application
!> program before it can be used as such. Alternatively, there are
!> different subroutines such as readlc() and writlc() which can be
!> used to read/write character data directly from/to a data subset
!> without the need to convert from/to real*8 format as an intermediate
!> step.
!>
!> Numeric (i.e. non-character) data values within usr are always in
!> the exact units specified for the corresponding mnemonic within the
!> relevant DX or master BUFR table, without any scale or reference
!> values applied. Specifically, this means that, when writing
!> data values into an output subset, the user only needs to store each
!> respective value into usr using the units specified within the table,
!> and the NCEPLIBS-bufr software will take care of any necessary scaling or
!> referencing of the value before it is actually encoded into BUFR.
!> Conversely, when reading data values from an input subset, the
!> values returned in usr are already de-scaled and de-referenced and,
!> thus, are already in the exact units that were defined for the
!> corresponding mnemonics within the table.
!>
!> "Missing" values in usr are always denoted by a unique
!> placeholder value. This placeholder value is initially set
!> to a default value of 10E10_8, but it can be reset to
!> any substitute value of the user's choice via a separate
!> call to subroutine setbmiss(). In any case, and whenever this
!> subroutine is used to read data values from an input subset, any
!> returned value in usr can be easily checked for equivalence to the
!> current placeholder value via a call to function ibfms(), and a
!> positive result means that the value for the corresponding mnemonic
!> was encoded as "missing" in BUFR (i.e. all bits set to 1) within the
!> original data subset. Conversely, whenever this subroutine
!> is used to write data values to an output subset, the current
!> placeholder value can be obtained via a separate call to function
!> getbmiss(), and the resulting value can then be stored into the
!> usr array whereever the user desires a BUFR "missing" value (i.e.
!> all bits set to 1) to be encoded for the corresponding mnemonic
!> within the output subset.
!>
!> @remarks
!> - If lunin < 0, and if abs(lunin) points to a file that is open
!> for output (writing BUFR), then the subroutine will treat the file
!> pointed to by abs(lunin) as though it was open for input (reading
!> BUFR). This is a special capability for use by some applications
!> that need to read certain values back out from a BUFR file during
!> the same time that it is in the process of being written to.
!>
!> @param lunin - Absolute value is Fortran logical unit number for BUFR file
!> @param usr - Data values
!>  - If abs(lunin) was opened for input, then usr is output from this subroutine and
!>    contains data values that were read from the current data subset
!>  - If abs(lunin) was opened for output, then usr is input to this subroutine and
!>    contains data values that are to be written to the current data subset
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr
!>  - If abs(lunin) was opened for input, then i2 must be set equal to the second dimension
!>    of usr as allocated within the calling program
!>  - If abs(lunin) was opened for output, then i2 must be set equal to the number of replications
!>    of str that are to be written to the data subset
!> @param iret - Number of replications of str that were read/written from/to the data subset
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of data values
!> that will be read/written from/to the data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table B mnemonics)
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbrep(lunin,usr,i1,i2,iret,str)

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd

  implicit none

  character*(*), intent(in) :: str
  character*128 bort_str1, bort_str2, errstr

  real*8, intent(inout) :: usr(i1,i2)

  integer, intent(in) :: lunin, i1, i2
  integer, intent(out) :: iret
  integer iprt, iac, ifirst1, my_lunin, my_i1, my_i2, lunit, lun, il, im, io, ia2, i, j

  common /quiet/ iprt
  common /acmode/ iac

  data ifirst1 /0/

  save ifirst1

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunin,my_lunin,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbrep(my_lunin,usr,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  iret = 0

  ! Check the file status and inode
  lunit = abs(lunin)
  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBREP - BUFR FILE IS CLOSED, IT MUST BE OPEN')
  if(im.eq.0) call bort('BUFRLIB: UFBREP - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')
  if(inode(lun).ne.inv(1,lun)) call bort('BUFRLIB: UFBREP - LOCATION OF INTERNAL TABLE FOR BUFR FILE DOES NOT AGREE ' // &
    'WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  io = min(max(0,il),1)
  if(lunit.ne.lunin) io = 0

  if(i1.le.0) then
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBREP - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2.le.0) then
    if(iprt.eq.-1) ifirst1 = 1
    if(io.eq.0 .or. ifirst1.eq.0 .or. iprt.ge.1)  then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBREP - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt.eq.0 .and. io.eq.1) then
        errstr = 'Note: Only the first occurrence of this WARNING message is printed, there may be more.  To output ' // &
          'all such messages,'
        call errwrt(errstr)
        errstr = 'modify your application program to add "CALL OPENBF(0,''QUIET'',1)" prior to the first call to a ' // &
          'BUFRLIB routine.'
        call errwrt(errstr)
      endif
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
      ifirst1 = 1
    endif
    return
  endif

  ! Initialize usr array preceeding an input operation
  if(io.eq.0) then
    do j=1,i2
      do i=1,I1
        usr(i,j) = bmiss
      enddo
    enddo
  endif

  ! Parse or recall the input string
  ia2 = iac
  iac = 1
  call string(str,lun,i1,io)

  ! Call the mnemonic reader/writer
  call ufbrp(lun,usr,i1,i2,io,iret)
  iac = ia2

  if(io.eq.1 .and. iret.lt.i2) then
    write(bort_str1,'("BUFRLIB: UFBREP - MNEMONIC STRING READ IN IS: ",A)') str
    write(bort_str2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
      'WRITTEN (",I3,") LESS THAN THE NUMBER REQUESTED (",I3,") - INCOMPLETE WRITE")')  iret,i2
    call bort2(bort_str1,bort_str2)
  endif

  if(iret.eq.0 .and. io.eq.0 .and. iprt.ge.1) then
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    errstr = 'BUFRLIB: UFBREP - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
    call errwrt(errstr)
    call errwrt(str)
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    call errwrt(' ')
  endif

  return
end subroutine ufbrep

!> Read or write one or more data values from or to
!> the BUFR data subset that is currently open within the NCEPLIBS-bufr
!> internal arrays.
!>
!> The direction of the data transfer is determined by the context of abs(lunin):
!> - If abs(lunin) points to a file that was previously opened for
!>   input using subroutine openbf(), then data values are read from
!>   the current data subset.
!> - If abs(lunin) points to a file that was previously opened for
!>   output using subroutine openbf(), then data values are written to
!>   the current data subset.
!>
!> This subroutine is specifically designed for use with Table B
!> mnemonics which are part of a fixed (i.e. non-delayed) replication
!> sequence, or for mnemonics which are replicated by being directly
!> listed more than once within an overall subset definition.
!> It is very similar to subroutine ufbrep(), but it differs in how it
!> processes the input mnemonic string str.  For more details, see
!> the discussion and example use case in [DX BUFR Tables](@ref ufbsubs).
!> See also subroutines ufbint() and ufbseq(), which can also be used
!> to read/write one or more data values from/to a data subset but are
!> also designed for different use cases as noted in
!> [DX BUFR Tables](@ref ufbsubs).
!>
!> It is the user's responsibility to ensure that usr is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from or written to the data subset. Note also
!> that usr is an array of real*8 values; therefore, any data that are
!> to be written out as character (i.e. CCITT IA5) values in
!> BUFR must be converted from character into real*8 format within the
!> application program before calling this subroutine. Conversely,
!> when this subroutine is being used to read character values from a
!> data subset, the value that is returned will be in real*8 format
!> and must be converted back into character format by the application
!> program before it can be used as such. Alternatively, there are
!> different subroutines such as readlc() and writlc() which can be
!> used to read/write character data directly from/to a data subset
!> without the need to convert from/to real*8 format as an intermediate
!> step.
!>
!> Numeric (i.e. non-character) data values within usr are always in
!> the exact units specified for the corresponding mnemonic within the
!> relevant DX or master BUFR table, without any scale or reference
!> values applied. Specifically, this means that, when writing
!> data values into an output subset, the user only needs to store each
!> respective value into usr using the units specified within the table,
!> and the NCEPLIBS-bufr software will take care of any necessary scaling or
!> referencing of the value before it is actually encoded into BUFR.
!> Conversely, when reading data values from an input subset, the
!> values returned in usr are already de-scaled and de-referenced and,
!> thus, are already in the exact units that were defined for the
!> corresponding mnemonics within the table.
!>
!> "Missing" values in usr are always denoted by a unique
!> placeholder value. This placeholder value is initially set
!> to a default value of 10E10_8, but it can be reset to
!> any substitute value of the user's choice via a separate
!> call to subroutine setbmiss(). In any case, and whenever this
!> subroutine is used to read data values from an input subset, any
!> returned value in usr can be easily checked for equivalence to the
!> current placeholder value via a call to function ibfms(), and a
!> positive result means that the value for the corresponding mnemonic
!> was encoded as "missing" in BUFR (i.e. all bits set to 1) within the
!> original data subset. Conversely, whenever this subroutine
!> is used to write data values to an output subset, the current
!> placeholder value can be obtained via a separate call to function
!> getbmiss(), and the resulting value can then be stored into the
!> usr array whereever the user desires a BUFR "missing" value (i.e.
!> all bits set to 1) to be encoded for the corresponding mnemonic
!> within the output subset.
!>
!> @remarks
!> - If lunin < 0, and if abs(lunin) points to a file that is open
!> for output (writing BUFR), then the subroutine will treat the file
!> pointed to by abs(lunin) as though it was open for input (reading
!> BUFR). This is a special capability for use by some applications
!> that need to read certain values back out from a BUFR file during
!> the same time that it is in the process of being written to.
!>
!> @param lunin - Absolute value is Fortran logical unit number for BUFR file
!> @param usr - Data values
!>  - If abs(lunin) was opened for input, then usr is output from this subroutine and
!>    contains data values that were read from the current data subset
!>  - If abs(lunin) was opened for output, then usr is input to this subroutine and
!>    contains data values that are to be written to the current data subset
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr
!>  - If abs(lunin) was opened for input, then i2 must be set equal to the second dimension
!>    of usr as allocated within the calling program
!>  - If abs(lunin) was opened for output, then i2 must be set equal to the number of replications
!>    of str that are to be written to the data subset
!> @param iret - Number of replications of str that were read/written from/to the data subset
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of data values
!> that will be read/written from/to the data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table B mnemonics)
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbstp(lunin,usr,i1,i2,iret,str)

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd

  implicit none

  character*(*), intent(in) :: str
  character*128 bort_str1, bort_str2, errstr

  real*8, intent(inout) :: usr(i1,i2)

  integer, intent(in) :: lunin, i1, i2
  integer, intent(out) :: iret
  integer iprt, ifirst1, my_lunin, my_i1, my_i2, lunit, lun, il, im, io, i, j

  common /quiet/ iprt

  data ifirst1 /0/

  save ifirst1

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunin,my_lunin,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbstp(my_lunin,usr,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  iret = 0

  ! Check the file status and inode
  lunit = abs(lunin)
  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBSTP - BUFR FILE IS CLOSED, IT MUST BE OPEN')
  if(im.eq.0) call bort('BUFRLIB: UFBSTP - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')
  if(inode(lun).ne.inv(1,lun)) call bort('BUFRLIB: UFBSTP - LOCATION OF INTERNAL TABLE FOR BUFR FILE DOES NOT AGREE ' // &
    'WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  io = min(max(0,il),1)
  if(lunit.ne.lunin) io = 0

  if(i1.le.0) then
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBSTP - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2.le.0) then
    if(iprt.eq.-1) ifirst1 = 1
    if(io.eq.0 .or. ifirst1.eq.0 .or. iprt.ge.1)  then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBSTP - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt.eq.0 .and. io.eq.1) then
        errstr = 'Note: Only the first occurrence of this WARNING message is printed, there may be more.  To output ' // &
          'all such messages,'
        call errwrt(errstr)
        errstr = 'modify your application program to add "CALL OPENBF(0,''QUIET'',1)" prior to the first call to a ' // &
          'BUFRLIB routine.'
        call errwrt(errstr)
      endif
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
      ifirst1 = 1
    endif
    return
  endif

  ! Initialize usr array preceeding an input operation
  if(io.eq.0) then
    do j=1,i2
      do i=1,I1
        usr(i,j) = bmiss
      enddo
    enddo
  endif

  ! Parse or recall the input string
  call string(str,lun,i1,io)

  ! Call the mnemonic reader/writer
  call ufbsp(lun,usr,i1,i2,io,iret)

  if(io.eq.1 .and. iret.ne.i2) then
    write(bort_str1,'("BUFRLIB: UFBSTP - MNEMONIC STRING READ IN IS: ",A)') str
    write(bort_str2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
      'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,") - INCOMPLETE WRITE")')  iret,i2
    call bort2(bort_str1,bort_str2)
  endif

  if(iret.eq.0 .and. io.eq.0 .and. iprt.ge.1) then
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    errstr = 'BUFRLIB: UFBSTP - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
    call errwrt(errstr)
    call errwrt(str)
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    call errwrt(' ')
  endif

  return
end subroutine ufbstp

!> Read or write an entire sequence of data values
!> from or to the BUFR data subset that is currently open within the
!> NCEPLIBS_bufr internal arrays.
!>
!> The direction of the data transfer is determined by the context of abs(lunin):
!> - If abs(lunin) points to a file that was previously opened for
!>   input using subroutine openbf(), then data values are read from
!>   the current data subset.
!> - If abs(lunin) points to a file that was previously opened for
!>   output using subroutine openbf(), then data values are written to
!>   the current data subset.
!>
!> This subroutine is specifically designed for use with a single
!> Table A or Table D mnemonic.  In the latter case, the mnemonic
!> may be replicated within the overall subset definition, and in
!> which case the subroutine will return all data values within all
!> replications of the sequence defined by the mnemonic.  But in
!> either case, the mnemonic itself may contain, within its own
!> sequence definition, any number of data values defined by Table B
!> mnemonics and/or subsequences of data values defined by other
!> Table D mnemonics, and any such subsequences may themselves be
!> replicated using any manner of fixed or delayed replication.
!> See [DX BUFR Tables](@ref ufbsubs) for more details including
!> an example use case, and see also subroutines ufbint(), ufbrep()
!> and ufbstp() which are also used to read/write one or more data
!> values from/to a data subset but cannot themselves be directly
!> used with Table A or Table D mnemonics.
!>
!> It is the user's responsibility to ensure that usr is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from or written to the data subset. Note also
!> that usr is an array of real*8 values; therefore, any data that are
!> to be written out as character (i.e. CCITT IA5) values in
!> BUFR must be converted from character into real*8 format within the
!> application program before calling this subroutine. Conversely,
!> when this subroutine is being used to read character values from a
!> data subset, the value that is returned will be in real*8 format
!> and must be converted back into character format by the application
!> program before it can be used as such. Alternatively, there are
!> different subroutines such as readlc() and writlc() which can be
!> used to read/write character data directly from/to a data subset
!> without the need to convert from/to real*8 format as an intermediate
!> step.
!>
!> Numeric (i.e. non-character) data values within usr are always in
!> the exact units specified for the corresponding mnemonic within the
!> relevant DX or master BUFR table, without any scale or reference
!> values applied. Specifically, this means that, when writing
!> data values into an output subset, the user only needs to store each
!> respective value into usr using the units specified within the table,
!> and the NCEPLIBS-bufr software will take care of any necessary scaling or
!> referencing of the value before it is actually encoded into BUFR.
!> Conversely, when reading data values from an input subset, the
!> values returned in usr are already de-scaled and de-referenced and,
!> thus, are already in the exact units that were defined for the
!> corresponding mnemonics within the table.
!>
!> "Missing" values in usr are always denoted by a unique
!> placeholder value. This placeholder value is initially set
!> to a default value of 10E10_8, but it can be reset to
!> any substitute value of the user's choice via a separate
!> call to subroutine setbmiss(). In any case, and whenever this
!> subroutine is used to read data values from an input subset, any
!> returned value in usr can be easily checked for equivalence to the
!> current placeholder value via a call to function ibfms(), and a
!> positive result means that the value for the corresponding mnemonic
!> was encoded as "missing" in BUFR (i.e. all bits set to 1) within the
!> original data subset. Conversely, whenever this subroutine
!> is used to write data values to an output subset, the current
!> placeholder value can be obtained via a separate call to function
!> getbmiss(), and the resulting value can then be stored into the
!> usr array whereever the user desires a BUFR "missing" value (i.e.
!> all bits set to 1) to be encoded for the corresponding mnemonic
!> within the output subset.
!>
!> @remarks
!> - If lunin < 0, and if abs(lunin) points to a file that is open
!> for output (writing BUFR), then the subroutine will treat the file
!> pointed to by abs(lunin) as though it was open for input (reading
!> BUFR). This is a special capability for use by some applications
!> that need to read certain values back out from a BUFR file during
!> the same time that it is in the process of being written to.
!> - If abs(lunin) points to a file that is open for output
!> (writing BUFR), and if the data values to be written are part of
!> a sequence replicated using delayed replication, then a call to
!> subroutine drfini() must be made prior to calling this subroutine,
!> in order to pre-allocate the necessary internal array space for
!> the number of replications of the sequence.
!>
!> @param lunin - Absolute value is Fortran logical unit number for BUFR file
!> @param usr - Data values
!>  - If abs(lunin) was opened for input, then usr is output from this subroutine and
!>    contains data values that were read from the current data subset
!>  - If abs(lunin) was opened for output, then usr is input to this subroutine and
!>    contains data values that are to be written to the current data subset
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr
!>  - If abs(lunin) was opened for input, then i2 must be set equal to the second dimension
!>    of usr as allocated within the calling program
!>  - If abs(lunin) was opened for output, then i2 must be set equal to the number of replications
!>    of str that are to be written to the data subset
!> @param iret - Number of replications of str that were read/written from/to the data subset
!> @param str - String consisting of a single Table A or Table D mnemonic whose sequence definition is
!> in one-to-one correspondence with the number of data values that will be read/written from/to the
!> data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table A and Table D mnemonics)
!>
!> @author J. Woollen @date 2000-09-19
recursive subroutine ufbseq(lunin,usr,i1,i2,iret,str)

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lunin, i1, i2
  integer, intent(out) :: iret
  integer, parameter :: mtag = 10
  integer iprt, ifirst1, ifirst2, my_lunin, my_i1, my_i2, lunit, lun, il, im, io, i, j, ntag, node, nods, ins1, ins2, insx, &
    nseq, isq, ityp, invwin, invtag

  real*8, intent(inout) :: usr(i1,i2)

  character*(*), intent(in) :: str
  character*156 bort_str
  character*128 errstr
  character*10 tags(mtag)

  common /quiet/ iprt

  data ifirst1 /0/, ifirst2 /0/

  save ifirst1, ifirst2

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunin,my_lunin,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbseq(my_lunin,usr,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  iret = 0

  ! Check the file status and inode
  lunit = abs(lunin)
  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBSEQ - BUFR FILE IS CLOSED, IT MUST BE OPEN')
  if(im.eq.0) call bort('BUFRLIB: UFBSEQ - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')

  io = min(max(0,il),1)
  if(lunit.ne.lunin) io = 0

  if(i1.le.0) then
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBSEQ - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2.le.0) then
    if(iprt.eq.-1) ifirst1 = 1
    if(io.eq.0 .or. ifirst1.eq.0 .or. iprt.ge.1)  then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBSEQ - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt.eq.0 .and. io.eq.1) then
        errstr = 'Note: Only the first occurrence of this WARNING message is printed, there may be more.  To output ' // &
          'all such messages,'
        call errwrt(errstr)
        errstr = 'modify your application program to add "CALL OPENBF(0,''QUIET'',1)" prior to the first call to a ' // &
          'BUFRLIB routine.'
        call errwrt(errstr)
      endif
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
      ifirst1 = 1
    endif
    return
  endif

  ! Check for valid sequence and sequence length arguments
  call parstr(str,tags,mtag,ntag,' ',.true.)
  if(ntag.lt.1) then
    write(bort_str,'("BUFRLIB: UFBSEQ - THE INPUT STRING (",A,") DOES NOT CONTAIN ANY MNEMONICS!!")') str
    call bort(bort_str)
  endif
  if(ntag.gt.1) then
    write(bort_str,'("BUFRLIB: UFBSEQ - THERE CANNOT BE MORE THAN '// &
      'ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE ",I3,")")') str,ntag
    call bort(bort_str)
  endif
  if(inode(lun).ne.inv(1,lun)) call bort('BUFRLIB: UFBSEQ - LOCATION OF INTERNAL TABLE FOR '// &
    'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  ! Initialize usr array preceeding an input operation
  if(io.eq.0) then
    do j=1,i2
      do i=1,I1
        usr(i,j) = bmiss
      enddo
    enddo
  endif

  ! Find the parameters of the specified sequence
  outer: do node=inode(lun),isc(inode(lun))
    if(str.eq.tag(node)) then
      if(typ(node).eq.'SEQ' .or. typ(node).eq.'RPC') then
        ins1 = 1
        do while (.true.)
          ins1 = invtag(node,lun,ins1,nval(lun))
          if(ins1.eq.0) exit outer
          if(typ(node).ne.'RPC' .or. val(ins1,lun).ne.0.) exit
          ins1 = ins1+1
        enddo
        ins2 = invtag(node,lun,ins1+1,nval(lun))
        if(ins2.eq.0) ins2 = 10E5
        nods = node
        do while(link(nods).eq.0 .and. jmpb(nods).gt.0)
          nods = jmpb(nods)
        enddo
        if(link(nods).eq.0) then
          insx = nval(lun)
        elseif(link(nods).gt.0) then
          insx = invwin(link(nods),lun,ins1+1,nval(lun))-1
        endif
        ins2 = min(ins2,insx)
      elseif(typ(node).eq.'SUB') then
        ins1 = 1
        ins2 = nval(lun)
      else
        write(bort_str,'("BUFRLIB: UFBSEQ - INPUT MNEMONIC ",A," MUST '// &
          'BE A SEQUENCE (HERE IT IS TYPE """,A,""")")') tags(1),typ(node)
        call bort(bort_str)
      endif
      nseq = 0
      do isq=ins1,ins2
        ityp = itp(inv(isq,lun))
        if(ityp.gt.1) nseq = nseq+1
      enddo
      if(nseq.gt.i1) then
        write(bort_str,'("BUFRLIB: UFBSEQ - INPUT SEQ. MNEM. ",A," CONSISTS OF",I4," TABLE B MNEM., .GT. THE MAX. '// &
          'SPECIFIED IN (INPUT) ARGUMENT 3 (",I3,")")') tags(1),nseq,i1
        call bort(bort_str)
      endif
      ! Frame a section of the buffer - return when no frame
      inner: do while (.true.)
        ins1 = invtag(node,lun,ins1,nval(lun))
        if(ins1.gt.nval(lun)) exit outer
        if(ins1.gt.0) then
          if(typ(node).eq.'RPC' .and. val(ins1,lun).eq.0.) then
            ins1 = ins1+1
            cycle
          elseif(io.eq.0 .and. iret+1.gt.i2) then
            if(iprt.ge.0) then
              call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
              write ( unit=errstr, fmt='(A,I5,A,A,A)' ) 'BUFRLIB: UFBSEQ - INCOMPLETE READ; ONLY THE FIRST ', i2, &
                ' (=4TH INPUT ARG.) ''LEVELS'' OF INPUT MNEMONIC ', tags(1), ' WERE READ'
              call errwrt(errstr)
              call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
              call errwrt(' ')
            endif
            exit outer
          endif
        elseif(ins1.eq.0) then
          if(io.eq.1 .and. iret.lt.i2) then
            write(bort_str,'("BUFRLIB: UFBSEQ - NO. OF ''LEVELS'' WRITTEN (",I5,") .LT. NO. REQUESTED (",I5,") - '// &
              'INCOMPLETE WRITE (INPUT MNEMONIC IS ",A,")")') iret,i2,tags(1)
            call bort(bort_str)
          endif
        else
          write(bort_str,'("BUFRLIB: UFBSEQ - VARIABLE INS1 MUST BE .GE. ZERO, HERE IT IS",I4," - INPUT MNEMONIC '// &
            'IS ",A)') ins1,tags(1)
          call bort(bort_str)
        endif
        if(ins1.eq.0 .or. iret.eq.i2) exit outer
        iret = iret+1
        ins1 = ins1+1
        ! Read/write user values
        j = ins1
        do i=1,nseq
          do while(itp(inv(j,lun)).lt.2)
            j = j+1
          enddo
          if(io.eq.0) usr(i,iret) = val(j,lun)
          if(io.eq.1) val(j,lun) = usr(i,iret)
          j = j+1
        enddo
      enddo inner
    endif
  enddo outer

  if(iret.eq.0) then
    if(io.eq.0) then
      if(iprt.ge.1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: UFBSEQ - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
        call errwrt(errstr)
        call errwrt(str)
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        call errwrt(' ')
      endif
    else
      if(iprt.eq.-1) ifirst2 = 1
      if(ifirst2.eq.0 .or. iprt.ge.1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: UFBSEQ - NO SPECIFIED VALUES WRITTEN OUT, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
        call errwrt(errstr)
        call errwrt(str)
        call errwrt('MAY NOT BE IN THE BUFR TABLE(?)')
        if(iprt.eq.0) then
          errstr = 'Note: Only the first occurrence of this WARNING message is printed, there may be more.  To output ' // &
           'all such messages,'
          call errwrt(errstr)
          errstr = 'modify your application program to add "CALL OPENBF(0,''QUIET'',1)" prior to the first call to a ' // &
           'BUFRLIB routine.'
          call errwrt(errstr)
        endif
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        call errwrt(' ')
        ifirst2 = 1
      endif
    endif
  endif

  return
end subroutine ufbseq
