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
  if ( il <= 0 ) return
  if ( inode(lun) /= inv(1,lun) ) return

  ! Starting from the beginning of the subset, locate the (ntagpv)th occurrence of tagpv.
  call fstag( lun, tagpv, ntagpv, 1, npv, ierft )
  if ( ierft /= 0 ) return

  ! Now, starting from the (ntagpv)th occurrence of tagpv, search forward or backward for the (ntagnb)th occurrence of tagnb.
  call fstag( lun, tagnb, ntagnb, npv, nnb, ierft )
  if ( ierft /= 0 ) return

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
  if ( il >= 0 ) return
  if ( inode(lun) /= inv(1,lun) ) return

  ! Starting from the beginning of the subset, locate the (ntagpv)th occurrence of tagpv.
  call fstag( lun, tagpv, ntagpv, 1, npv, ierft )
  if ( ierft /= 0 ) return

  ! Now, starting from the (ntagpv)th occurrence of tagpv, search forward or backward for the (ntagnb)th occurrence of tagnb.
  call fstag( lun, tagnb, ntagnb, npv, nnb, ierft )
  if ( ierft /= 0 ) return

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
!> with str set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and
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
  if(il==0) call bort('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il<0) call bort('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im==0) call bort('BUFRLIB: WRITLC - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')

  ! Check for tags (mnemonics) in input string (there can only be one)
  call parstr(str,tgs,maxtg,ntg,' ',.true.)
  if(ntg>1) then
    write(bort_str,'("BUFRLIB: WRITLC - THERE CANNOT BE MORE THAN '// &
      ' ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE",I4,")")') str,ntg
    call bort(bort_str)
  endif

  ! Check if a specific occurrence of the input string was requested; if not, then the default is to write the first occurrence
  call parutg(lun,1,tgs(1),nnod,kon,roid)
  if(kon==6) then
    ioid=nint(roid)
    if(ioid<=0) ioid = 1
    ctag = ' '
    ii = 1
    do while((ii<=10).and.(tgs(1)(ii:ii)/='#'))
      ctag(ii:ii)=tgs(1)(ii:ii)
      ii = ii + 1
    enddo
  else
    ioid = 1
    ctag = tgs(1)(1:10)
  endif

  if(iupbs3(mbay(1,lun),'ICMP')>0) then
    ! The message is compressed
    n = 1
    itagct = 0
    call usrtpl(lun,n,n)
    do while (n+1<=nval(lun))
      n = n+1
      node = inv(n,lun)
      if(itp(node)==1) then
        nbmp=int(matx(n,ncol))
        call usrtpl(lun,n,nbmp)
      elseif(ctag==tag(node)) then
        itagct = itagct + 1
        if(itagct==ioid) then
          if(itp(node)/=3) then
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
    do while(nsubs<nsub(lun))
      ibit = mbyte*8
      call upb(nbyt,16,mbay(1,lun),ibit)
      mbyte = mbyte + nbyt
      nsubs = nsubs + 1
    enddo
    if(nsubs/=nsub(lun)) then
      if(iprt>=0) then
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
    do while (n+1<=nval(lun))
      n = n+1
      node = inv(n,lun)
      mbit = mbit+nbit
      nbit = ibt(node)
      if(itp(node)==1) then
        call upbb(ival,nbit,mbit,mbay(1,lun))
        call usrtpl(lun,n,ival)
      elseif(ctag==tag(node)) then
        itagct = itagct + 1
        if(itagct==ioid) then
          if(itp(node)/=3) then
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
  if(iprt>=0) then
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
!> with str set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and
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
  if(il==0) call bort('BUFRLIB: READLC - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il>0) call bort('BUFRLIB: READLC - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im==0) call bort('BUFRLIB: READLC - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')

  ! Check for tags (mnemonics) in input string (there can only be one)
  call parstr(str,tgs,maxtg,ntg,' ',.true.)
  if(ntg>1) then
    write(bort_str,'("BUFRLIB: READLC - THERE CANNOT BE MORE THAN '// &
       'ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE ",I3,")")') str,ntg
    call bort(bort_str)
  endif

  ! Check if a specific occurrence of the input string was requested; if not, then the default is to return the
  ! first occurrence.
  call parutg(lun,0,tgs(1),nnod,kon,roid)
  if(kon==6) then
    ioid=nint(roid)
    if(ioid<=0) ioid = 1
    ctag = ' '
    ii = 1
    do while((ii<=10).and.(tgs(1)(ii:ii)/='#'))
      ctag(ii:ii)=tgs(1)(ii:ii)
      ii = ii + 1
    enddo
  else
    ioid = 1
    ctag = tgs(1)(1:10)
  endif

  ! Locate and decode the long character string
  if(msgunp(lun)==0.or.msgunp(lun)==1) then
    ! The message is not compressed
    itagct = 0
    do n=1,nval(lun)
      nod = inv(n,lun)
      if(ctag==tag(nod)) then
        itagct = itagct + 1
        if(itagct==ioid) then
          if(itp(nod)/=3) then
            write(bort_str,'("BUFRLIB: READLC - MNEMONIC ",A," DOES NOT '// &
              'REPRESENT A CHARACTER ELEMENT (ITP=",I2,")")') tgs(1),itp(nod)
            call bort(bort_str)
          endif
          nchr = nbit(n)/8
          if(nchr>lchr) then
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
    if(nrst>0) then
      itagct = 0
      do ii=1,nrst
        if(ctag==crtag(ii)) then
          itagct = itagct + 1
          if(itagct==ioid) then
            nchr = irnch(ii)
            if(nchr>lchr) then
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
  if(iprt>=0) then
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

!> Read or write one or more data values from or to a data subset.
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
  if(il==0) call bort('BUFRLIB: UFBINT - BUFR FILE IS CLOSED, IT MUST BE OPEN')
  if(im==0) call bort('BUFRLIB: UFBINT - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')
  if(inode(lun)/=inv(1,lun)) call bort('BUFRLIB: UFBINT - LOCATION OF INTERNAL TABLE FOR BUFR FILE DOES NOT AGREE ' // &
    'WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  io = min(max(0,il),1)
  if(lunit/=lunin) io = 0

  if(i1<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBINT - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2<=0) then
    if(iprt==-1) ifirst1 = 1
    if(io==0 .or. ifirst1==0 .or. iprt>=1)  then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBINT - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt==0 .and. io==1) then
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
  if(io==0) then
    do j=1,i2
      do i=1,I1
        usr(i,j) = bmiss
      enddo
    enddo
  endif

  ! Call the mnemonic reader/writer
  call ufbrw(lun,usr,i1,i2,io,iret)

  ! If incomplete write try to initialize replication sequence or return
  if(io==1 .and. iret/=i2 .and. iret>=0) then
    call trybump(lun,usr,i1,i2,io,iret)
    if(iret/=i2) then
      write(bort_str1,'("BUFRLIB: UFBINT - MNEMONIC STRING READ IN IS: ",A)') str
      write(bort_str2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
        'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,") - INCOMPLETE WRITE")')  iret,i2
      call bort2(bort_str1,bort_str2)
    endif
  elseif(iret==-1) then
    iret = 0
  endif

  if(iret==0) then
    if(io==0) then
      if(iprt>=1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: UFBINT - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
        call errwrt(errstr)
        call errwrt(str)
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        call errwrt(' ')
      endif
    else
      if(iprt==-1) ifirst2 = 1
      if(ifirst2==0 .or. iprt>=1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: UFBINT - NO SPECIFIED VALUES WRITTEN OUT, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
        call errwrt(errstr)
        call errwrt(str)
        call errwrt('MAY NOT BE IN THE BUFR TABLE(?)')
        if(iprt==0) then
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

!> Read or write one or more data values from or to a data subset.
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

  use modv_vars, only: im8b, bmiss, iac

  use moda_usrint
  use moda_msgcwd

  implicit none

  character*(*), intent(in) :: str
  character*128 bort_str1, bort_str2, errstr

  real*8, intent(inout) :: usr(i1,i2)

  integer, intent(in) :: lunin, i1, i2
  integer, intent(out) :: iret
  integer iprt, ifirst1, my_lunin, my_i1, my_i2, lunit, lun, il, im, io, iac_prev, i, j

  common /quiet/ iprt

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
  if(il==0) call bort('BUFRLIB: UFBREP - BUFR FILE IS CLOSED, IT MUST BE OPEN')
  if(im==0) call bort('BUFRLIB: UFBREP - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')
  if(inode(lun)/=inv(1,lun)) call bort('BUFRLIB: UFBREP - LOCATION OF INTERNAL TABLE FOR BUFR FILE DOES NOT AGREE ' // &
    'WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  io = min(max(0,il),1)
  if(lunit/=lunin) io = 0

  if(i1<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBREP - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2<=0) then
    if(iprt==-1) ifirst1 = 1
    if(io==0 .or. ifirst1==0 .or. iprt>=1)  then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBREP - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt==0 .and. io==1) then
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
  if(io==0) then
    do j=1,i2
      do i=1,i1
        usr(i,j) = bmiss
      enddo
    enddo
  endif

  ! Parse or recall the input string
  iac_prev = iac
  iac = 1
  call string(str,lun,i1,io)
  iac = iac_prev

  ! Call the mnemonic reader/writer
  call ufbrp(lun,usr,i1,i2,io,iret)

  if(io==1 .and. iret<i2) then
    write(bort_str1,'("BUFRLIB: UFBREP - MNEMONIC STRING READ IN IS: ",A)') str
    write(bort_str2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
      'WRITTEN (",I3,") LESS THAN THE NUMBER REQUESTED (",I3,") - INCOMPLETE WRITE")')  iret,i2
    call bort2(bort_str1,bort_str2)
  endif

  if(iret==0 .and. io==0 .and. iprt>=1) then
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    errstr = 'BUFRLIB: UFBREP - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
    call errwrt(errstr)
    call errwrt(str)
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    call errwrt(' ')
  endif

  return
end subroutine ufbrep

!> Read or write one or more data values from or to a data subset.
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
  if(il==0) call bort('BUFRLIB: UFBSTP - BUFR FILE IS CLOSED, IT MUST BE OPEN')
  if(im==0) call bort('BUFRLIB: UFBSTP - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')
  if(inode(lun)/=inv(1,lun)) call bort('BUFRLIB: UFBSTP - LOCATION OF INTERNAL TABLE FOR BUFR FILE DOES NOT AGREE ' // &
    'WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  io = min(max(0,il),1)
  if(lunit/=lunin) io = 0

  if(i1<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBSTP - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2<=0) then
    if(iprt==-1) ifirst1 = 1
    if(io==0 .or. ifirst1==0 .or. iprt>=1)  then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBSTP - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt==0 .and. io==1) then
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
  if(io==0) then
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

  if(io==1 .and. iret/=i2) then
    write(bort_str1,'("BUFRLIB: UFBSTP - MNEMONIC STRING READ IN IS: ",A)') str
    write(bort_str2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
      'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,") - INCOMPLETE WRITE")')  iret,i2
    call bort2(bort_str1,bort_str2)
  endif

  if(iret==0 .and. io==0 .and. iprt>=1) then
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    errstr = 'BUFRLIB: UFBSTP - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
    call errwrt(errstr)
    call errwrt(str)
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    call errwrt(' ')
  endif

  return
end subroutine ufbstp

!> Read or write an entire sequence of data values from or to a data subset.
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
  if(il==0) call bort('BUFRLIB: UFBSEQ - BUFR FILE IS CLOSED, IT MUST BE OPEN')
  if(im==0) call bort('BUFRLIB: UFBSEQ - A MESSAGE MUST BE OPEN IN BUFR FILE, NONE ARE')

  io = min(max(0,il),1)
  if(lunit/=lunin) io = 0

  if(i1<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBSEQ - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2<=0) then
    if(iprt==-1) ifirst1 = 1
    if(io==0 .or. ifirst1==0 .or. iprt>=1)  then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBSEQ - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt==0 .and. io==1) then
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
  if(ntag<1) then
    write(bort_str,'("BUFRLIB: UFBSEQ - THE INPUT STRING (",A,") DOES NOT CONTAIN ANY MNEMONICS!!")') str
    call bort(bort_str)
  endif
  if(ntag>1) then
    write(bort_str,'("BUFRLIB: UFBSEQ - THERE CANNOT BE MORE THAN '// &
      'ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE ",I3,")")') str,ntag
    call bort(bort_str)
  endif
  if(inode(lun)/=inv(1,lun)) call bort('BUFRLIB: UFBSEQ - LOCATION OF INTERNAL TABLE FOR '// &
    'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  ! Initialize usr array preceeding an input operation
  if(io==0) then
    do j=1,i2
      do i=1,I1
        usr(i,j) = bmiss
      enddo
    enddo
  endif

  ! Find the parameters of the specified sequence
  outer: do node=inode(lun),isc(inode(lun))
    if(str==tag(node)) then
      if(typ(node)=='SEQ' .or. typ(node)=='RPC') then
        ins1 = 1
        do while (.true.)
          ins1 = invtag(node,lun,ins1,nval(lun))
          if(ins1==0) exit outer
          if(typ(node)/='RPC' .or. val(ins1,lun)/=0.) exit
          ins1 = ins1+1
        enddo
        ins2 = invtag(node,lun,ins1+1,nval(lun))
        if(ins2==0) ins2 = 10E5
        nods = node
        do while(link(nods)==0 .and. jmpb(nods)>0)
          nods = jmpb(nods)
        enddo
        if(link(nods)==0) then
          insx = nval(lun)
        elseif(link(nods)>0) then
          insx = invwin(link(nods),lun,ins1+1,nval(lun))-1
        endif
        ins2 = min(ins2,insx)
      elseif(typ(node)=='SUB') then
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
        if(ityp>1) nseq = nseq+1
      enddo
      if(nseq>i1) then
        write(bort_str,'("BUFRLIB: UFBSEQ - INPUT SEQ. MNEM. ",A," CONSISTS OF",I4," TABLE B MNEM., .GT. THE MAX. '// &
          'SPECIFIED IN (INPUT) ARGUMENT 3 (",I3,")")') tags(1),nseq,i1
        call bort(bort_str)
      endif
      ! Frame a section of the buffer - return when no frame
      inner: do while (.true.)
        ins1 = invtag(node,lun,ins1,nval(lun))
        if(ins1>nval(lun)) exit outer
        if(ins1>0) then
          if(typ(node)=='RPC' .and. val(ins1,lun)==0.) then
            ins1 = ins1+1
            cycle
          elseif(io==0 .and. iret+1>i2) then
            if(iprt>=0) then
              call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
              write ( unit=errstr, fmt='(A,I5,A,A,A)' ) 'BUFRLIB: UFBSEQ - INCOMPLETE READ; ONLY THE FIRST ', i2, &
                ' (=4TH INPUT ARG.) ''LEVELS'' OF INPUT MNEMONIC ', tags(1), ' WERE READ'
              call errwrt(errstr)
              call errwrt('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
              call errwrt(' ')
            endif
            exit outer
          endif
        elseif(ins1==0) then
          if(io==1 .and. iret<i2) then
            write(bort_str,'("BUFRLIB: UFBSEQ - NO. OF ''LEVELS'' WRITTEN (",I5,") .LT. NO. REQUESTED (",I5,") - '// &
              'INCOMPLETE WRITE (INPUT MNEMONIC IS ",A,")")') iret,i2,tags(1)
            call bort(bort_str)
          endif
        else
          write(bort_str,'("BUFRLIB: UFBSEQ - VARIABLE INS1 MUST BE .GE. ZERO, HERE IT IS",I4," - INPUT MNEMONIC '// &
            'IS ",A)') ins1,tags(1)
          call bort(bort_str)
        endif
        if(ins1==0 .or. iret==i2) exit outer
        iret = iret+1
        ins1 = ins1+1
        ! Read/write user values
        j = ins1
        do i=1,nseq
          do while(itp(inv(j,lun))<2)
            j = j+1
          enddo
          if(io==0) usr(i,iret) = val(j,lun)
          if(io==1) val(j,lun) = usr(i,iret)
          j = j+1
        enddo
      enddo inner
    endif
  enddo outer

  if(iret==0) then
    if(io==0) then
      if(iprt>=1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: UFBSEQ - NO SPECIFIED VALUES READ IN, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
        call errwrt(errstr)
        call errwrt(str)
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        call errwrt(' ')
      endif
    else
      if(iprt==-1) ifirst2 = 1
      if(ifirst2==0 .or. iprt>=1) then
        call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
        errstr = 'BUFRLIB: UFBSEQ - NO SPECIFIED VALUES WRITTEN OUT, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
        call errwrt(errstr)
        call errwrt(str)
        call errwrt('MAY NOT BE IN THE BUFR TABLE(?)')
        if(iprt==0) then
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

!> Explicitly initialize delayed replication factors
!> and allocate a corresponding amount of space within internal arrays,
!> thereby allowing the subsequent use of subroutine ufbseq() to write
!> data into delayed replication sequences.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param mdrf - Array of delayed replication factors, in one-to-one correspondence with the number of occurrences
!> of drftag within the overall subset definition, and explicitly defining how much space (i.e. how many replications)
!> to allocate within each successive occurrence
!> @param ndrf - Number of delayed replication factors within mdrf
!> @param drftag - Table D sequence mnemonic, bracketed by appropriate delayed replication notation (e.g. {}, () OR <>)
!>
!> Logical unit lunit should have already been opened for output
!> operations (i.e. writing/encoding BUFR) via a previous call to
!> subroutine openbf(), and a message for output should have already
!> been opened via a previous call to one of the
!> [message-writing subroutines].
!>
!> The use of this subroutine is only required when writing data
!> into delayed replication sequences using ufbseq(), or for cases
!> where ufbint() or ufbrep() are being used to write data into
!> delayed replication sequences which occur more than once within
!> an overall subset definition.  In such cases, the use of this
!> subroutine allows the application code to explicitly specify how
!> many replications of the sequence are to be allocated to each
!> occurrence of the delayed replication sequence within the overall
!> subset definition, prior to storing all of the actual data values
!> themselves via a single subsequent call to ufbint() or ufbrep().
!> In contrast, the use of this subroutine is not required when
!> ufbint() or ufbrep() are to be called to store data values
!> for a delayed replication sequence which only occurs one time
!> within an overall subset definition, because in that case the
!> same type of initialization and space allocation functionality
!> will be automatically handled internally within subroutine
!> ufbint() or ufbrep().
!>
!> @author J. Woollen @date 2002-05-14
recursive subroutine drfini(lunit,mdrf,ndrf,drftag)

  use modv_vars, only: im8b

  use moda_usrint
  use moda_tables

  implicit none

  character*(*), intent(in) :: drftag

  integer, intent(in) :: mdrf(*), lunit, ndrf
  integer, parameter :: mxdrf = 2000
  integer my_mdrf(mxdrf), my_lunit, my_ndrf, mdrf4, ii, lun, il, im, m, n, node

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    do ii = 1, ndrf
      call x84(mdrf(ii),mdrf4,1)
      my_mdrf(ii) = mdrf4
    enddo
    call x84(ndrf,my_ndrf,1)
    call drfini(my_lunit,my_mdrf,my_ndrf,drftag)
    im8b=.true.
    return
  endif

  call status(lunit,lun,il,im)
  ! Conform the template to the delayed replication factors
  m = 0
  n = 0
  do n = n+1, nval(lun)
    node = inv(n,lun)
    if(itp(node)==1 .and. tag(node)==drftag) then
      m = m+1
      call usrtpl(lun,n,mdrf(m))
    endif
  enddo

  return
end subroutine drfini

!> Write or read specified values to or from
!> the current BUFR data subset within internal arrays, with the
!> direction of the data transfer determined by the context of io.
!>
!> The data values correspond to internal arrays representing parsed
!> strings of mnemonics which are part of a delayed-replication
!> sequence, or for which there is no replication at all.
!>
!> This subroutine should never be directly called by an application
!> program; instead, an application program should directly call ufbint()
!> which will internally call this subroutine.
!>
!> @param lun - File ID
!> @param usr - Data values
!> @param i1 - Length of first dimension of usr
!> @param i2 - Length of second dimension of usr
!> @param io - Status indicator for BUFR file associated with lun:
!> - 0 input file
!> - 1 output file
!> @param iret - Number of "levels" of data values read from or written to data subset
!> - -1 none of the mnemonics in the string passed to ufbint() were found in the data subset template
!>
!> @author J. Woollen @date 1994-01-06
subroutine ufbrw(lun,usr,i1,i2,io,iret)

  use modv_vars, only: bmiss

  use moda_usrint
  use moda_tables
  use moda_msgcwd

  implicit none

  integer, intent(in) :: lun, i1, i2, io
  integer, intent(out) :: iret
  integer iprt, nnod, ncon, nods, nodc, ivls, kons, inc1, inc2, ins1, ins2, invn, i, j, invwin, ibfms, lstjpb

  real*8, intent(inout) :: usr(i1,i2)

  character*128 errstr
  character*10 tagstr, subset

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)
  common /quiet/ iprt

  subset=tag(inode(lun))
  iret = 0

  ! Loop over condition windows
  inc1 = 1
  inc2 = 1
  outer: do while (.true.)
    call conwin(lun,inc1,inc2)
    if(nnod==0) then
      iret = i2
      return
    elseif(inc1==0) then
      return
    else
      do j=1,nnod
        if(nods(j)>0) then
          ins2 = inc1
          call getwin(nods(j),lun,ins1,ins2)
          if(ins1==0) return
          do while (.true.)
            ! Loop over store nodes
            iret = iret+1
            if(iprt>=2) then
              call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
              call errwrt('UFBRW LEV TAG     IO   INS1   INVN   INS2  '//SUBSET)
              call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
              do i=1,nnod
                if(io==0) tagstr=tag(nods(i))(1:8)//' R'
                if(io==1) tagstr=tag(nods(i))(1:8)//' W'
                invn = invwin(nods(i),lun,ins1,ins2)
                if(invn==0.and.io==1) call drstpl(nods(i),lun,ins1,ins2,invn)
                write(errstr,'("LEV=",I5,1X,A,3I7)') iret,tagstr,ins1,invn,ins2
                call errwrt(errstr)
              enddo
            endif
            ! Write user values
            if(io==1 .and. iret<=i2) then
              do i=1,nnod
                if(nods(i)>0) then
                  if(ibfms(usr(i,iret))==0) then
                    invn = invwin(nods(i),lun,ins1,ins2)
                    if(invn==0) then
                      call drstpl(nods(i),lun,ins1,ins2,invn)
                      if(invn==0) then
                        iret = 0
                        return
                      endif
                      call newwin(lun,inc1,inc2)
                      val(invn,lun) = usr(i,iret)
                    elseif(lstjpb(nods(i),lun,'RPS')==0) then
                      val(invn,lun) = usr(i,iret)
                    elseif(ibfms(val(invn,lun))/=0) then
                      val(invn,lun) = usr(i,iret)
                    else
                      call drstpl(nods(i),lun,ins1,ins2,invn)
                      if(invn==0) then
                        iret = 0
                        return
                      endif
                      call newwin(lun,inc1,inc2)
                      val(invn,lun) = usr(i,iret)
                    endif
                  endif
                endif
              enddo
            endif
            ! Read user values
            if(io==0 .and. iret<=i2) then
              do i=1,nnod
                usr(i,iret) = bmiss
                if(nods(i)>0) then
                  invn = invwin(nods(i),lun,ins1,ins2)
                  if(invn>0) usr(i,iret) = val(invn,lun)
                endif
              enddo
            endif
            ! Decide what to do next
            if(io==1.and.iret==i2) return
            call nxtwin(lun,ins1,ins2)
            if(ins1>0 .and. ins1<inc2) cycle
            if(ncon>0) cycle outer
            return
          enddo
        endif
      enddo
      iret = -1
      return
    endif
  enddo outer

  return
end subroutine ufbrw

!> Write or read specified data values to or
!> from the current BUFR data subset within internal arrays, with the
!> direction of the data transfer determined by the context of io.
!>
!> The data values correspond to internal arrays representing parsed
!> strings of mnemonics which are either part of a fixed (i.e. non-delayed)
!> replication sequence, or for mnememonics which are replicated by being
!> directly listed more than once within an overall subset definition.
!>
!> This subroutine should never be directly called by an application
!> program; instead, an application program should directly call ufbrep()
!> which will internally call this subroutine.
!>
!> @param lun - File ID
!> @param usr - Data values
!> @param i1 - Length of first dimension of usr
!> @param i2 - Length of second dimension of usr
!> @param io - Status indicator for BUFR file associated with lun:
!> - 0 input file
!> - 1 output file
!> @param iret - Number of "levels" of data values read from or written to data subset
!>
!> @author J. Woollen @date 1994-01-06
subroutine ufbrp(lun,usr,i1,i2,io,iret)

  use moda_usrint

  implicit none

  integer, intent(in) :: lun, i1, i2, io
  integer, intent(out) :: iret
  integer nnod, ncon, nods, nodc, ivls, kons, ins1, ins2, invn, i, nz, invtag

  real*8, intent(inout) :: usr(i1,i2)

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)

  iret = 0
  ins1 = 0
  ins2 = 0

  ! Find first non-zero node in string
  do nz=1,nnod
    if(nods(nz)>0) then
      do while (.true.)
        ! Frame a section of the buffer - return when no frame
        if(ins1+1>nval(lun)) return
        if(io==1 .and. iret==i2) return
        ins1 = invtag(nods(nz),lun,ins1+1,nval(lun))
        if(ins1==0) return
        ins2 = invtag(nods(nz),lun,ins1+1,nval(lun))
        if(ins2==0) ins2 = nval(lun)
        iret = iret+1
        ! Read user values
        if(io==0 .and. iret<=i2) then
          do i=1,nnod
            if(nods(i)>0) then
              invn = invtag(nods(i),lun,ins1,ins2)
              if(invn>0) usr(i,iret) = val(invn,lun)
            endif
          enddo
        endif
        ! Write user values
        if(io==1 .and. iret<=i2) then
          do i=1,nnod
            if(nods(i)>0) then
              invn = invtag(nods(i),lun,ins1,ins2)
              if(invn>0) val(invn,lun) = usr(i,iret)
            endif
          enddo
        endif
      enddo
    endif
  enddo

  return
end subroutine ufbrp

!> Write or read specified values to or
!> from the current BUFR data subset within internal arrays, with the
!> direction of the data transfer determined by the context of io.
!>
!> The data values correspond to internal arrays representing parsed
!> strings of mnemonics which are either part of a fixed (i.e. non-delayed)
!> replication sequence, or for mnememonics which are replicated by being
!> directly listed more than once within an overall subset definition.
!>
!> This subroutine should never be directly called by an application
!> program; instead, an application program should directly call ufbstp()
!> which will internally call this subroutine.
!>
!> This subroutine is similar to subroutine ufbrp(), but it is designed
!> for different use cases.  For a more detailed explanation of how
!> subroutine ufbstp() differs from subroutine ufbrep(), and therefore
!> how this subroutine differs from subroutine ufbrp(), see the
!> discussion in [DX BUFR Tables](@ref ufbsubs).
!>
!> @param lun - File ID
!> @param usr - Data values
!> @param i1 - Length of first dimension of usr
!> @param i2 - Length of second dimension of usr
!> @param io - Status indicator for BUFR file associated with lun:
!> - 0 input file
!> - 1 output file
!> @param iret - Number of "levels" of data values read from or written to data subset
!>
!> @author J. Woollen @date 1999-11-18
subroutine ufbsp(lun,usr,i1,i2,io,iret)

  use moda_usrint

  implicit none

  integer, intent(in) :: lun, i1, i2, io
  integer, intent(out) :: iret
  integer nnod, ncon, nods, nodc, ivls, kons, ins1, ins2, invn, invm, i, invtag

  real*8, intent(inout) :: usr(i1,i2)

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)

  iret = 0
  ins1 = 0
  ins2 = 0

  do while (.true.)
    ! Frame a section of the buffer - return when no frame
    if(ins1+1>nval(lun)) return
    ins1 = invtag(nods(1),lun,ins1+1,nval(lun))
    if(ins1==0) return
    ins2 = invtag(nods(1),lun,ins1+1,nval(lun))
    if(ins2==0) ins2 = nval(lun)
    iret = iret+1
    ! Read user values
    if(io==0 .and. iret<=i2) then
      invm = ins1
      do i=1,nnod
        if(nods(i)>0) then
          invn = invtag(nods(i),lun,invm,ins2)
          if(invn>0) usr(i,iret) = val(invn,lun)
          invm = max(invn,invm)
        endif
      enddo
    endif
    ! Write user values
    if(io==1 .and. iret<=i2) then
      invm = ins1
      do i=1,nnod
        if(nods(i)>0) then
          invn = invtag(nods(i),lun,invm,ins2)
          if(invn>0) val(invn,lun) = usr(i,iret)
          invm = max(invn,invm)
        endif
      enddo
    endif
  enddo

  return
end subroutine ufbsp

!> Write a long character string (greater than 8 bytes) to a data subset.
!>
!> Normally, subroutine writlc() is used to write a long character
!> string to a data subset.  However, subroutine writlc() can only be
!> called <b>after</b> a call to one of the
!> [subset-writing subroutines](@ref hierarchy), so it will not work
!> for cases when one of those subroutines flushes the message
!> containing the data subset in question to logical unit lunit during
!> the same call to that subroutine, such as when the data subset
!> contains more than 65530 bytes.  When this happens, there is no
!> longer any way for a subsequent writlc() call to write a long
!> character string into that data subset, because the data subset has
!> already been flushed from internal memory.  This subroutine solves
!> that problem, by allowing a long character string to be specified
!> <b>before</b> calling one of the
!> [subset-writing subroutines](@ref hierarchy), and the string value
!> will be held and stored automatically (via an internal call to
!> subroutine writlc()) at the proper time during the subsequent call
!> to the [subset-writing subroutines](@ref hierarchy).
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param chr - Value corresponding to str
!> @param str - Table B mnemonic of long character string to be written, possibly supplemented with an ordinal
!> occurrence notation
!>
!> If there is more than one occurrence of str within the data subset
!> definition, then each occurrence can be written via a separate call
!> to this subroutine, and by appending the ordinal number of the
!> occurrence to str in each case.  For example, if there are 5
!> occurrences of mnemonic LSTID within a given data subset definition,
!> then 5 separate calls should be made to this subroutine, once each
!> with str set to 'LSTID#1', 'LSTID#2', 'LSTID#3', 'LSTID#4' and
!> 'LSTID#5'.  However, the first notation is superfluous, because
!> omitting the ordinal number always defaults to the first occurrence
!> of a particular string, so a user could just specify 'LSTID'
!> instead of 'LSTID#1'.
!>
!> @remarks
!> - Character strings which are 8 bytes or less in length can be
!> written by converting the string into a real*8 value within the
!> application program, and then using the real*8 usr array within a
!> call to one of the NCEPLIBS-bufr
!> [values-writing subroutines](@ref hierarchy)
!> prior to calling one of the
!> [subset-writing subroutines](@ref hierarchy)
!> for the data subset.
!>
!> @author J. Ator @date 2014-02-05
recursive subroutine hold4wlc(lunit,chr,str)

  use modv_vars, only: im8b, mxh4wlc

  use moda_h4wlc

  implicit none

  integer, intent(in) :: lunit
  integer my_lunit, iprt, lens, lenc, i

  character*(*), intent(in) :: chr, str

  character*128 errstr
  character*14 mystr

  common /quiet/ iprt

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call hold4wlc(my_lunit,chr,str)
    im8b=.true.
    return
  endif

  call strsuc( str, mystr, lens )
  if ( lens == -1 ) return

  lenc = min( len( chr ), 120 )

  ! If this subroutine has already been called with this mnemonic for this particular subset, then overwrite the
  ! corresponding entry in the internal holding area
  if ( nh4wlc > 0 ) then
    do i = 1, nh4wlc
      if ( ( lunit == luh4wlc(i) ) .and. ( mystr(1:lens) == sth4wlc(i)(1:lens) ) ) then
        chh4wlc(i) = ''
        chh4wlc(i)(1:lenc) = chr(1:lenc)
        return
      endif
    enddo
  endif

  ! Otherwise, use the next available unused entry in the holding area
  if ( nh4wlc >= mxh4wlc ) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I3)' ) 'BUFRLIB: HOLD4WLC - THE MAXIMUM NUMBER OF LONG CHARACTER ', &
        'STRINGS THAT CAN BE HELD INTERNALLY IS ', mxh4wlc
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    endif
  else
    nh4wlc = nh4wlc + 1
    luh4wlc(nh4wlc) = lunit
    sth4wlc(nh4wlc) = ''
    sth4wlc(nh4wlc)(1:lens) = mystr(1:lens)
    chh4wlc(nh4wlc) = ''
    chh4wlc(nh4wlc)(1:lenc) = chr(1:lenc)
  endif

  return
end subroutine hold4wlc

!> Try to expand a delayed replication sequence.
!>
!> Check the first node associated with a character string (parsed into arrays in common block /usrstr/) in
!> order to determine if it represents a delayed replication sequence.  If so, then the delayed replication
!> sequence is initialized and expanded (i.e. "bumped") to the value of input argument i2.
!> A call is then made to subroutine ufbrw() in order to write user data
!> into the newly expanded replication sequence.
!>
!> This subroutine is usually called from ufbint() whenever that routine receives a
!> non-zero return code from ufbrw().  The cause of a bad return from ufbrw() is usually a delayed replication
!> sequence which isn't expanded enough to hold the array of data the user is trying to
!> write.  So this subroutine is one last chance to try to resolve that situation.
!>
!> @note Argument io is always passed in with a value of 1 at the present time.  In the future the subroutine
!> may be expanded to allow it to operate on input files.
!>
!> @param lun - File ID
!> @param usr - Data values
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Number of levels of data values that are to be written to the data subset
!> @param io - Status indicator for BUFR file:
!> - 0 = Input file (possible future use)
!> - 1 = Output file
!> @param iret - Number of ldevels of data values that were written to the data subset
!>
!> @author Woollen @date 1994-01-06
subroutine trybump(lun,usr,i1,i2,io,iret)

  use moda_usrint

  implicit none

  integer, intent(in) :: lun, i1, i2, io
  integer, intent(out) :: iret
  integer nnod, ncon, nods, nodc, ivls, kons, ndrp, invn, jnvn, knvn, invwin, lstjpb

  real*8, intent(inout) :: usr(i1,i2)

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)

  ! See if there's a delayed replication group involved

  ndrp = lstjpb(nods(1),lun,'DRP')
  if(ndrp<=0) return

  ! If so, clean it out and bump it to i2

  invn = invwin(ndrp,lun,1,nval(lun))
  val(invn,lun) = 0
  jnvn = invn+1
  do while(nint(val(jnvn,lun))>0)
    jnvn = jnvn+nint(val(jnvn,lun))
  enddo
  do knvn=1,nval(lun)-jnvn+1
    inv(invn+knvn,lun) = inv(jnvn+knvn-1,lun)
    val(invn+knvn,lun) = val(jnvn+knvn-1,lun)
  enddo
  nval(lun) = nval(lun)-(jnvn-invn-1)
  call usrtpl(lun,invn,i2)

  ! Call the mnemonic writer

  call ufbrw(lun,usr,i1,i2,io,iret)

  return
end subroutine trybump

!> Overwrite one or more data values within a data subset.
!>
!> Overwrite specified values which exist in current internal BUFR subset arrays in a file open for output.
!> The data values correspond to mnemonics which are part of a delayed-replication sequence, or for which
!> there is no replication at all.  Either subroutine openmg() or openmb() must have been previously called
!> to open and initialize a BUFR message within memory for this lunit.  In addition, subroutine writsb() or
!> invmrg() must have previously been called to store the data values that are now intended to be overwritten
!> within the internal output subset arrays.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param usr - Data values
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Number of replications of str that are to be written to the data subset
!> @param iret - Number of replications of str that were written to the data subset
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of data values
!> that will be written to the data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table B mnemonics)
!>
!> @author Woollen @date 1994-01-06
recursive subroutine ufbovr(lunit,usr,i1,i2,iret,str)

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd

  implicit none

  integer, intent(in) :: lunit, i1, i2
  integer, intent(out) :: iret
  integer iprt, ifirst1, my_lunit, my_i1, my_i2, lun, il, im, io

  character*(*), intent(in) :: str
  character*128 bort_str1, bort_str2, errstr

  real*8, intent(inout) :: usr(i1,i2)

  common /quiet/ iprt

  data ifirst1 /0/

  save ifirst1

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbovr(my_lunit,usr,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  iret = 0

  ! Check the file status and inode

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: UFBOVR - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il<0) call bort('BUFRLIB: UFBOVR - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im==0) call bort('BUFRLIB: UFBOVR - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')
  if(inode(lun)/=inv(1,lun)) call bort('BUFRLIB: UFBOVR - LOCATION OF INTERNAL TABLE FOR '// &
    'OUTPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  io = min(max(0,il),1)

  if(i1<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBOVR - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2<=0) then
    if(iprt==-1) ifirst1 = 1
    if(io==0 .or. ifirst1==0 .or. iprt>=1) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBOVR - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      if(iprt==0 .and. io==1) then
        errstr = 'Note: Only the first occurrence of this WARNING ' // &
          'message is printed, there may be more.  To output all such messages,'
        call errwrt(errstr)
        errstr = 'modify your application program to add ' // &
          '"CALL OPENBF(0,''QUIET'',1)" prior to the first call to a BUFRLIB routine.'
        call errwrt(errstr)
      endif
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
      ifirst1 = 1
    endif
    return
  endif

  ! Parse or recall the input string - write values

  call string(str,lun,i1,io)
  call trybump(lun,usr,i1,i2,io,iret)

  if(io==1 .and. iret/=i2) then
    write(bort_str1,'("BUFRLIB: UFBOVR - MNEMONIC STRING READ IN IS: ",A)') str
    write(bort_str2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
      'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,") - INCOMPLETE WRITE")') iret, i2
    call bort2(bort_str1,bort_str2)
  endif

  return
end subroutine ufbovr

!> Read one or more data values from an NCEP prepbufr file.
!>
!> This subroutine is specifically designed for use with NCEP prepbufr files,
!> which contain a third dimension of data events for every
!> reported data value at every replicated vertical level.  It is
!> similar to subroutine ufbin3(), except that ufbin3() is used
!> for NCEP prepfits files and has one extra argument containing
!> the maximum number of data events for any data value, whereas
!> this subroutine is used for NCEP prepbufr files and stores the
!> same information internally within a common block.
!>
!> It is the user's responsibility to ensure that usr is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from the data subset.  Note also
!> that usr is an array of real*8 values; therefore, any
!> character (i.e. CCITT IA5) value in the data subset will be
!> returned in real*8 format and must be converted back into character
!> format by the application program before it can be used as such.
!>
!> "Missing" values in usr are always denoted by a unique
!> placeholder value.  This placeholder value is initially set
!> to a default value of 10E10_8, but it can be reset to
!> any substitute value of the user's choice via a separate
!> call to subroutine setbmiss().  In any case, any
!> returned value in usr can be easily checked for equivalence to the
!> current placeholder value via a call to function ibfms(), and a
!> positive result means that the value for the corresponding mnemonic
!> was encoded as "missing" in BUFR (i.e. all bits set to 1) within the
!> original data subset.
!>
!> @param lunit - Fortran logical unit number for NCEP prepbufr file
!> @param usr - Data values
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr as allocated within the calling program
!> @param i3 - Third dimension of usr as allocated within the calling program
!> @param iret - Number of replications of str that were read from the data subset, corresponding to the second dimension
!> of usr
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of data values
!> that will be read/written from/to the data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table B mnemonics)
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbevn(lunit,usr,i1,i2,i3,iret,str)

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd

  implicit none

  character*(*), intent(in) :: str
  character*128 errstr

  integer, intent(in) :: lunit, i1, i2, i3
  integer, intent(out) :: iret
  integer invn(255), nnod, ncon, nods, nodc, ivls, kons, maxevn, iprt, my_lunit, my_i1, my_i2, my_i3, i, j, k, lun, il, im, &
    ins1, ins2, inc1, inc2, nnvn, nvnwin

  real*8, intent(out) :: usr(i1,i2,i3)

  logical nodgt0

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)
  common /ufbn3c/ maxevn
  common /quiet/ iprt

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call x84(i3,my_i3,1)
    call ufbevn(my_lunit,usr,my_i1,my_i2,my_i3,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  maxevn = 0
  iret = 0

  ! Check the file status and inode

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: UFBEVN - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il>0) call bort('BUFRLIB: UFBEVN - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im==0) call bort('BUFRLIB: UFBEVN - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
  if(inode(lun)/=inv(1,lun)) call bort('BUFRLIB: UFBEVN - LOCATION OF INTERNAL TABLE FOR '// &
    'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  if(i1<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBEVN - 3rd ARG. (INPUT) IS .LE. 0, SO RETURN WITH 6th ARG. (IRET) = 0; 7th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBEVN - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 6th ARG. (IRET) = 0; 7th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i3<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBEVN - 5th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 6th ARG. (IRET) = 0; 7th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  endif

  ! Parse or recall the input string

  call string(str,lun,i1,0)

  ! Initialize usr array

  do k=1,i3
    do j=1,i2
      do i=1,i1
        usr(i,j,k) = bmiss
      enddo
    enddo
  enddo

  ! Loop over condition windows

  inc1 = 1
  inc2 = 1
  outer: do while (.true.)
    call conwin(lun,inc1,inc2)
    if(nnod==0) then
      iret = i2
      return
    elseif(inc1==0) then
      return
    else
      nodgt0 = .false.
      do i=1,nnod
        if(nods(i)>0) then
          ins2 = inc1
          call getwin(nods(i),lun,ins1,ins2)
          if(ins1==0) return
          nodgt0 = .true.
          exit
        endif
      enddo
      if(.not.nodgt0) then
        ins1 = inc1
        ins2 = inc2
      endif
      ! Read push down stack data into 3D arrays
      inner: do while (.true.)
        iret = iret+1
        if(iret<=i2) then
          do j=1,nnod
            if(nods(j)>0) then
              nnvn = nvnwin(nods(j),lun,ins1,ins2,invn,i3)
              maxevn = max(nnvn,maxevn)
              do k=1,nnvn
                usr(j,iret,k) = val(invn(k),lun)
              enddo
            endif
          enddo
        endif
        ! Decide what to do next
        call nxtwin(lun,ins1,ins2)
        if(ins1<=0 .or. ins1>=inc2) exit inner
      enddo inner
      if(ncon<=0) exit outer
    endif
  enddo outer

  if(iret==0) then
    if(iprt>=1) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBEVN - NO SPECIFIED VALUES READ IN, SO RETURN WITH 6th ARG. (IRET) = 0; 7th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
  endif

  return
end subroutine ufbevn

!> Read one or more data values from an NCEP prepfits file.
!>
!> This subroutine is specifically designed for use with NCEP prepfits files,
!> which contain a third dimension of data events for every
!> reported data value at every replicated vertical level.  It is
!> similar to subroutine ufbevn(), except that ufbevn() is used
!> for NCEP prepbufr files and stores the maximum number of data
!> events for any data value within an internal common block,
!> whereas this subroutine is used for NCEP prepfits files and
!> has one extra argument which returns the same information to
!> the calling program.
!>
!> It is the user's responsibility to ensure that usr is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from the data subset.  Note also
!> that usr is an array of real*8 values; therefore, any
!> character (i.e. CCITT IA5) value in the data subset will be
!> returned in real*8 format and must be converted back into character
!> format by the application program before it can be used as such.
!>
!> "Missing" values in usr are always denoted by a unique
!> placeholder value.  This placeholder value is initially set
!> to a default value of 10E10_8, but it can be reset to
!> any substitute value of the user's choice via a separate
!> call to subroutine setbmiss().  In any case, any
!> returned value in usr can be easily checked for equivalence to the
!> current placeholder value via a call to function ibfms(), and a
!> positive result means that the value for the corresponding mnemonic
!> was encoded as "missing" in BUFR (i.e. all bits set to 1) within the
!> original data subset.
!>
!> @param lunit - Fortran logical unit number for NCEP prepfits file
!> @param usr - Data values
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr as allocated within the calling program
!> @param i3 - Third dimension of usr as allocated within the calling program
!> @param iret - Number of replications of str that were read from the data subset, corresponding to the second dimension
!> of usr
!> @param jret - Maximum number of data events for any data value that was read from the data subset at any replicated
!> vertical level, and corresponding to the third dimension of usr
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of data values
!> that will be read/written from/to the data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table B mnemonics)
!>
!> @author J. Woollen @date 2003-11-04
recursive subroutine ufbin3(lunit,usr,i1,i2,i3,iret,jret,str)

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd

  implicit none

  character*(*), intent(in) :: str
  character*128 errstr

  integer, intent(in) :: lunit, i1, i2, i3
  integer, intent(out) :: iret, jret
  integer nnod, ncon, nods, nodc, ivls, kons, iprt, my_lunit, my_i1, my_i2, my_i3, i, j, k, lun, il, im, &
    ins1, ins2, inc1, inc2, nnvn, nevn

  real*8, intent(out) :: usr(i1,i2,i3)

  logical nodgt0

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)
  common /quiet/ iprt

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call x84(i3,my_i3,1)
    call ufbin3(my_lunit,usr,my_i1,my_i2,my_i3,iret,jret,str)
    call x48(iret,iret,1)
    call x48(jret,jret,1)
    im8b=.true.
    return
  endif

  iret = 0
  jret = 0

  ! Check the file status and inode

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: UFBIN3 - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il>0) call bort('BUFRLIB: UFBIN3 - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im==0) call bort('BUFRLIB: UFBIN3 - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
  if(inode(lun)/=inv(1,lun)) call bort('BUFRLIB: UFBIN3 - LOCATION OF INTERNAL TABLE FOR '// &
    'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  if(i1<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBIN3 - 3rd ARG. (INPUT) IS .LE. 0, ' // &
        'SO RETURN WITH 6th AND 7th ARGS. (IRET, JRET) = 0; 8th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBIN3 - 4th ARG. (INPUT) IS .LE. 0, ' // &
        'SO RETURN WITH 6th AND 7th ARGS. (IRET, JRET) = 0; 8th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i3<=0) then
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBIN3 - 5th ARG. (INPUT) IS .LE. 0, ' // &
        'SO RETURN WITH 6th AND 7th ARGS. (IRET, JRET) = 0; 8th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  endif

  ! Parse or recall the input string

  call string(str,lun,i1,0)

  ! Initialize usr array

  do k=1,i3
    do j=1,i2
      do i=1,i1
        usr(i,j,k) = bmiss
      enddo
    enddo
  enddo

  ! Loop over condition windows

  inc1 = 1
  inc2 = 1
  outer: do while (.true.)
    call conwin(lun,inc1,inc2)
    if(nnod==0) then
      iret = i2
      return
    elseif(inc1==0) then
      return
    else
      nodgt0 = .false.
      do i=1,nnod
        if(nods(i)>0) then
          ins2 = inc1
          call getwin(nods(i),lun,ins1,ins2)
          if(ins1==0) return
          nodgt0 = .true.
          exit
        endif
      enddo
      if(.not.nodgt0) then
        ins1 = inc1
        ins2 = inc2
      endif
      ! Read push down stack data into 3D arrays
      inner: do while (.true.)
        iret = iret+1
        if(iret<=i2) then
          do j=1,nnod
            nnvn = nevn(nods(j),lun,ins1,ins2,i1,i2,i3,usr(j,iret,1))
            jret = max(jret,nnvn)
          enddo
        endif
        ! Decide what to do next
        call nxtwin(lun,ins1,ins2)
        if(ins1<=0 .or. ins1>=inc2) exit inner
      enddo inner
      if(ncon<=0) exit outer
    endif
  enddo outer

  if(iret==0 .or. jret==0) then
    if(iprt>=1) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBIN3 - NO SPECIFIED VALUES READ IN, ' // &
        'SO RETURN WITH 6th AND/OR 7th ARGS. (IRET, JRET) = 0; 8th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
  endif

  return
end subroutine ufbin3

!> Read one or more data values from a specified data subset.
!>
!> If logical unit lunit has already been opened for input operations
!> via a previous call to subroutine openbf(), then this subroutine
!> will save the current file position, rewind the file to the
!> beginning, reposition the file to a specified data subset
!> within a specified message, read one or more specified data values
!> from that data subset via an internal call to ufbint(), and then
!> restore the file to its previous position.
!>
!> Otherwise, if logical unit lunit has not already been opened for
!> input operations via a previous call to subroutine openbf(),
!> then this subroutine will open it via an internal call to
!> subroutine openbf(), position the file to a specified data subset
!> within a specified message, read one or more specified data values
!> from that data subset via an internal call to ufbint(), and then
!> close the file via an internal call to subroutine closbf().
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param imsg - Number of BUFR message to be read from the
!> BUFR file, counting from the beginning of the file, but <b>not</b>
!> counting any DX BUFR table messages which may be present in the file
!> @param isub - Number of data subset to be read from the
!> (imsg)th BUFR message, counting from the beginning of the message
!> @param usr - Data values
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr as allocated within the calling program
!> @param iret - Number of replications of str that were read from the data subset
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of data values
!> that will be read from the data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table B mnemonics)
!>
!> @author Woollen @date 2003-11-04
recursive subroutine ufbinx(lunit,imsg,isub,usr,i1,i2,iret,str)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_bitbuf

  implicit none

  integer, intent(in) :: lunit, imsg, isub, i1, i2
  integer, intent(out) :: iret
  integer my_lunit, my_imsg, my_isub, my_i1, my_i2, lun, il, im, jdate, jret, i

  character*(*), intent(in) :: str
  character*128 bort_str
  character*8 subset

  real*8, intent(out) :: usr(i1,i2)

  logical openit

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(imsg,my_imsg,1)
    call x84(isub,my_isub,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbinx(my_lunit,my_imsg,my_isub,usr,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  call status(lunit,lun,il,im)
  openit = il==0

  if(openit) then
    ! Open BUFR file connected to unit lunit if it isn't already open
    call openbf(lunit,'INX',lunit)
  else
    ! If BUFR file already opened, save position and rewind to first data message
    call rewnbf(lunit,0)
  endif

  ! Skip to the requested message
  do i=1,imsg
    call readmg(lunit,subset,jdate,jret)
    if(jret<0) then
      write(bort_str,'("BUFRLIB: UFBINX - HIT END OF FILE BEFORE '// &
        'READING REQUESTED MESSAGE NO.",I5," IN BUFR FILE CONNECTED TO UNIT",I4)')  imsg, lunit
      call bort(bort_str)
    endif
  enddo

  ! Position at the requested subset
  do i=1,isub
    call readsb(lunit,jret)
    if(jret/=0) then
      write(bort_str,'("BUFRLIB: UFBINX - ALL SUBSETS READ BEFORE '// &
        'READING REQ. SUBSET NO.",I3," IN REQ. MSG NO.",I5," IN BUFR FILE CONNECTED TO UNIT",I4)') isub, imsg, lunit
      call bort(bort_str)
    endif
  enddo

  ! Read the requested data values
  call ufbint(lunit,usr,i1,i2,iret,str)

  if(openit) then
    ! Close BUFR file if it was opened here
    call closbf(lunit)
  else
    ! Restore BUFR file to its previous status and position
    call rewnbf(lunit,1)
  endif

  return
end subroutine ufbinx

!> Read one or more data values from a data subset without advancing the subset pointer.
!>
!> The data values to be read must be one-dimensional (i.e. non-replicated).
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param tab - Data values
!> @param i1 - Size of tab as allocated within the calling program
!> @param iret - Return code:
!> - 0 = Normal return
!> - -1 = There are no more subsets in the BUFR message
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of data values
!> that will be read from the data subset into tab
!>
!> @author Woollen @date 1994-01-06
recursive subroutine ufbget(lunit,tab,i1,iret,str)

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_usrbit
  use moda_msgcwd
  use moda_bitbuf
  use moda_tables

  implicit none

  integer*8 ival
  integer, intent(in) :: lunit, i1
  integer, intent(out) :: iret
  integer nnod, ncon, nods, nodc, ivls, kons, my_lunit, my_i1, lun, il, im, i, n, node, nbmp, kbit, invn, invwin

  character*(*), intent(in) :: str
  character*8 cval

  real*8, intent(out) :: tab(i1)
  real*8 rval, ups

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)

  equivalence (cval,rval)

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(i1,my_i1,1)
    call ufbget(my_lunit,tab,my_i1,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  iret = 0

  do i=1,i1
    tab(i) = bmiss
  enddo

  ! Make sure a file/message is open for input

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: UFBGET - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il>0) call bort('BUFRLIB: UFBGET - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im==0) call bort('BUFRLIB: UFBGET - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')

  ! See if there's another subset in the message

  if(nsub(lun)==msub(lun)) then
    iret = -1
    return
  endif

  ! Parse the string

  call string(str,lun,i1,0)

  ! Expand the template for this subset as little as possible

  n = 1
  nbit(n) = 0
  mbit(n) = mbyt(lun)*8 + 16
  call usrtpl(lun,n,n)
  do n=n+1,nval(lun)
    node = inv(n,lun)
    nbit(n) = ibt(node)
    mbit(n) = mbit(n-1)+nbit(n-1)
    if(node==nods(nnod)) then
      nval(lun) = n
      exit
    elseif(itp(node)==1) then
      call upb8(ival,nbit(n),mbit(n),mbay(1,lun))
      nbmp=int(ival)
      call usrtpl(lun,n,nbmp)
    endif
  enddo

  ! Unpack only the nodes found in the string

  do i=1,nnod
    node = nods(i)
    invn = invwin(node,lun,1,nval(lun))
    if(invn>0) then
      call upb8(ival,nbit(invn),mbit(invn),mbay(1,lun))
      if(itp(node)==1) then
        tab(i) = ival
      elseif(itp(node)==2) then
        if(ival<2_8**(ibt(node))-1) tab(i) = ups(ival,node)
      elseif(itp(node)==3) then
        cval = ' '
        kbit = mbit(invn)
        call upc(cval,nbit(invn)/8,mbay(1,lun),kbit,.true.)
        tab(i) = rval
      endif
    else
      tab(i) = bmiss
    endif
  enddo

  return
end subroutine ufbget

!> Read one or more data values from a stacked data event within a specified portion of the current data subset.
!>
!> Search for all stacked data events within the portion of the current
!> subset buffer bounded by the indices inv1 and inv2.  All such
!> events are accumulated and returned to the calling program within
!> array usr.  The value of the function itself is the total number
!> of events found.
!>
!> @param node - Jump/link table index of node for which to return stacked values
!> @param lun  - File ID
!> @param inv1 - Starting index of the portion of the subset buffer in which to look for stack values
!> @param inv2 - Ending index of the portion of the subset buffer in which to look for stack values
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr as allocated within the calling program
!> @param i3 - Third dimension of usr as allocated within the calling program
!> @param usr - Starting address of data values read from data subset; events are returned in the third dimension for a
!> particular data value and level in the first and second dimensions
!> @returns - Number of events in stack (must be less than or equal to i3)
!>
!> @note: This routine should only be called by routine ufbin3(), which itself is called only by verification
!> application program gridtobs, where it was previously an in-line subroutine.  In general, nevn() does not work
!> properly in other application programs at this time.
!>
!> @author J. Woollen @date 2003-11-04
integer function nevn(node,lun,inv1,inv2,i1,i2,i3,usr) result(iret)

  use moda_usrint

  implicit none

  integer, intent(in) :: node, lun, inv1, inv2, i1, i2, i3
  integer ndrs, invn, n1, n2, l, n, invwin, lstjpb

  character*128 bort_str

  real*8, intent(out) :: usr(i1,i2,i3)

  iret = 0

  ! Find the enclosing event stack descriptor

  ndrs = lstjpb(node,lun,'DRS')
  if(ndrs<=0) return

  invn = invwin(ndrs,lun,inv1,inv2)
  if(invn==0) call bort('BUFRLIB: iret - CAN''T FIND THE EVENT STACK!!!!!!')

  iret = nint(val(invn,lun))
  if(iret>i3) then
    write(bort_str,'("BUFRLIB: NEVN - THE NO. OF EVENTS FOR THE '// &
      'REQUESTED STACK (",I3,") EXCEEDS THE VALUE OF THE 3RD DIM. OF THE USR ARRAY (",I3,")")') iret, i3
    call bort(bort_str)
  endif

  ! Search each stack level for the requested node and copy the value

  n2 = invn + 1

  do l=1,iret
    n1 = n2
    n2 = n2 + nint(val(n1,lun))
    do n=n1,n2
      if(inv(n,lun)==node) usr(1,1,l) = val(n,lun)
    enddo
  enddo

  return
end function nevn
