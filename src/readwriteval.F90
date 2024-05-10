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
