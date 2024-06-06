!> @file
!> @brief Copy data between BUFR files or messages
!>
!> @author J. Woollen @date 1994-01-06

!> Copy an entire BUFR file from one Fortran logical unit to another.
!>
!> @param lunin - Fortran logical unit number for source BUFR file
!> @param lunot - Fortran logical unit number for target BUFR file
!>
!> The logical unit numbers lunin and lunot must already be associated with actual filenames on the local system,
!> typically via a Fortran "OPEN" statement.
!>
!> @remarks
!> - This subroutine uses subroutine msgwrt() to copy each BUFR
!> message from lunin to lunot; therefore, this subroutine can be
!> used to create a copy of lunin where each corresponding message
!> in lunot contains any or all of the updates described in the
!> documentation for subroutine msgwrt().
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine copybf(lunin,lunot)

  use modv_vars, only: im8b

  use moda_mgwa

  implicit none

  integer, intent(in) :: lunin, lunot
  integer my_lunin, my_lunot, lun, il, im, ier, iupbs01

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunin,my_lunin,1)
    call x84(lunot,my_lunot,1)
    call copybf(my_lunin,my_lunot)

    im8b=.true.
    return
  endif

  ! Check BUFR file statuses

  call status(lunin,lun,il,im)
  if(il.ne.0) call bort ('BUFRLIB: COPYBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED')
  call status(lunot,lun,il,im)
  if(il.ne.0) call bort ('BUFRLIB: COPYBF - OUTPUT BUFR FILE IS OPEN, IT MUST BE CLOSED')

  ! Connect the files for reading/writing to the C I/O interface

  call openbf(lunin,'INX',lunin)
  call openbf(lunot,'OUX',lunin)

  ! Read and copy a BUFR file from unit lunin to unit lunot

  ier = 0
  do while (ier.eq.0)
    call rdmsgw(lunin,mgwa,ier)
    if(ier.eq.0) call msgwrt(lunot,mgwa,iupbs01(mgwa,'LENM'))
  enddo

  ! Free up the file connections for the two files

  call closbf(lunin)
  call closbf(lunot)

  return
end subroutine copybf

!> Copy a BUFR message from one file to another.
!>
!> This subroutine is similar to subroutine cpymem(), except that
!> it copies a BUFR message from one Fortran logical unit to another,
!> whereas cpymem() copies a BUFR message from internal arrays in
!> memory to a specified Fortran logical unit.
!>
!> Logical unit lUNIN should have already been opened for input
!> operations via a previous call to subroutine openbf(). A BUFR
!> message should have already been read into internal arrays for
!> lunin via a previous call to one of the
!> [message-reading subroutines](@ref hierarchy).
!>
!> Logical unit lunot should have already been opened for output
!> operations via a previous call to subroutine openbf(), but there
!> should not be any BUFR message already open for output within the
!> internal arrays for lunot via a previous call to one of the
!> [message-writing subroutines](@ref hierarchy).
!>
!> The [DX BUFR Table information](@ref dfbftab) associated with
!> each of the logical units lunin and lunot must contain identical
!> definitions for the type of BUFR message to be copied from lunin
!> to lunot.
!>
!> This subroutine uses subroutine msgwrt() to write to lunot;
!> therefore, it can be used to transform a copy of the
!> original BUFR message from lunin with any or all of the updates
!> described in the documentation for subroutine msgwrt().
!>
!> @param lunin - Fortran logical unit number for source BUFR file
!> @param lunot - Fortran logical unit number for target BUFR file
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine copymg(lunin,lunot)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_bitbuf
  use moda_tables

  implicit none

  integer, intent(in) :: lunin, lunot
  integer my_lunin, my_lunot, lin, lot, il, im, mtyp, msbt, inod, mbym, iupbs01, iok2cpy

  character*8 subset

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunin,my_lunin,1)
    call x84(lunot,my_lunot,1)
    call copymg(my_lunin,my_lunot)

    im8b=.true.
    return
  endif

  ! Check the file statuses

  call status(lunin,lin,il,im)
  if(il.eq.0) call bort('BUFRLIB: COPYMG - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: COPYMG - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) call bort('BUFRLIB: COPYMG - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')

  call status(lunot,lot,il,im)
  if(il.eq.0) call bort('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il.lt.0) call bort('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im.ne.0) call bort('BUFRLIB: COPYMG - ALL MESSAGES MUST BE CLOSED IN OUTPUT BUFR FILE, A MESSAGE IS OPEN')

  ! Make sure both files have the same tables

  subset = tag(inode(lin))(1:8)
  call nemtba(lot,subset,mtyp,msbt,inod)
  if(inode(lin).ne.inod .and. iok2cpy(lin,lot).ne.1) &
    call bort('BUFRLIB: COPYMG - INPUT AND OUTPUT BUFR FILES MUST HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')

  ! Everything okay, so copy a message

  mbym = iupbs01(mbay(1,lin),'LENM')
  call msgwrt(lunot,mbay(1,lin),mbym)

  ! Set the message control words for partition associated with lunot

  nmsg(lot) = nmsg(lot) + 1
  nsub(lot) = msub(lin)
  msub(lot) = msub(lin)
  idate(lot) = idate(lin)
  inode(lot) = inod

  return
end subroutine copymg

!> Copy a BUFR data subset from one Fortran logical unit to another.
!>
!> Logical unit lunin should have already been opened for input
!> operations via a previous call to subroutine openbf(), and a BUFR
!> message should have already been read into internal arrays for
!> lunin via a previous call to one of the
!> [message-reading subroutines](@ref hierarchy).
!>
!> Logical unit lunot should have already been opened for output
!> operations via a previous call to subroutine openbf(), and a BUFR
!> message should already be open for output within internal arrays
!> via a previous call to one of the NCEPLIBS-bufr
!> [message-writing subroutines](@ref hierarchy).
!>
!> The compression status of the data subset (i.e. compressed or
!> uncompressed) will be preserved when copying from lunin to lunot.
!>
!> If lunot < 0, then a data subset is read from the BUFR message
!> in internal arrays for lunin but is not copied to the BUFR
!> message in internal arrays for lunot.  Otherwise, the
!> [DX BUFR Table information](@ref dfbftab) associated with
!> each of the logical units lunin and lunot must contain identical
!> definitions for the type of BUFR message containing the data
!> subset to be copied from lunin to lunot.
!>
!> @param lunin - Fortran logical unit number for source BUFR file
!> @param lunot - Fortran logical unit number for target BUFR file
!> @param iret - return code:
!>     - 0 = normal return
!>     - -1 = a BUFR data subset could not be read from the BUFR message in internal arrays for lunin
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine copysb(lunin,lunot,iret)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_bitbuf
  use moda_tables

  implicit none

  integer, intent(in) :: lunin, lunot
  integer, intent(out) :: iret
  integer my_lunin, my_lunot, lin, lot, il, im, mest, icmp, nbyt, len0, len1, len2, len3, len4, l5, iok2cpy

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunin,my_lunin,1)
    call x84(lunot,my_lunot,1)
    call copysb(my_lunin,my_lunot,iret)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  iret = 0

  ! Check the file statuses

  call status(lunin,lin,il,im)
  if(il.eq.0) call bort('BUFRLIB: COPYSB - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: COPYSB - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) call bort('BUFRLIB: COPYSB - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')

  if(lunot.gt.0) then
    call status(lunot,lot,il,im)
    if(il.eq.0) call bort('BUFRLIB: COPYSB - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
    if(il.lt.0) call bort('BUFRLIB: COPYSB - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
    if(im.eq.0) call bort('BUFRLIB: COPYSB - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')
    if( (inode(lin).ne.inode(lot)) .and. ( (tag(inode(lin)).ne.tag(inode(lot))) .or. (iok2cpy(lin,lot).ne.1) ) ) &
      call bort('BUFRLIB: COPYSB - INPUT AND OUTPUT BUFR FILES MUST HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')
  endif

  ! See if there is another subset in the message

  if(nsub(lin).eq.msub(lin)) then
    iret = -1
    return
  endif

  ! Check compression status of input message, output message will match

  call mesgbc(-lunin, mest, icmp)

  if(icmp.eq.1) then

    ! The input message is compressed, so read in the next subset and copy it as compressed to the output message.

    call readsb(lunin,iret)
    if(lunot.gt.0) then
      call ufbcpy(lunin,lunot)
      call cmpmsg('Y')
      call writsb(lunot)
      call cmpmsg('N')
    endif
  else

    ! The input message is uncompressed, so read in the next subset and copy it as uncompressed to the output message.

    ibit = (mbyt(lin))*8
    call upb(nbyt,16,mbay(1,lin),ibit)
    if (nbyt.gt.65530) then
      ! This is an oversized subset, so we can't rely on the value of nbyt as being the true size (in bytes) of the subset.
      if ( (nsub(lin).eq.0) .and. (msub(lin).eq.1) ) then
        ! But it's also the first and only subset in the message, so we can determine its true size in a different way.
        call getlens(mbay(1,lin), 4, len0, len1, len2, len3, len4, l5)
        nbyt = len4 - 4
      else
        ! We have no way to easily determine the true size of this oversized subset.
        iret = -1
        return
      endif
    endif
    if(lunot.gt.0) call cpyupd(lunot, lin, lot, nbyt)
    mbyt(lin) = mbyt(lin) + nbyt
    nsub(lin) = nsub(lin) + 1
  endif

  return
end subroutine copysb

!> Copy a BUFR data subset from one Fortran logical unit to another.
!>
!> This function calls subroutine copysb() and passes back its return code as the function value.
!>
!> @param lunin - Fortran logical unit number for source BUFR file
!> @param lunot - Fortran logical unit number for target BUFR file
!> @returns icopysb - return code:
!>     - 0 = normal return
!>     - -1 = a BUFR data subset could not be read from the BUFR message in internal arrays for lunin
!>
!> @remarks
!> - The use of this function allows the return code from copysb() to be used as the target variable
!> within an iterative program loop.
!>
!> @author J. Woollen @date 1994-01-06
recursive integer function icopysb(lunin,lunot) result(iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: lunin, lunot
  integer my_lunin, my_lunot

  if(im8b) then
    im8b=.false.

    call x84(lunin,my_lunin,1)
    call x84(lunot,my_lunot,1)
    iret=icopysb(my_lunin,my_lunot)

    im8b=.true.
    return
  endif

  call copysb(lunin,lunot,iret)

  return
end function icopysb

!> Check whether a BUFR message, or a data subset from within a BUFR message, can be copied from one Fortran logical
!> unit to another.
!>
!> The decision is based on whether both logical units contain
!> identical definitions for the BUFR message type in question within
!> their associated [DX BUFR Table information](@ref dfbftab).
!> Note that it's possible for a BUFR message type to be identically
!> defined for two different logical units even if the full set of
!> associated DX BUFR table information isn't identical between both
!> units.
!>
!> @param lui - File ID for source BUFR file
!> @param luo - File ID for target BUFR file
!> @returns iok2cpy - Flag indicating whether a BUFR message or data subset can be copied from lui to luo:
!> - 0 = No
!> - 1 = Yes
!>
!> @author J. Ator @date 2009-06-26
integer function iok2cpy(lui,luo) result(iret)

  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lui, luo
  integer icmpdx, mtyp, msbt, inod, ntei, nteo, i

  character*8 subset

  iret = 0

  ! Do both logical units have the same internal table information?

  if ( icmpdx(lui,luo) .eq. 1 ) then
    iret = 1
    return
  endif

  ! No, so get the Table A mnemonic from the message to be copied, then check whether that mnemonic is defined within the
  ! dictionary tables for the logical unit to be copied to.

  subset = tag(inode(lui))(1:8)
  call nemtbax(luo,subset,mtyp,msbt,inod)
  if ( inod .eq. 0 ) return

  ! The Table A mnemonic is defined within the dictionary tables for both units, so now make sure the definitions are identical.

  ntei = isc(inode(lui))-inode(lui)
  nteo = isc(inod)-inod
  if ( ntei .ne. nteo ) return

  do i = 1, ntei
    if ( tag(inode(lui)+i) .ne. tag(inod+i) ) return
    if ( typ(inode(lui)+i) .ne. typ(inod+i) ) return
    if ( isc(inode(lui)+i) .ne. isc(inod+i) ) return
    if ( irf(inode(lui)+i) .ne. irf(inod+i) ) return
    if ( ibt(inode(lui)+i) .ne. ibt(inod+i) ) return
  enddo

  iret = 1

  return
end function iok2cpy

!> Copy a BUFR message from internal arrays to a file.
!>
!> This subroutine is similar to subroutine copymg(), except that
!> it copies a BUFR message from internal arrays in memory to a
!> specified Fortran logical unit, whereas copymg() copies a BUFR
!> message from one Fortran logical unit to another.
!>
!> One or more files of BUFR messages should have already been
!> read into internal arrays in memory via one or more previous
!> calls to subroutine ufbmem(), and a BUFR message should already
!> be in scope for processing from these arrays via a previous call
!> to subroutine rdmemm() or readmm().
!>
!> Logical unit lunot should have already been opened for output
!> operations via a previous call to subroutine openbf(), but there
!> should not be any BUFR message already open for output within the
!> internal arrays for lunot via a previous call to one of the
!> [message-writing subroutines](@ref hierarchy).
!>
!> The [DX BUFR Table information](@ref dfbftab) associated with
!> the internal arrays in memory and with logical unit lunot must
!> contain identical definitions for the type of BUFR message to be
!> copied from the former to the latter.
!>
!> This subroutine uses subroutine msgwrt() to write to lunot;
!> therefore, it can be used to transform a copy of the
!> original BUFR message from memory with any or all of the updates
!> described in the documentation for subroutine msgwrt().
!>
!> @param lunot - Fortran logical unit number for target BUFR file.
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine cpymem(lunot)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_bitbuf
  use moda_msgmem
  use moda_tables

  implicit none

  integer, intent(in) :: lunot
  integer my_lunot, lin, lot, il, im, mtyp, msbt, inod, mbym, iupbs01, iok2cpy

  character*8  subset

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunot,my_lunot,1)
    call cpymem(my_lunot)

    im8b=.true.
    return
  endif

  ! Check the file statuses

  call status(munit,lin,il,im)
  if(im.eq.0) call bort('BUFRLIB: CPYMEM - A MESSAGE MUST BE OPEN IN INPUT BUFR MESSAGES IN INTERNAL MEMORY, NONE ARE')

  call status(lunot,lot,il,im)
  if(il.eq.0) call bort('BUFRLIB: CPYMEM - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il.lt.0) call bort('BUFRLIB: CPYMEM - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im.ne.0) call bort('BUFRLIB: CPYMEM - ALL MESSAGES MUST BE CLOSED IN OUTPUT BUFR FILE, A MESSAGE IS OPEN')

  ! Make sure both files have the same tables

  subset = tag(inode(lin))(1:8)
  call nemtba(lot,subset,mtyp,msbt,inod)
  if(inode(lin).ne.inod .and. iok2cpy(lin,lot).ne.1) &
    call bort('BUFRLIB: CPYMEM - INPUT BUFR MESSAGES IN INTERNAL MEMORY AND OUTPUT BUFR FILE MUST HAVE SAME INTERNAL '// &
      'TABLES (DIFFERENT HERE)')

  ! Everything okay, so copy a message

  mbym = iupbs01(mbay(1,lin),'LENM')
  call msgwrt(lunot,mbay(1,lin),mbym)

  ! Set the message control words for partition associated with lunot

  nmsg(lot) = nmsg(lot) + 1
  nsub(lot) = msub(lin)
  msub(lot) = msub(lin)
  idate(lot) = idate(lin)
  inode(lot) = inod

  return
end subroutine cpymem

!> Copy a BUFR data subset from one unit to another within internal memory.
!>
!> If the subset will not fit into the output message, or
!> if the subset byte count exceeds 65530 (sufficiently close to the
!> 16-bit byte counter upper limit of 65535), then that message is
!> flushed to lunit and a new one is created in order to hold the
!> copied subset. Any subset with byte count > 65530 will be written
!> into its own one-subset message. If the subset to be copied is
!> larger than the maximum message length, then a call is issued to
!> subroutine bort().
!>
!> @param lunit - Fortran logical unit number for BUFR file associated with output unit
!> @param lin - File ID for input unit
!> @param lun - File ID for output unit
!> @param ibyt - Length (in bytes) of data subset
!>
!> @author Woollen @date 1994-01-06
subroutine cpyupd(lunit,lin,lun,ibyt)

  use moda_msgcwd
  use moda_bitbuf

  implicit none

  integer, intent(in) :: lunit, lin, lun, ibyt
  integer nby0, nby1, nby2, nby3, nby4, nby5, iprt, lbit, lbyt, nbyt, iupb

  common /msgptr/ nby0,nby1,nby2,nby3,nby4,nby5

  common /quiet/ iprt

  character*128 bort_str, errstr

  logical msgfull

  ! Check whether the new subset should be written into the currently open message

  if(msgfull(mbyt(lun),ibyt,maxbyt) .or. ((ibyt.gt.65530).and.(nsub(lun).gt.0))) then
    ! NO it should not, either because:
    !  1) it doesn't fit,
    !          -- OR --
    !  2) it has byte count > 65530 (sufficiently close to the upper limit for the 16 bit byte counter placed at the
    !     beginning of each subset), AND the current message has at least one subset in it,
    ! SO write the current message out and create a new one to  hold the current subset
    call msgwrt(lunit,mbay(1,lun),mbyt(lun))
    call msgini(lun)
  endif

  if(msgfull(mbyt(lun),ibyt,maxbyt)) then
    write(bort_str,'("BUFRLIB: CPYUPD - THE LENGTH OF THIS SUBSET EXCEEDS THE MAXIMUM MESSAGE LENGTH (",I6,")")') maxbyt
    call bort(bort_str)
  endif

  ! Transfer subset from one message to the other.

  ! Note that we want to append the data for this subset to the end of Section 4, but the value in mbyt(lun) already includes
  ! the length of Section 5 (i.e. 4 bytes).  Therefore, we need to begin writing at the point 3 bytes prior to the byte
  ! currently pointed to by mbyt(lun).

  call mvb(mbay(1,lin),mbyt(lin)+1,mbay(1,lun),mbyt(lun)-3,ibyt)

  ! Update the subset and byte counters

  mbyt(lun) = mbyt(lun) + ibyt
  nsub(lun) = nsub(lun) + 1

  lbit = (nby0+nby1+nby2+4)*8
  call pkb(nsub(lun),16,mbay(1,lun),lbit)

  lbyt = nby0+nby1+nby2+nby3
  nbyt = iupb(mbay(1,lun),lbyt+1,24)
  lbit = lbyt*8
  call pkb(nbyt+ibyt,24,mbay(1,lun),lbit)

  ! If the subset byte count is > 65530, then give it its own one-subset message (i.e. we cannot have any other subsets
  ! in this message because their beginning would be beyond the upper limit of 65535 in the 16-bit byte counter, meaning
  ! they could not be located!)

  if(ibyt.gt.65530) then
    if(iprt.ge.1) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,I7,A,A)') 'BUFRLIB: CPYUPD - SUBSET HAS BYTE COUNT = ',ibyt,' > UPPER LIMIT OF 65535'
      call errwrt(errstr)
      call errwrt('>>>>>>>WILL BE COPIED INTO ITS OWN MESSAGE<<<<<<<<')
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    call msgwrt(lunit,mbay(1,lun),mbyt(lun))
    call msgini(lun)
  endif

  return
end subroutine cpyupd

!> Copy a BUFR data subset from one Fortran logical unit to another.
!>
!> This subroutine is similar to subroutine copysb(), except that here a
!> BUFR data subset should have already been read into internal arrays
!> for logical unit lubin via a previous call to one of the
!> [subset-reading subroutines](@ref hierarchy), whereas copysb()
!> only requires that a BUFR message should have already been read
!> into internal arrays via a previous call to one of the
!> [message-reading subroutines](@ref hierarchy).
!>
!> For logical unit lubot, a BUFR message should already be open
!> for output within internal arrays via a previous call to one of
!> the [message-writing subroutines](@ref hierarchy).
!>
!> The [DX BUFR Table information](@ref dfbftab) associated with
!> each of the logical units lubin and lubot must contain identical
!> definitions for the data subset to be copied.
!>
!> @param lubin - Fortran logical unit number for source BUFR file
!> @param lubot - Fortran logical unit number for target BUFR file
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbcpy(lubin,lubot)

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_ufbcpl
  use moda_tables

  implicit none

  integer, intent(in) :: lubin, lubot
  integer my_lubin, my_lubot, lui, luo, il, im, n, iok2cpy

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lubin,my_lubin,1)
    call x84(lubot,my_lubot,1)
    call ufbcpy(my_lubin,my_lubot)

    im8b=.true.
    return
  endif

  ! Check the file statuses and inode

  call status(lubin,lui,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBCPY - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: UFBCPY - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) call bort('BUFRLIB: UFBCPY - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
  if(inode(lui).ne.inv(1,lui)) &
    call bort('BUFRLIB: UFBCPY - LOCATION OF INTERNAL TABLE FOR INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION '// &
      'IN INTERNAL SUBSET ARRAY')

  call status(lubot,luo,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBCPY - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il.lt.0) call bort('BUFRLIB: UFBCPY - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im.eq.0) call bort('BUFRLIB: UFBCPY - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')

  if( (inode(lui).ne.inode(luo)) .and. ( (tag(inode(lui)).ne.tag(inode(luo))) .or. (iok2cpy(lui,luo).ne.1) ) ) &
    call bort('BUFRLIB: UFBCPY - INPUT AND OUTPUT BUFR FILES MUST HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')

  ! Everything okay, so copy user array from lui to luo

  nval(luo) = nval(lui)

  do n=1,nval(lui)
    inv(n,luo) = inv(n,lui)
    nrfelm(n,luo) = nrfelm(n,lui)
    val(n,luo) = val(n,lui)
  enddo

  luncpy(luo)=lubin

  return
end subroutine ufbcpy

!> Copy all of the DX BUFR table information from one unit to another within internal memory.
!>
!> @param lud - File ID for input unit
!> @param lun - File ID for output unit
!>
!> @author Woollen @date 1994-01-06
subroutine cpbfdx(lud,lun)

  use moda_msgcwd
  use moda_tababd

  implicit none

  integer, intent(in) :: lud, lun
  integer i

  ! Initialize the dictionary table partition

  call dxinit(lun,0)

  ! Positional index for Table A mnem.
  inode(lun) = inode(lud)

  ntba(lun) = ntba(lud)
  ntbb(lun) = ntbb(lud)
  ntbd(lun) = ntbd(lud)

  ! Table A entries
  do i=1,ntba(lud)
    idna(i,lun,1) = idna(i,lud,1)
    idna(i,lun,2) = idna(i,lud,2)
    taba(i,lun) = taba(i,lud)
    mtab(i,lun) = mtab(i,lud)
  enddo

  ! Table B entries
  do i=1,ntbb(lud)
    idnb(i,lun) = idnb(i,lud)
    tabb(i,lun) = tabb(i,lud)
  enddo

  ! Copy Table D entries
  do i=1,ntbd(lud)
    idnd(i,lun) = idnd(i,lud)
    tabd(i,lun) = tabd(i,lud)
  enddo

  return
end subroutine cpbfdx

!> Copy a specified number of bytes from one packed binary array to another.
!>
!> @param ib1 - Packed input binary array
!> @param nb1 - Pointer to first byte in ib1 to copy from
!> @param ib2 - Packed output binary array
!> @param nb2 - Pointer to first byte in ib2 to copy to
!> @param nbm - Number of bytes to copy
!>
!> @author Woollen @date 1994-01-06
subroutine mvb(ib1,nb1,ib2,nb2,nbm)

  implicit none

  integer, intent(in) :: ib1(*), nb1, nb2, nbm
  integer, intent(out) :: ib2(*)
  integer jb1, jb2, n, nval

  jb1 = 8*(nb1-1)
  jb2 = 8*(nb2-1)

  do n=1,nbm
    call upb(nval,8,ib1,jb1)
    call pkb(nval,8,ib2,jb2)
  enddo

  return
end subroutine mvb

!> Copy unique elements of a data subset.
!>
!> Copy each unique element from an input subset into the identical mnemonic slot in the output subset.
!>
!> Before this subroutine is called:
!> - The input file must be opened for input via openbf()
!> - The output file must be opened for output via openbf()
!> - A message must be read from the input file via one of the [message-reading subroutines](@ref hierarchy)
!> - A data subset must be read from the input message via one of the [subset-reading subroutines](@ref hierarchy)
!> - A message must open for writing within the output file via one of the [message-writing subroutines](@ref hierarchy)
!>
!> After this subroutine is called, one of the [subset-writing subroutines](@ref hierarchy) must be called on the output
!> file to write the subset to file.
!>
!> @param lubin - Fortran logical unit number for input BUFR file
!> @param lubot - Fortran logical unit number for output BUFR file
!>
!> @author Woollen @date 1994-01-06
recursive subroutine ufbcup(lubin,lubot)

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_tables
  use moda_ivttmp

  implicit none

  integer, intent(in) :: lubin, lubot
  integer my_lubin, my_lubot, lui, luo, il, im, ntag, ni, no, nv, nin

  character*10 tago

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lubin,my_lubin,1)
    call x84(lubot,my_lubot,1)
    call ufbcup(my_lubin,my_lubot)
    im8b=.true.
    return
  endif

  ! Check the file statuses and inode

  call status(lubin,lui,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBCUP - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: UFBCUP - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) call bort('BUFRLIB: UFBCUP - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
  if(inode(lui).ne.inv(1,lui)) call bort('BUFRLIB: UFBCUP - LOCATION OF INTERNAL TABLE FOR '// &
    'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  call status(lubot,luo,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBCUP - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il.lt.0) call bort('BUFRLIB: UFBCUP - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im.eq.0) call bort('BUFRLIB: UFBCUP - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')

  ! Make a list of unique tags in the input buffer

  ntag = 0

  outer1: do ni=1,nval(lui)
    nin = inv(ni,lui)
    if(itp(nin).ge.2) then
      do nv=1,ntag
        if(ttmp(nv).eq.tag(nin)) cycle outer1
      enddo
      ntag = ntag+1
      itmp(ntag) = ni
      ttmp(ntag) = tag(nin)
    endif
  enddo outer1

  if(ntag.eq.0) call bort('BUFRLIB: UFBCUP - THERE ARE NO ELEMENTS (TAGS) IN INPUT SUBSET BUFFER')

  ! Now, using the list of unique tags, make one copy of the common elements to the output buffer

  outer2: do nv=1,ntag
    ni = itmp(nv)
    do no=1,nval(luo)
      tago = tag(inv(no,luo))
      if(ttmp(nv).eq.tago) then
        val(no,luo) = val(ni,lui)
        cycle outer2
      endif
    enddo
  enddo outer2

  return
end subroutine ufbcup
