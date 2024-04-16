!> @file
!> @brief Read or write compressed BUFR messages
!>
!> @author J. Ator J. Woollen @date 2005-03-09

!> Specify whether BUFR messages output by future calls to [message-writing subroutines](@ref hierarchy) and
!> [subset-writing subroutines](@ref hierarchy) are to be compressed.
!>
!> This subroutine can be called at any time after the first call
!> to subroutine openbf(), and the specified value for cf will remain
!> in effect for all future calls to
!> [message-writing subroutines](@ref hierarchy) and
!> [subset-writing subroutines](@ref hierarchy) for all Fortran logical
!> units that are open for output within the application program, unless
!> a subsequent call is made to this subroutine to reset the value of
!> cf again.  If this subroutine is never called, a default value of
!> 'N' is used for cf, as set within subroutine bfrini().
!>
!> When compression is activated, it is implemented using the
!> algorithm for data subset compression prescribed within the
!> [official WMO BUFR regulations](@ref manual).
!> Compression is most useful when the data subsets to be
!> compressed are devoid of any delayed replication, and when
!> there is minimal variation of corresponding data values among
!> different data subsets within the same BUFR message. Otherwise,
!> compression may provide little to no benefit, and which is why
!> it is not activated by default.
!>
!> @param cf - Flag indicating whether future BUFR output messages are to be compressed
!>  - 'N' = No (the default)
!>  - 'Y' = Yes
!>
!> @author J. Ator @date 2005-03-09
subroutine cmpmsg(cf)

  implicit none

  character, intent(in) :: cf
  character ccmf
  character*128 bort_str

  common /msgcmp/ ccmf

  call capit(cf)
  if(cf.ne.'Y'.and. cf.ne.'N') then
    write(bort_str,'("BUFRLIB: CMPMSG - INPUT ARGUMENT IS ",A1,", IT MUST BE EITHER Y OR N")') cf
    call bort(bort_str)
  endif
  ccmf = cf

  return
end subroutine cmpmsg

!> Write a data subset into a BUFR message using compression.
!>
!> This subroutine is similar to subroutine writsb(), except that
!> when the subset is encoded and packed into the current message
!> for the BUFR file associated with logical unit lunit, it is
!> packed using compression as prescribed within the
!> [official WMO BUFR regulations](@ref manual).
!>
!> This subroutine activates compression via an internal call to
!> subroutine cmpmsg(), followed by an internal call to subroutine
!> writsb(), followed by a second internal call to subroutine
!> cmpmsg() to deactivate compression.  For this reason, most
!> application programs which write compressed BUFR messages now
!> call subroutines cmpmsg() and writsb() directly; however, this
!> subroutine is still supported within the NCEPLIBS-bufr software for
!> backwards-compatibility with certain legacy application programs.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!>
!> @author J. Woollen @date 2002-05-14
recursive subroutine writcp(lunit)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: lunit
  integer my_lunit

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call writcp(my_lunit)

    im8b=.true.
    return
  endif

  call cmpmsg('Y')
  call writsb(lunit)
  call cmpmsg('N')

  return
end subroutine writcp

!> Read the next compressed BUFR data subset into internal arrays.
!>
!> Uncompress and unpack the next subset
!> from the internal compressed message buffer (array mbay in module
!> @ref moda_bitbuf) and store the result within the internal
!> array val(*,lun) in module @ref moda_usrint.
!>
!> @param[in] lun - integer: File ID
!>
!> @author Woollen @date 2000-09-19
subroutine rdcmps(lun)

  use modv_vars, only: bmiss, mxrst

  use moda_usrint
  use moda_msgcwd
  use moda_bitbuf
  use moda_tables
  use moda_rlccmn
  use moda_stcode

  implicit none

  integer, intent(in) :: lun
  integer(8) :: ival, lref, ninc, lps
  integer nsbs, jbit, lbit, nbit, n, node, ityp, linc, lre4, nin4, nbmp, nchr, lelm, ibsv, igetrfel, icbfms

  real*8 rval, ups

  character*128 bort_str
  character*8 cref, cval

  equivalence (cval,rval)

  ! Statement function to compute BUFR "missing value" for field of length lbit bits (all bits "on")
  lps(lbit) = max(2_8**(lbit)-1,1)

  ! Setup the subset template

  call usrtpl(lun,1,1)

  ! Uncompress a subset into the val array according to Table B

  nsbs = nsub(lun)

  ! Note that we are going to unpack the (nsbs)th subset from within the current BUFR message.

  ibit = mbyt(lun)
  nrst = 0

  ! Loop through each element of the subset, including immediately resolving any replication sequences by emulating recursion
  ! via an explicit goto statement.

  n = 0
  11 do n=n+1,nval(lun)
    node = inv(n,lun)
    nrfelm(n,lun) = igetrfel(n,lun)
    nbit = ibt(node)
    ityp = itp(node)

    ! In each of the following code blocks, the "local reference value" for the element is determined first, followed by the
    ! 6-bit value which indicates how many bits are used to store the increment (i.e. offset) from this "local reference value".
    ! Then, we jump ahead to where this increment is stored for this particular subset, unpack it, and add it to the
    ! "local reference value" to determine the final uncompressed value for this element from this subset.  Note that, if an
    ! element has the same final uncompressed value for each subset in the message, then the encoding rules for BUFR compression
    ! dictate that the "local reference value" will be equal to this value, the 6-bit increment length indicator will have a
    ! value of zero, and the actual increments themselves will be omitted from the message.

    if(ityp.eq.1.or.ityp.eq.2) then
      ! This is a numeric element.
      if(nbit<=32) then
        call upb(lre4,nbit,mbay(1,lun),ibit)
        call upb(linc,6,mbay(1,lun),ibit)
        jbit = ibit + linc*(nsbs-1)
        call upb(nin4,linc,mbay(1,lun),jbit)
        lref = lre4
        ninc = nin4
      elseif(nbit<=64) then
        call up8(lref,nbit,mbay(1,lun),ibit)
        call upb(linc,6,mbay(1,lun),ibit)
        jbit = ibit + linc*(nsbs-1)
        call up8(ninc,linc,mbay(1,lun),jbit)
      endif
      if(ninc.eq.lps(linc)) then
        ival = lps(nbit)
      else
        ival = lref + ninc
      endif
      if(ityp.eq.1) then
        nbmp = int(ival)
        call usrtpl(lun,n,nbmp)
        if (iscodes(lun) .ne. 0) return
        goto 11
      endif
      if(ival.lt.lps(nbit)) val(n,lun) = ups(ival,node)
      call strbtm(n,lun)
      ibit = ibit + linc*msub(lun)
    elseif(ityp.eq.3) then
      ! This is a character element.  If there are more than 8 characters, then only the first 8 will be unpacked by this
      ! routine, and a separate subsequent call to subroutine readlc() will be required to unpack the remainder of the string.
      ! In this case, pointers will be saved within common /rlccmn/ for later use within readlc().
      lelm = nbit/8
      nchr = min(8,lelm)
      ibsv = ibit
      cref = ' '
      call upc(cref,nchr,mbay(1,lun),ibit,.true.)
      if(lelm.gt.8) then
        ibit = ibit + (lelm-8)*8
        nrst = nrst + 1
        if(nrst.gt.mxrst) then
          write(bort_str,'("BUFRLIB: RDCMPS - NUMBER OF LONG CHARACTER STRINGS EXCEEDS THE LIMIT (",I4,")")') mxrst
          call bort(bort_str)
        endif
        crtag(nrst) = tag(node)
      endif
      ! Unpack the increment length indicator.  For character elements, this length is in bytes rather than bits.
      call upb(linc,6,mbay(1,lun),ibit)
      if(linc.eq.0) then
        if(lelm.gt.8) then
          irnch(nrst) = lelm
          irbit(nrst) = ibsv
        endif
        cval = cref
      else
        jbit = ibit + linc*(nsbs-1)*8
        if(lelm.gt.8) then
          irnch(nrst) = linc
          irbit(nrst) = jbit
        endif
        nchr = min(8,linc)
        cval = ' '
        call upc(cval,nchr,mbay(1,lun),jbit,.true.)
      endif
      if (lelm.le.8 .and. icbfms(cval,nchr).ne.0) then
        val(n,lun) = bmiss
      else
        val(n,lun) = rval
      endif
      ibit = ibit + 8*linc*msub(lun)
    endif
  enddo

  return
end subroutine rdcmps

!> Initialize a new BUFR message for output in compressed format
!>
!> @param lun - File ID
!> @param mesg - BUFR message
!> @param subset - Table A mnemonic for type of BUFR message being written
!> @param idate - Date-time stored within Section 1 of BUFR message being written, in format of either YYMMDDHH
!> or YYYYMMDDHH, depending on datelen() value
!> @param nsub - Number of subsets in mesg
!> @param nbyt - Message length:
!>  - On input, contains the length (in bytes) of Section 4, except for the first 4 bytes
!>  - On output, contains the length (in bytes) of the entire BUFR message, up to the point in Section 4 where
!>    compressed data are to be written
!>
!> @author Woollen @date 2002-05-14
subroutine cmsgini(lun,mesg,subset,idate,nsub,nbyt)

  implicit none

  integer, intent(in) :: lun, idate, nsub
  integer, intent(inout) :: nbyt
  integer, intent(out) :: mesg(*)
  integer mtyp, msbt, inod, isub, iret, jdate, mcen, mear, mmon, mday, mour, mmin, mbit, mbyt, len1, len3, i4dy

  character*128 bort_str
  character*8, intent(in) :: subset
  character*4 bufr
  character tab

  data bufr/'BUFR'/

  ! Get the message tag and type, and break up the date which can be either YYMMDDHH or YYYYMMDDHH

  call nemtba(lun,subset,mtyp,msbt,inod)
  call nemtab(lun,subset,isub,tab,iret)
  if(iret.eq.0) then
    write(bort_str,'("BUFRLIB: CMSGINI - TABLE A MESSAGE TYPE MNEMONIC ",A," NOT FOUND IN INTERNAL TABLE D ARRAYS")') subset
    call bort(bort_str)
  endif

  jdate = i4dy(idate)
  mcen = mod(jdate/10**8,100)+1
  mear = mod(jdate/10**6,100)
  mmon = mod(jdate/10**4,100)
  mday = mod(jdate/10**2,100)
  mour = mod(jdate      ,100)
  mmin = 0

  if(mear.eq.0) then
    mcen = mcen-1
    mear = 100
  endif

  ! Initialize the message

  mbit = 0

  ! Section 0

  call pkc(bufr ,  4 , mesg,mbit)
  ! Note that the actual Section 0 length will be computed and stored below; for now, we're really only interested in
  ! advancing mbit by the correct amount, so we'll just store a default value of 0.
  call pkb(   0 , 24 , mesg,mbit)
  call pkb(   3 ,  8 , mesg,mbit)

  ! Section 1

  len1 = 18

  call pkb(len1 , 24 , mesg,mbit)
  call pkb(   0 ,  8 , mesg,mbit)
  call pkb(   3 ,  8 , mesg,mbit)
  call pkb(   7 ,  8 , mesg,mbit)
  call pkb(   0 ,  8 , mesg,mbit)
  call pkb(   0 ,  8 , mesg,mbit)
  call pkb(mtyp ,  8 , mesg,mbit)
  call pkb(msbt ,  8 , mesg,mbit)
  call pkb(  36 ,  8 , mesg,mbit)
  call pkb(   0 ,  8 , mesg,mbit)
  call pkb(mear ,  8 , mesg,mbit)
  call pkb(mmon ,  8 , mesg,mbit)
  call pkb(mday ,  8 , mesg,mbit)
  call pkb(mour ,  8 , mesg,mbit)
  call pkb(mmin ,  8 , mesg,mbit)
  call pkb(mcen ,  8 , mesg,mbit)

  ! Section 3

  len3 = 10

  call pkb(len3 , 24 , mesg,mbit)
  call pkb(   0 ,  8 , mesg,mbit)
  call pkb(nsub , 16 , mesg,mbit)
  call pkb( 192 ,  8 , mesg,mbit)
  call pkb(isub , 16 , mesg,mbit)
  call pkb(   0 ,  8 , mesg,mbit)

  ! Section 4

  ! Store the total length of Section 4.  Remember that the input value of nbyt only contains the length of the compressed
  ! data portion of Section 4, so we need to add four bytes to this number in order to account for the total length of
  ! Section 4.  The actual compressed data portion will be filled in later by subroutine wrcmps().
  call pkb((nbyt+4) , 24 , mesg,mbit)
  call pkb(       0 ,  8 , mesg,mbit)

  ! Section 5

  ! This section will be filled in later by subroutine wrcmps().  However, for now, and noting that mbit currently points
  ! to the last bit of the fourth byte of Section 4, then we have:
  !   (total length of BUFR message (in Section 0)) =
  !         (length of message up through fourth byte of Section 4)
  !      +  (length of compressed data portion of Section 4)
  !      +  (length of Section 5)
  mbyt = mbit/8 + nbyt + 4

  ! For output, make nbyt point to the current location of mbit, which is the byte after which to actually begin writing the
  ! compressed data into Section 4.
  nbyt = mbit/8

  ! Now, store the total length of the BUFR message in Section 0.
  mbit = 32
  call pkb(mbyt,24,mesg,mbit)

  return
end subroutine cmsgini
