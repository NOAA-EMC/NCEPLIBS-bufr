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
!> @param[in] lun - File ID
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

!> Write a compressed BUFR data subset.
!>
!> Pack up the current subset within memory
!> (array ibay in module @ref moda_bitbuf), storing it for compression.
!> Then, try to add it to the compressed BUFR message that is
!> currently open within memory for abs(lunix). If the
!> subset will not fit into the currently open message, then that
!> compressed message is flushed to lunix and a new one is created in
!> order to hold the current subset (still stored for compression).
!>
!> This subroutine performs functions similar to NCEPLIBS-bufr
!> subroutine msgupd() except that it acts on compressed bufr messages.
!>
!> @param lunix - Absolute value is Fortran logical unit number for BUFR file
!> - if lunix is less than zero, then this is a "flush" call and the buffer must be cleared out
!>
!> @author Woollen @date 2002-05-14
subroutine wrcmps(lunix)

  use modv_vars, only: mxcdv, mxcsb

  use moda_usrint
  use moda_msgcwd
  use moda_bitbuf
  use moda_mgwa
  use moda_tables
  use moda_comprx
  use moda_comprs
  use moda_s01cm

  implicit none

  integer, intent(in) :: lunix
  integer ibyt, jbit, lunit, lun, il, im, icol, i, j, node, lbyt, nbyt, nchr, ldata, iupbs01

  character*128 bort_str
  character*8 subset
  character czero

  logical first, kmiss, edge4, msgfull, cmpres

  real, parameter :: rln2 = 1./log(2.)
  real range

  data first /.true./

  save first, ibyt, jbit, subset, edge4

  ! Get the unit and subset tag

  lunit = abs(lunix)
  call status(lunit,lun,il,im)

  do while (.true.)

    if(first) then
      ! Initialize some values in order to prepare for the creation of a new compressed BUFR message for output.
      kbyt = 0
      ncol = 0
      lunc = lun
      nrow = nval(lun)
      subset = tag(inode(lun))(1:8)
      first = .false.
      flush = .false.
      writ1 = .false.
      ! The following call to cmsgini() is just being done to determine how many bytes (kbyt) will be taken up in a message
      ! by the information in Sections 0, 1, 2 and 3.  This in turn will allow us to determine how many compressed data subsets
      ! will fit into Section 4 without overflowing maxbyt.  Then, later on, another separate call to cmsgini() will be done to
      ! actually initialize Sections 0, 1, 2 and 3 of the final compressed BUFR message that will be written out.
      call cmsgini(lun,mbay(1,lun),subset,idate(lun),ncol,kbyt)
      ! Check the edition number of the BUFR message to be created
      edge4 = .false.
      if(ns01v.gt.0) then
        i = 1
        do while ( (.not.edge4) .and. (i.le.ns01v) )
          if( (cmnem(i).eq.'BEN') .and. (ivmnem(i).ge.4) ) then
            edge4 = .true.
          else
            i = i+1
          endif
        enddo
      endif
    endif

    if(lun.ne.lunc) then
      write(bort_str,'("BUFRLIB: WRCMPS - FILE ID FOR THIS CALL (",I3,") .NE. FILE ID FOR INITIAL CALL (",I3,")'// &
        ' - UNIT NUMBER NOW IS",I4)') lun,lunc,lunix
      call bort(bort_str)
    endif

    cmpres = .true.
    if(lunix.lt.0) then
      ! This is a "flush" call, so clear out the buffer (note that there is no current subset to be stored!) and prepare
      ! to write the final compressed BUFR message.
      if(ncol.le.0) return
      flush = .true.
      writ1 = .true.
      icol = 1
    elseif(ncol+1.gt.mxcsb) then
      ! There's no more room in the internal compression arrays for another subset, so we'll need to write out a message
      ! containing all of the data in those arrays, then initialize a new message to hold the current subset.
      cmpres = .false.
    else
      ! Check on some other possibly problematic situations
      if(nval(lun).ne.nrow) then
        writ1 = .true.
        icol = 1
      elseif(nval(lun).gt.mxcdv) then
        write(bort_str,'("BUFRLIB: WRCMPS - NO. OF ELEMENTS IN THE '// &
          'SUBSET (",I6,") .GT. THE NO. OF ROWS ALLOCATED FOR THE COMPRESSION MATRIX (",I6,")")') nval(lun),mxcdv
        call bort(bort_str)
      elseif(ncol.gt.0) then
        ! Confirm that all of the nodes are the same as in the previous subset for this same BUFR message.  If not, then
        ! there may be different nested replication sequences activated in the current subset vs. in the previous subset,
        ! even though the total number of nodes is the same.
        do i = 1, nval(lun)
          if ( inv(i,lun) .ne. jlnode(i) ) then
            writ1 = .true.
            icol = 1
            exit
          endif
        enddo
      endif
      if(.not.writ1) then
        ! Store the next subset for compression
        ncol = ncol+1
        icol = ncol
        ibit = 16
        do i=1,nval(lun)
          node = inv(i,lun)
          jlnode(i) = node
          ityp(i) = itp(node)
          iwid(i) = ibt(node)
          if(ityp(i).eq.1.or.ityp(i).eq.2) then
            call up8(matx(i,ncol),ibt(node),ibay,ibit)
          elseif(ityp(i).eq.3) then
            call upc(catx(i,ncol),ibt(node)/8,ibay,ibit,.true.)
          endif
        enddo
      endif
    endif

    ! Will the next subset fit into the current message?  The only way to find out is to actually re-do the compression
    ! by re-computing all of the local reference values, increments, etc. to determine the new Section 4 length.

    do while (cmpres)
      if(ncol.le.0) then
        write(bort_str,'("BUFRLIB: WRCMPS - NO. OF COLUMNS CALCULATED '// &
          'FOR COMPRESSION MAXRIX IS .LE. 0 (=",I6,")")') ncol
        call bort(bort_str)
      endif
      ! ldata will hold the length (in bits) of the compressed data, i.e. the sum total for all data values for all data
      ! subsets in the message
      ldata = 0
      do i=1,nrow
        if(ityp(i).eq.1 .or. ityp(i).eq.2) then
          ! Row i of the compression matrix contains numeric values, so kmis(i) will store .true. if any such values are
          ! "missing", or .false. otherwise
          imiss = 2_8**iwid(i)-1
          if(icol.eq.1) then
            kmin(i) = imiss
            kmax(i) = 0
            kmis(i) = .false.
          endif
          do j=icol,ncol
            if(matx(i,j).lt.imiss) then
              kmin(i) = min(kmin(i),matx(i,j))
              kmax(i) = max(kmax(i),matx(i,j))
            else
              kmis(i) = .true.
            endif
          enddo
          kmiss = kmis(i) .and. kmin(i).lt.imiss
          range = real(max(1,kmax(i)-kmin(i)+1))
          if(ityp(i).eq.2 .and. (range.gt.1. .or. kmiss)) then
            ! The data values in row i of the compression matrix are numeric values that aren't all identical.  Compute the
            ! number of bits needed to hold the largest of the increments.
            kbit(i) = nint(log(range)*rln2)
            if(2**kbit(i)-1.le.range) kbit(i) = kbit(i)+1
            ! However, under no circumstances should this number ever exceed the width of the original underlying descriptor!
            if(kbit(i).gt.iwid(i)) kbit(i) = iwid(i)
          else
            ! The data values in row i of the compression matrix are numeric values that are all identical, so the increments
            ! will be omitted from the message.
            kbit(i) = 0
          endif
          ldata = ldata + iwid(i) + 6 + ncol*kbit(i)
        elseif(ityp(i).eq.3) then
          ! Row i of the compression matrix contains character values, so kmis(i) will store .false. if all such values are
          ! identical, OR .true. otherwise
          if(icol.eq.1) then
            cstr(i) = catx(i,1)
            kmis(i) = .false.
          endif
          do j=icol,ncol
            if ( (.not.kmis(i)) .and. (cstr(i).ne.catx(i,j)) ) then
              kmis(i) = .true.
            endif
          enddo
          if (kmis(i)) then
            ! The data values in row i of the compression matrix are character values that are not all identical
            kbit(i) = iwid(i)
          else
            ! The data values in row i of the compression matrix are character values that are all identical, so the
            ! increments will be omitted from the message
            kbit(i) = 0
          endif
          ldata = ldata + iwid(i) + 6 + ncol*kbit(I)
        endif
      enddo
      ! Round data length up to a whole byte count
      ibyt = (ldata+8-mod(ldata,8))/8
      ! Depending on the edition number of the message, we need to ensure that we round to an even byte count
      if( (.not.edge4) .and. (mod(ibyt,2).ne.0) ) ibyt = ibyt+1
      jbit = ibyt*8-ldata
      if(msgfull(ibyt,kbyt,maxbyt)) then
        ! The current subset will not fit into the current message.  Set the flag to indicate that a message write is needed,
        ! then go back and re-compress the Section 4 data for this message while excluding the data for the current subset,
        ! which will be held and stored as the first subset of a new message after writing the current message.
        writ1 = .true.
        ncol = ncol-1
        icol = 1
      elseif(.not.writ1) then
        ! Add the current subset to the current message and return
        call usrtpl(lun,1,1)
        nsub(lun) = -ncol
        return
      else
        ! Exit the loop and proceed to write out the current message
        cmpres = .false.
      endif
    enddo

    ! Write the complete compressed message.  First, we need to do another call to cmsgini() to initialize Sections 0, 1, 2,
    ! and 3 of the final compressed BUFR message that will be written out.

    call cmsgini(lun,mgwa,subset,idate(lun),ncol,ibyt)

    ! Now add the Section 4 data

    ibit = ibyt*8
    do i=1,nrow
      if(ityp(i).eq.1.or.ityp(i).eq.2) then
        call pkb8(kmin(i),iwid(i),mgwa,ibit)
        call pkb(kbit(i),6,mgwa,ibit)
        if(kbit(i).gt.0) then
          do j=1,ncol
            if(matx(i,j).lt.2_8**iwid(i)-1) then
              incr = matx(i,j)-kmin(i)
            else
              incr = 2_8**kbit(i)-1
            endif
            call pkb8(incr,kbit(i),mgwa,ibit)
          enddo
        endif
      elseif(ityp(i).eq.3) then
        nchr = iwid(i)/8
        if(kbit(i).gt.0) then
          call ipkm(czero,1,0)
          do j=1,nchr
            call pkc(czero,1,mgwa,ibit)
          enddo
          call pkb(nchr,6,mgwa,ibit)
          do j=1,ncol
            call pkc(catx(i,j),nchr,mgwa,ibit)
          enddo
        else
          call pkc(cstr(i),nchr,mgwa,ibit)
          call pkb(0,6,mgwa,ibit)
        endif
      endif
    enddo

    ! Pad the end of Section 4 with zeroes up to the necessary byte count

    call pkb(0,jbit,mgwa,ibit)

    ! Add Section 5

    call pkc('7777',4,mgwa,ibit)

    ! Check that the message byte counters agree, then write the message

    if(mod(ibit,8).ne.0) call bort('BUFRLIB: WRCMPS - THE NUMBER OF BITS IN THE '// &
      'COMPRESSED BUFR MSG IS NOT A MULTIPLE OF 8 - MSG MUST END ON A BYTE BOUNDARY')
    lbyt = iupbs01(mgwa,'LENM')
    nbyt = ibit/8
    if(nbyt.ne.lbyt) then
      write(bort_str,'("BUFRLIB: WRCMPS - OUTPUT MESSAGE LENGTH FROM '// &
       'SECTION 0",I6," DOES NOT EQUAL FINAL PACKED MESSAGE LENGTH (",I6,")")') lbyt,nbyt
      call bort(bort_str)
    endif

    call msgwrt(lunit,mgwa,nbyt)

    ! Now, unless this was a "flush" call to this subroutine, go back and initialize a new message to hold the current subset
    ! that we weren't able to fit into the message that was just written out.

    first = .true.
    if(flush) return
  end do

end subroutine wrcmps
