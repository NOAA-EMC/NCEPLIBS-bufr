!> @file
!> @brief Build the internal jump/link table.
!>
!> @author J. Woollen @date 1994-01-06

!> Build the entire internal jump/link table within module @ref moda_tables,
!> using all of the internal BUFR table array information
!> from module @ref moda_tababd for all of the file IDs that are
!> currently defined to the library in module @ref moda_stbfr.
!>
!> The entire jump/link table will always be completely reconstructed
!> from scratch, even if some of the information within the internal
!> BUFR table arrays already existed there at the time of the previous
!> call to this subroutine, because there may have been other events
!> that have taken place since the previous call to this subroutine and
!> which haven't yet been reflected within the internal jump/link table.
!> For example, a file ID may have recently been unlinked from the
!> library via an internal call to subroutine closbf(), or the DX BUFR
!> tables associated with a file ID may have changed.
!>
!> @author J. Woollen @date 1994-01-06
subroutine makestab

  use modv_vars, only: bmiss, maxjl, nfiles

  use moda_usrint
  use moda_stbfr
  use moda_lushr
  use moda_xtab
  use moda_tababd
  use moda_tables
  use moda_nrv203
  use moda_bitmaps

  implicit none

  integer iprt, lunit, lundx, lun, lum, k, n, itba, inc, newn, noda, node, inod, icmpdx, ishrdx

  character*128 bort_str, errstr
  character*8 nemo

  logical expand

  common /quiet/ iprt

  ! Reset pointer table and string cache.

  ntab = 0
  nnrv = 0
  ntamc = 0
  call strcln

  ! Figure out which units share tables.

  ! The lus array is static between calls to this subroutine, and it keeps track of which logical units share dictionary
  ! table information:
  !   if lus(i) = 0, then iolun(i) does not share dictionary table information with any other logical unit
  !   if lus(i) > 0, then iolun(i) shares dictionary table information with logical unit iolun(lus(i))
  !   if lus(i) < 0, then iolun(i) does not now, but at one point in the past, shared dictionary table information
  !                  with logical unit iolun(abs(lus(i)))

  ! The xtab array is non-static and is recomputed within the below loop during each call to this subroutine:
  !   if xtab(i) = .true., then the dictionary table information has changed for iolun(i) since the last call to this subroutine
  !   if xtab(i) = .false., then the dictionary table information has not changed for iolun(i) since the last call to this subroutine

  do lun=1,nfiles
    xtab(lun) = .false.
    if(iolun(lun).eq.0) then
      ! Logical unit iolun(lun) is not defined to NCEPLIBS-bufr.
      lus(lun) = 0
    else if(mtab(1,lun).eq.0) then
      ! New dictionary table information has been read for logical unit iolun(lun) since the last call to this subroutine.
      xtab(lun) = .true.
      if(lus(lun).ne.0) then
        if(iolun(abs(lus(lun))).eq.0) then
          lus(lun) = 0
        else if(lus(lun).gt.0) then
          ! iolun(lun) was sharing table information with logical unit iolun(lus(lun)), so check whether the table information
          ! has really changed.  If not, then iolun(lun) just re-read a copy of the exact same table information as before,
          ! and therefore it can continue to share with logical unit iolun(lus(lun)).
          if(icmpdx(lus(lun),lun).eq.1) then
            xtab(lun) = .false.
            call cpbfdx(lus(lun),lun)
          else
            lus(lun) = (-1)*lus(lun)
          endif
        else if(icmpdx(abs(lus(lun)),lun).eq.1) then
          ! iolun(lun) was not sharing table information with logical unit iolun(lus(lun)), but it did at one point in the past
          ! and now once again has the same table information as that logical unit.  Since the two units shared table
          ! information at one point in the past, allow them to do so again.
          xtab(lun) = .false.
          lus(lun) = abs(lus(lun))
          call cpbfdx(lus(lun),lun)
        endif
      endif
    else if(lus(lun).gt.0) then
      ! Logical unit iolun(lun) is sharing table information with logical unit iolun(lus(lun)), so make sure that the latter
      ! unit is still defined to NCEPLIBS-bufr.
      if(iolun(lus(lun)).eq.0) then
        lus(lun) = 0
      else if( xtab(lus(lun)) .and. (icmpdx(lus(lun),lun).eq.0) ) then
        ! The table information for logical unit iolun(lus(lun)) just changed (in midstream).  If iolun(lun) is an output
        ! file, then we will have to update it with the new table information later on in this subroutine.  Otherwise,
        ! iolun(lun) is an input file and is no longer sharing tables with iolun(lus(lun)).
        if(iolun(lun).lt.0) lus(lun) = (-1)*lus(lun)
      endif
    else
      ! Determine whether logical unit iolun(lun) is sharing table information with any other logical units.
      lum = 1
      do while ((lum.lt.lun).and.(lus(lun).eq.0))
        if(ishrdx(lum,lun).eq.1) then
          lus(lun) = lum
        else
          lum = lum+1
        endif
      enddo
    endif
  enddo

  ! Initialize jump/link tables with subsets/sequences/elements.

  do lun=1,nfiles
    if(iolun(lun).ne.0 .and. ntba(lun).gt.0) then
      ! Reset any existing inventory pointers.
      if(iomsg(lun).ne.0) then
        if(lus(lun).le.0) then
          inc = (ntab+1)-mtab(1,lun)
        else
          inc = mtab(1,lus(lun))-mtab(1,lun)
        endif
        do n=1,nval(lun)
          inv(n,lun) = inv(n,lun)+inc
        enddo
      endif
      if(lus(lun).le.0) then
        ! The dictionary table information corresponding to logical unit iolun(lun) has not yet been written into the internal
        ! jump/link table, so add it in now.
        call chekstab(lun)
        do itba=1,ntba(lun)
          inod = ntab+1
          nemo = taba(itba,lun)(4:11)
          call tabsub(lun,nemo)
          mtab(itba,lun) = inod
          isc(inod) = ntab
        enddo
      else if( xtab(lus(lun)) .and. (icmpdx(lus(lun),lun).eq.0) ) then
        ! Logical unit iolun(lun) is an output file that is sharing table information with logical unit iolun(lus(lun)) whose
        ! table just changed (in midstream).  Flush any existing data messages from iolun(lun), then update the table information
        ! for this logical unit with the corresponding new table information from iolun(lus(lun)), then update iolun(lun) itself
        ! with a copy of the new table information.
        lunit = abs(iolun(lun))
        if(iomsg(lun).ne.0) call closmg(lunit)
        call cpbfdx(lus(lun),lun)
        lundx = abs(iolun(lus(lun)))
        call wrdxtb(lundx,lunit)
      endif
    endif
  enddo

  ! Store types and initial values and counts

  do node=1,ntab
    if(typ(node).eq.'SUB') then
      vali(node) = 0
      knti(node) = 1
      itp (node) = 0
    elseif(typ(node).eq.'SEQ') then
      vali(node) = 0
      knti(node) = 1
      itp (node) = 0
    elseif(typ(node).eq.'RPC') then
      vali(node) = 0
      knti(node) = 0
      itp (node) = 0
    elseif(typ(node).eq.'RPS') then
      vali(node) = 0
      knti(node) = 0
      itp (node) = 0
    elseif(typ(node).eq.'REP') then
      vali(node) = bmiss
      knti(node) = irf(node)
      itp (node) = 0
    elseif(typ(node).eq.'DRS') then
      vali(node) = 0
      knti(node) = 1
      itp (node) = 1
    elseif(typ(node).eq.'DRP') then
      vali(node) = 0
      knti(node) = 1
      itp (node) = 1
    elseif(typ(node).eq.'DRB') then
      vali(node) = 0
      knti(node) = 0
      itp (node) = 1
    elseif(typ(node).eq.'NUM') then
      vali(node) = bmiss
      knti(node) = 1
      itp (node) = 2
    else ! typ(node).eq.'CHR'
      vali(node) = bmiss
      knti(node) = 1
      itp (nodE) = 3
    endif
  enddo

  ! Set up expansion segments for type 'SUB', 'DRP', and 'DRS' nodes.

  newn = 0

  do n=1,ntab
    iseq(n,1) = 0
    iseq(n,2) = 0
    expand = typ(n).eq.'SUB' .or. typ(n).eq.'DRP' .or. typ(n).eq.'DRS' .or. typ(n).eq.'REP' .or. typ(n).eq.'DRB'
    if(expand) then
      iseq(n,1) = newn+1
      noda = n
      node = n+1
      do k=1,maxjl
        knt(k) = 0
      enddo
      if(typ(noda).eq.'REP') then
        knt(node) = knti(noda)
      else
        knt(node) = 1
      endif

      outer: do while (.true.)
        newn = newn+1
        if(newn.gt.maxjl) then
          write(bort_str,'("BUFRLIB: MAKESTAB - NUMBER OF JSEQ ENTRIES IN JUMP/LINK TABLE EXCEEDS THE LIMIT (",I6,")")') maxjl
          call bort(bort_str)
        endif
        jseq(newn) = node
        knt(node) = max(knti(node),knt(node))
        inner: do while (.true.)
          if(jump(node)*knt(node).gt.0) then
            node = jump(node)
            cycle outer
          else if(link(node).gt.0) then
            node = link(node)
            cycle outer
          else
            node = jmpb(node)
            if(node.eq.noda) exit outer
            if(node.eq.0) then
              write(bort_str,'("BUFRLIB: MAKESTAB - NODE IS ZERO, FAILED TO CIRCULATE (TAG IS ",A,")")') tag(n)
              call bort(bort_str)
            endif
            knt(node) = max(knt(node)-1,0)
          endif
        enddo inner
      enddo outer
      iseq(n,2) = newn
    endif
  enddo

  ! Print the sequence tables

  if(iprt.ge.2) then
    call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      do n=1,ntab
        write ( unit=errstr, fmt='(A,I5,2X,A10,A5,6I8)' ) &
          'BUFRLIB: MAKESTAB ', n, tag(n), typ(n), jmpb(n), jump(n), link(n), ibt(n), irf(n), isc(n)
        call errwrt(errstr)
      enddo
    call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
    call errwrt(' ')
  endif

  return
end subroutine makestab

!> Check that an internal BUFR table representation is self-consistent and fully defined.  If any errors
!> are found, then make an appropriate call to NCEPLIBS-bufr subroutine bort().
!>
!> @param lun - File ID
!>
!> @author Woollen @date 1994-01-06
subroutine chekstab(lun)

  use moda_tababd
  use moda_nmikrp

  implicit none

  integer, intent(in) :: lun
  integer itab, idn, iret, iscl, iref, ibit, nseq

  character*128 bort_str
  character*24 unit
  character*8 nemo
  character*1 tab

  ! There must be entries in Tables A, B, and D

  if(ntba(lun).eq.0) call bort ('BUFRLIB: CHEKSTAB - EMPTY TABLE A IN INTERNAL BUFR TABLES')
  if(ntbb(lun).eq.0) call bort ('BUFRLIB: CHEKSTAB - EMPTY TABLE B IN INTERNAL BUFR TABLES')
  if(ntbd(lun).eq.0) call bort ('BUFRLIB: CHEKSTAB - EMPTY TABLE D IN INTERNAL BUFR TABLES')

  ! Make sure each Table A entry is defined as a sequence

  do itab=1,ntba(lun)
    nemo = taba(itab,lun)(4:11)
    call nemtab(lun,nemo,idn,tab,iret)
    if(tab.ne.'D') then
      write(bort_str,'("BUFRLIB: CHEKSTAB - TABLE A ENTRY: ",A," NOT DEFINED AS A SEQUENCE")') nemo
      call bort(bort_str)
    endif
  enddo

  ! Check Table B contents

  do itab=1,ntbb(lun)
    call nemtbb(lun,itab,unit,iscl,iref,ibit)
  enddo

  ! Check Table D contents

  do itab=1,ntbd(lun)
    call nemtbd(lun,itab,nseq,nem(1,1),irp(1,1),krp(1,1))
  enddo

  return
end subroutine chekstab

!> Build and store the entire jump/link tree (including recursively resolving all "child" mnemonics) for a Table A mnemonic
!> within the internal jump/link table.
!>
!> @param lun - File ID
!> @param nemo - Table A mnemonic
!>
!> @author Woollen @date 1994-01-06
subroutine tabsub(lun,nemo)

  use modv_vars, only: mxtamc, mxtco

  use moda_tables
  use moda_nmikrp
  use moda_nrv203
  use moda_bitmaps

  implicit none

  integer, intent(in) :: lun
  integer jmp0(10), nodl(10), ntag(10,2), icdw, icsc, icrv, incw, maxlim, node, idn, itab, nseq, limb, n, jj, iyyy, &
    irep, iknt, jum0, iokoper

  character*128 bort_str
  character*8, intent(in) :: nemo
  character*8 nems
  character*1 tab

  logical drop(10), ltamc

  common /tabccc/ icdw, icsc, icrv, incw

  data maxlim /10/

  ! Check the mnemonic

  ! Note that Table A mnemonics, in addition to being stored within internal BUFR Table A array taba(*,lun), are also stored as
  ! Table D mnemonics within internal BUFR Table D array tabd(*,lun).  So the following test is valid.

  call nemtab(lun,nemo,idn,tab,itab)
  if(tab.ne.'D') then
    write(bort_str,'("BUFRLIB: TABSUB - SUBSET NODE NOT IN TABLE D (TAB=",A,") FOR INPUT MNEMONIC ",A)') tab,nemo
    call bort(bort_str)
  endif

  ! Store a subset node and jump/link the tree

  call inctab(nemo,'SUB',node)
  jump(node) = node+1
  jmpb(node) = 0
  link(node) = 0
  ibt (node) = 0
  irf (node) = 0
  isc (node) = 0

  call nemtbd(lun,itab,nseq,nem(1,1),irp(1,1),krp(1,1))
  ntag(1,1) = 1
  ntag(1,2) = nseq
  jmp0(1) = node
  nodl(1) = node
  limb = 1

  icdw = 0
  icsc = 0
  icrv = 1
  incw = 0

  ibtnrv = 0
  ipfnrv = 0

  if(ntamc+1.gt.mxtamc) call bort('BUFRLIB: TABSUB - MXTAMC OVERFLOW')
  inodtamc(ntamc+1) = node
  ntco(ntamc+1) = 0
  ltamc = .false.

  ! The following loop resolves all entities in the subset by emulating recursion via explicit goto statements.

  11 do n=ntag(limb,1),ntag(limb,2)

    ntag(limb,1) = n+1
    drop(limb) = n.eq.ntag(limb,2)

    call nemtab(lun,nem(n,limb),idn,tab,itab)
    nems = nem(n,limb)

    if(tab.eq.'C') then
      ! Special treatment for certain operator descriptors.
      read(nems,'(3X,I3)') iyyy
      if(itab.eq.1) then
        if(iyyy.ne.0) then
          if(icdw.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE DATA WIDTH OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          icdw = iyyy-128
        else
          icdw = 0
        endif
      elseif(itab.eq.2) then
        if(iyyy.ne.0) then
          if(icsc.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE DATA SCALE OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          icsc = iyyy-128
        else
          icsc = 0
        endif
      elseif(itab.eq.3) then
        if(iyyy.eq.0) then
          ! Stop applying new reference values to subset nodes.  Instead, revert to the use of standard Table B values.
          if(ipfnrv.eq.0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-03-000 OPERATOR WAS '// &
              'ENCOUNTERED WITHOUT ANY PRIOR 2-03-YYY OPERATOR FOR INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          do jj=ipfnrv,nnrv
            ienrv(jj) = ntab
          enddo
          ipfnrv = 0
        elseif(iyyy.eq.255) then
          ! End the definition of new reference values.
          ibtnrv = 0
        else
          ! Begin the definition of new reference values.
          if(ibtnrv.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE REF VALUE OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          ibtnrv = iyyy
        endif
      elseif(itab.eq.7) then
        if(iyyy.gt.0) then
          if(icdw.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE DATA WIDTH OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(icsc.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE DATA SCALE OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          icdw = ((10*iyyy)+2)/3
          icsc = iyyy
          icrv = 10**iyyy
        else
          icsc = 0
          icdw = 0
          icrv = 1
        endif
      elseif(itab.eq.8) then
        incw = iyyy
      elseif((itab.ge.21).and.(iokoper(nems).eq.1)) then
        ! Save the location of this operator within the jump/link table, for possible later use.
        if(.not.ltamc) then
          ltamc = .true.
          ntamc = ntamc+1
        end if
        if(ntco(ntamc)+1.gt.mxtco) call bort('BUFRLIB: TABSUB - MXTCO OVERFLOW')
        ntco(ntamc) = ntco(ntamc)+1
        ctco(ntamc,ntco(ntamc)) = nems(1:6)
        inodtco(ntamc,ntco(ntamc)) = ntab
      endif
    else
      nodl(limb) = ntab+1
      irep = irp(n,limb)
      iknt = krp(n,limb)
      jum0 = jmp0(limb)
      call tabent(lun,nems,tab,itab,irep,iknt,jum0)
    endif

    if(tab.eq.'D') then
      ! Note here how a new tree "limb" is created (and is then immediately recursively resolved) whenever a Table D mnemonic
      ! contains another Table D mnemonic as one of its children.
      limb = limb+1
      if(limb.gt.maxlim) then
        write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TOO MANY NESTED '// &
          'TABLE D SEQUENCES (TREES) WITHIN INPUT MNEMONIC ",A," - THE LIMIT IS",I4)') nemo,maxlim
        call bort(bort_str)
      endif
      call nemtbd(lun,itab,nseq,nem(1,limb),irp(1,limb),krp(1,limb))
      ntag(limb,1) = 1
      ntag(limb,2) = nseq
      jmp0(limb) = ntab
      goto 11
    elseif(drop(limb)) then
      do while (.true.)
        link(nodl(limb)) = 0
        limb = limb-1
        if(limb.eq.0) then
          if(icrv.ne.1) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-07-YYY OPERATOR WAS '// &
              'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(icdw.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-01-YYY OPERATOR WAS '// &
              'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(icsc.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-02-YYY OPERATOR WAS '// &
              'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(incw.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-08-YYY OPERATOR WAS '// &
              'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(ibtnrv.ne.0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-03-YYY OPERATOR WAS '// &
              'APPLIED WITHOUT ANY SUBSEQUENT 2-03-255 OPERATOR FOR INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(ipfnrv.ne.0) then
            ! One or more new reference values were defined for this subset, but there was no subsequent 2-03-000 operator,
            ! so set all IENRV(*) values for this subset to point to the last element of the subset within the jump/link table.
            ! Note that, if there had been a subsequent 2-03-000 operator, then these IENRV(*) values would have already been
            ! properly set above.
            do jj=ipfnrv,nnrv
              ienrv(jj) = ntab
            enddo
          endif
          return
        endif
        if(.not.drop(limb)) exit
      enddo
      link(nodl(limb)) = ntab+1
      goto 11
    elseif(tab.ne.'C') then
      link(nodl(limb)) = ntab+1
    endif

  enddo

  write(bort_str,'("BUFRLIB: TABSUB - ENTITIES WERE NOT SUCCESSFULLY RESOLVED (BY EMULATING RESURSION) FOR SUBSET '// &
    'DEFINED BY TBL A MNEM. ",A)') nemo
  call bort(bort_str)

  return
end subroutine tabsub

!> Build and store an entry for a Table B or Table D mnemonic within the internal jump/link table.
!>
!> @param lun - File ID
!> @param nemo - Table B or D mnemonic to store in jump/link table
!> @param tab - Internal BUFR table array ('B' or 'D') in which nemo is defined
!> @param itab - Positional index of nemo within TAB
!> @param irep - Positional index within common /reptab/ arrays, for use when nemo is replicated:
!>   - 0, if nemo is not replicated
!> @param iknt - Number of replications, for use when nemo is replicated using F=1 regular (i.e., non-delayed) replication:
!>   - 0, if nemo is not replicated using F=1 regular (i.e., non-delayed) replication
!> @param jum0 - Index value to be stored for nemo within internal jump/link table array jmpb(*)
!>
!> @author Woollen @date 1994-01-06
subroutine tabent(lun,nemo,tab,itab,irep,iknt,jum0)

  use modv_vars, only: mxnrv

  use moda_tables
  use moda_nrv203

  implicit none

  integer, intent(in) :: lun, itab, irep, iknt, jum0
  integer idnr, lens, icdw, icsc, icrv, incw, i, jm0, node, iscl, iref, ibit

  character*24 unit
  character*10 rtag
  character*8, intent(in) :: nemo
  character, intent(in) :: tab
  character*3 typs, typt
  character reps

  common /reptab/ idnr(5,2), typs(5,2), reps(5,2), lens(5)

  common /tabccc/ icdw, icsc, icrv, incw

  jm0 = jum0

  ! Make a jump/link table entry for a replicator

  if(irep.ne.0) then
    rtag = reps(irep,1)//nemo
    do i=1,10
      if(rtag(i:i).eq.' ') then
        rtag(i:i) = reps(irep,2)
        call inctab(rtag,typs(irep,1),node)
        jump(node) = node+1
        jmpb(node) = jm0
        link(node) = 0
        ibt (node) = lens(irep)
        irf (node) = 0
        isc (node) = 0
        if(irep.eq.1) irf(node) = iknt
        jm0 = node
        exit
      endif
    enddo
  endif

  ! Make a jump/link entry for an element or a sequence

  if(tab.eq.'B') then
    call nemtbb(lun,itab,unit,iscl,iref,ibit)
    if(unit(1:5).eq.'CCITT') then
      typt = 'CHR'
    else
      typt = 'NUM'
    endif
    call inctab(nemo,typt,node)
    jump(node) = 0
    jmpb(node) = jm0
    link(node) = 0
    ibt (node) = ibit
    irf (node) = iref
    isc (node) = iscl
    if(unit(1:4).eq.'CODE') then
      typt = 'COD'
    elseif(unit(1:4).eq.'FLAG') then
      typt = 'FLG'
    endif
    if( (typt.eq.'NUM') .and. (ibtnrv.ne.0) ) then
      ! This node contains a new (redefined) reference value.
      if(nnrv+1.gt.mxnrv) call bort('BUFRLIB: TABENT - MXNRV OVERFLOW')
      nnrv = nnrv+1
      tagnrv(nnrv) = nemo
      inodnrv(nnrv) = node
      isnrv(nnrv) = node+1
      ibt(node) = ibtnrv
      if(ipfnrv.eq.0) ipfnrv = nnrv
    elseif( (typt.eq.'NUM') .and. (nemo(1:3).ne.'204') ) then
      ibt(node) = ibt(node) + icdw
      isc(node) = isc(node) + icsc
      irf(node) = irf(node) * icrv
    elseif( (typt.eq.'CHR') .and. (incw.gt.0) ) then
      ibt(node) = incw * 8
    endif
  else ! tab.eq.'D'
    if(irep.eq.0) then
      typt = 'SEQ'
    else
      typt = typs(irep,2)
    endif
    call inctab(nemo,typt,node)
    jump(node) = node+1
    jmpb(node) = jm0
    link(node) = 0
    ibt (node) = 0
    irf (node) = 0
    isc (node) = 0
  endif

  return
end subroutine tabent

!> Get the next available positional index
!> for writing into the internal jump/link table in module @ref moda_tables,
!> and also use that index to store atag and atyp within,
!> respectively, the internal jump/link table arrays tag(*) and typ(*).
!> If there is no more room for additional entries within the internal
!> jump/link table, then an appropriate call is made to subroutine bort().
!>
!> @param atag - Mnemonic name
!> @param atyp - Mnemonic type
!> @param node - Next available positional index for writing into the internal jump/link table
!>
!> @author Woollen @date 1994-01-06
subroutine inctab(atag,atyp,node)

  use modv_vars, only: maxjl

  use moda_tables

  implicit none

  integer, intent(out) :: node

  character*(*), intent(in) :: atag, atyp
  character*128 bort_str

  ntab = ntab+1
  if(ntab.gt.maxjl) then
    write(bort_str,'("BUFRLIB: INCTAB - THE NUMBER OF JUMP/LINK TABLE ENTRIES EXCEEDS THE LIMIT, MAXJL (",I7,")")') maxjl
    call bort(bort_str)
  endif
  tag(ntab) = atag
  typ(ntab) = atyp
  node = ntab

  return
end subroutine inctab

!> Search backwards, beginning from a given
!> node within the jump/link table, until finding the most recent
!> node of type jbtyp.  The internal jmpb array is used to jump
!> backwards within the jump/link table, and the function returns
!> the table index of the found node.  If the input node itself is
!> of type jbtyp, then the function simply returns the index of that
!> same node.
!>
!> @note See module @ref moda_tables for an
!> explanation of the various node types present within an internal
!> jump/link table.
!>
!> @param node - Jump/link table index of entry to begin searching backwards from
!> @param lun - File ID
!> @param jbtyp - Type of node for which to search
!>
!> @return - Index of first node of type jbtyp found by jumping backwards from input node
!>  - 0 no such node found
!>
!> @author Woollen @date 1994-01-06
integer function lstjpb(node,lun,jbtyp) result(iret)

  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: node, lun
  integer nod

  character*(*), intent(in) :: jbtyp
  character*128 bort_str

  if(node.lt.inode(lun)) then
    write(bort_str,'("BUFRLIB: LSTJPB - TABLE NODE (",I7,") IS OUT OF BOUNDS, < LOWER BOUNDS (",I7,")")') node,inode(lun)
    call bort(bort_str)
  endif
  if(node.gt.isc(inode(lun))) then
    write(bort_str,'("BUFRLIB: LSTJPB - TABLE NODE (",I7,") IS OUT OF BOUNDS, > UPPER BOUNDS (",I7,")")') node,isc(inode(lun))
    call bort(bort_str)
  endif

  nod = node

  !  Find this or the previous node of type jbtyp

  do while (nod.ne.0)
    if(typ(nod).eq.jbtyp) exit
    nod = jmpb(nod)
  enddo

  iret = nod

  return
end function lstjpb

!> Check whether the same [DX BUFR Table](@ref dfbftab) is being shared between two Fortran logical units.
!>
!> @param lud - File ID for first BUFR file
!> @param lun - File ID for second BUFR file
!> @returns ishrdx - Flag indicating whether the same DX BUFR table is being shared between the
!> BUFR file associated with lud and the BUFR file associated with lun
!>    - 0 = No
!>    - 1 = Yes
!>
!> @author J. Ator @date 2009-06-18
integer function ishrdx(lud,lun) result(iret)

  use moda_tababd

  implicit none

  integer, intent(in) :: lud, lun
  integer ii

  ! Note that, for any file ID luX, the mtab(*,luX) array contains pointer indices into the internal jump/link table
  ! for each of the Table A mnemonics that is currently defined for that luX value.  Thus, if all of these indices are
  ! identical for two different luX values, then the associated logical units are sharing table information.

  if ( ( ntba(lud) .ge. 1 ) .and. ( ntba(lud) .eq. ntba(lun) ) ) then
    ii = 1
    iret = 1
    do while ( ( ii .le. ntba(lud) ) .and. ( iret .eq. 1 ) )
      if ( ( mtab(ii,lud) .ne. 0 ) .and. ( mtab(ii,lud) .eq. mtab(ii,lun) ) ) then
        ii = ii + 1
      else
        iret = 0
      endif
    enddo
  else
    iret = 0
  endif

  return
end function ishrdx

!> Check whether the full set of associated [DX BUFR Table information](@ref dfbftab) is identical between
!> two Fortran logical units.
!>
!> Note that two different logical units can have identical DX BUFR
!> Table information associated with them even if they aren't actually
!> sharing the same DX BUFR table.
!>
!> @param lud - File ID for first BUFR file
!> @param lun - File ID for second BUFR file
!> @returns icmpdx - Flag indicating whether the BUFR file associated with lud and the BUFR
!> file associated with lun have the same DX BUFR table information
!>    - 0 = No
!>    - 1 = Yes
!>
!> @author J. Ator @date 2009-06-18
integer function icmpdx(lud,lun) result(iret)

  use moda_tababd

  implicit none

  integer, intent(in) :: lud, lun
  integer ishrdx, i

  ! First, check whether the two units are actually sharing tables.
  ! If so, then they obviously have the same table information.

  iret = ishrdx(lud,lun)
  if ( iret .eq. 1 ) return

  ! Otherwise, check whether the internal Table A, B and D entries are all identical between the two units.

  if ( ( ntba(lud) .eq. 0 ) .or. ( ntba(lun) .ne. ntba(lud) ) ) return
  do i = 1, ntba(lud)
    if ( idna(i,lun,1) .ne. idna(i,lud,1) ) return
    if ( idna(i,lun,2) .ne. idna(i,lud,2) ) return
    if ( taba(i,lun) .ne. taba(i,lud) ) return
  enddo

  if ( ( ntbb(lud) .eq. 0 ) .or. ( ntbb(lun) .ne. ntbb(lud) ) ) return
  do i = 1, ntbb(lud)
    if ( idnb(i,lun) .ne. idnb(i,lud) ) return
    if ( tabb(i,lun) .ne. tabb(i,lud) ) return
  enddo

  if ( ( ntbd(lud) .eq. 0 ) .or. ( ntbd(lun) .ne. ntbd(lud) ) ) return
  do i = 1, ntbd(lud)
    if ( idnd(i,lun) .ne. idnd(i,lud) ) return
    if ( tabd(i,lun) .ne. tabd(i,lud) ) return
  enddo

  iret = 1

  return
end function icmpdx

!> Search for a specified mnemonic within unexpanded sequences of the internal jump/link table.
!>
!> This subroutine is called internally by subroutine
!> ufbrw() whenever it can't find a mnemonic it wants to write within the
!> current subset buffer. It looks for the mnemonic within any
!> unexpanded "DRS" (stack) or "DRB" (1-bit delayed replication)
!> sequences inside of the portion of the subset buffer bounded by the
!> indices inv1 and inv2. If found, it expands the applicable "DRS" or
!> "DRB" sequence to the point where the mnemonic in question now
!> appears in the subset buffer, and in doing so it will also return
!> a new value for inv2.
!>
!> @param inod - Jump/link table index of mnemonic to look for
!> @param lun - File ID
!> @param inv1 - Starting index of the portion of the subset buffer currently being processed by ufbrw()
!> @param inv2 - Ending index
!>  - On input, ending index of the portion of the subset buffer currently being processed by ufbrw()
!>  - On output, if invn = 0 then inv2 is unchanged from its input value.
!>    Otherwise, it contains the redefined ending index of the portion of the subset
!>    buffer currently being processed by ufbrw(), since expanding a delayed
!>    replication sequence will have necessarily increased the size of this buffer.
!> @param invn - Location index of inod within subset buffer
!>  - 0 = Not found
!>
!> @author Woollen @date 1994-01-06
subroutine drstpl(inod,lun,inv1,inv2,invn)

  use moda_tables

  implicit none

  integer, intent(in) :: inod, lun, inv1
  integer, intent(inout) :: inv2
  integer, intent(out) :: invn
  integer node, invwin

  do while (.true.)
    node = inod
    do while (.true.)
      node = jmpb(node)
      if(node.eq.0) return
      if(typ(node).eq.'DRS' .or. typ(node).eq.'DRB') then
        invn = invwin(node,lun,inv1,inv2)
        if(invn.gt.0) then
          call usrtpl(lun,invn,1)
          call newwin(lun,inv1,inv2)
          invn = invwin(inod,lun,invn,inv2)
          if(invn.gt.0) return
          exit
        endif
      endif
    enddo
  enddo

  return
end subroutine drstpl
