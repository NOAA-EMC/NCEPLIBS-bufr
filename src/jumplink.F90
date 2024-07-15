!> @file
!> @brief Read or write jump/link table information.
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
    if(iolun(lun)==0) then
      ! Logical unit iolun(lun) is not defined to NCEPLIBS-bufr.
      lus(lun) = 0
    else if(mtab(1,lun)==0) then
      ! New dictionary table information has been read for logical unit iolun(lun) since the last call to this subroutine.
      xtab(lun) = .true.
      if(lus(lun)/=0) then
        if(iolun(abs(lus(lun)))==0) then
          lus(lun) = 0
        else if(lus(lun)>0) then
          ! iolun(lun) was sharing table information with logical unit iolun(lus(lun)), so check whether the table information
          ! has really changed.  If not, then iolun(lun) just re-read a copy of the exact same table information as before,
          ! and therefore it can continue to share with logical unit iolun(lus(lun)).
          if(icmpdx(lus(lun),lun)==1) then
            xtab(lun) = .false.
            call cpbfdx(lus(lun),lun)
          else
            lus(lun) = (-1)*lus(lun)
          endif
        else if(icmpdx(abs(lus(lun)),lun)==1) then
          ! iolun(lun) was not sharing table information with logical unit iolun(lus(lun)), but it did at one point in the past
          ! and now once again has the same table information as that logical unit.  Since the two units shared table
          ! information at one point in the past, allow them to do so again.
          xtab(lun) = .false.
          lus(lun) = abs(lus(lun))
          call cpbfdx(lus(lun),lun)
        endif
      endif
    else if(lus(lun)>0) then
      ! Logical unit iolun(lun) is sharing table information with logical unit iolun(lus(lun)), so make sure that the latter
      ! unit is still defined to NCEPLIBS-bufr.
      if(iolun(lus(lun))==0) then
        lus(lun) = 0
      else if( xtab(lus(lun)) .and. (icmpdx(lus(lun),lun)==0) ) then
        ! The table information for logical unit iolun(lus(lun)) just changed (in midstream).  If iolun(lun) is an output
        ! file, then we will have to update it with the new table information later on in this subroutine.  Otherwise,
        ! iolun(lun) is an input file and is no longer sharing tables with iolun(lus(lun)).
        if(iolun(lun)<0) lus(lun) = (-1)*lus(lun)
      endif
    else
      ! Determine whether logical unit iolun(lun) is sharing table information with any other logical units.
      lum = 1
      do while ((lum<lun).and.(lus(lun)==0))
        if(ishrdx(lum,lun)==1) then
          lus(lun) = lum
        else
          lum = lum+1
        endif
      enddo
    endif
  enddo

  ! Initialize jump/link tables with subsets/sequences/elements.

  do lun=1,nfiles
    if(iolun(lun)/=0 .and. ntba(lun)>0) then
      ! Reset any existing inventory pointers.
      if(iomsg(lun)/=0) then
        if(lus(lun)<=0) then
          inc = (ntab+1)-mtab(1,lun)
        else
          inc = mtab(1,lus(lun))-mtab(1,lun)
        endif
        do n=1,nval(lun)
          inv(n,lun) = inv(n,lun)+inc
        enddo
      endif
      if(lus(lun)<=0) then
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
      else if( xtab(lus(lun)) .and. (icmpdx(lus(lun),lun)==0) ) then
        ! Logical unit iolun(lun) is an output file that is sharing table information with logical unit iolun(lus(lun)) whose
        ! table just changed (in midstream).  Flush any existing data messages from iolun(lun), then update the table information
        ! for this logical unit with the corresponding new table information from iolun(lus(lun)), then update iolun(lun) itself
        ! with a copy of the new table information.
        lunit = abs(iolun(lun))
        if(iomsg(lun)/=0) call closmg(lunit)
        call cpbfdx(lus(lun),lun)
        lundx = abs(iolun(lus(lun)))
        call wrdxtb(lundx,lunit)
      endif
    endif
  enddo

  ! Store types and initial values and counts

  do node=1,ntab
    if(typ(node)=='SUB') then
      vali(node) = 0
      knti(node) = 1
      itp (node) = 0
    elseif(typ(node)=='SEQ') then
      vali(node) = 0
      knti(node) = 1
      itp (node) = 0
    elseif(typ(node)=='RPC') then
      vali(node) = 0
      knti(node) = 0
      itp (node) = 0
    elseif(typ(node)=='RPS') then
      vali(node) = 0
      knti(node) = 0
      itp (node) = 0
    elseif(typ(node)=='REP') then
      vali(node) = bmiss
      knti(node) = irf(node)
      itp (node) = 0
    elseif(typ(node)=='DRS') then
      vali(node) = 0
      knti(node) = 1
      itp (node) = 1
    elseif(typ(node)=='DRP') then
      vali(node) = 0
      knti(node) = 1
      itp (node) = 1
    elseif(typ(node)=='DRB') then
      vali(node) = 0
      knti(node) = 0
      itp (node) = 1
    elseif(typ(node)=='NUM') then
      vali(node) = bmiss
      knti(node) = 1
      itp (node) = 2
    else ! typ(node)=='CHR'
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
    expand = typ(n)=='SUB' .or. typ(n)=='DRP' .or. typ(n)=='DRS' .or. typ(n)=='REP' .or. typ(n)=='DRB'
    if(expand) then
      iseq(n,1) = newn+1
      noda = n
      node = n+1
      do k=1,maxjl
        knt(k) = 0
      enddo
      if(typ(noda)=='REP') then
        knt(node) = knti(noda)
      else
        knt(node) = 1
      endif

      outer: do while (.true.)
        newn = newn+1
        if(newn>maxjl) then
          write(bort_str,'("BUFRLIB: MAKESTAB - NUMBER OF JSEQ ENTRIES IN JUMP/LINK TABLE EXCEEDS THE LIMIT (",I6,")")') maxjl
          call bort(bort_str)
        endif
        jseq(newn) = node
        knt(node) = max(knti(node),knt(node))
        inner: do while (.true.)
          if(jump(node)*knt(node)>0) then
            node = jump(node)
            cycle outer
          else if(link(node)>0) then
            node = link(node)
            cycle outer
          else
            node = jmpb(node)
            if(node==noda) exit outer
            if(node==0) then
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

  if(iprt>=2) then
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

  if(ntba(lun)==0) call bort ('BUFRLIB: CHEKSTAB - EMPTY TABLE A IN INTERNAL BUFR TABLES')
  if(ntbb(lun)==0) call bort ('BUFRLIB: CHEKSTAB - EMPTY TABLE B IN INTERNAL BUFR TABLES')
  if(ntbd(lun)==0) call bort ('BUFRLIB: CHEKSTAB - EMPTY TABLE D IN INTERNAL BUFR TABLES')

  ! Make sure each Table A entry is defined as a sequence

  do itab=1,ntba(lun)
    nemo = taba(itab,lun)(4:11)
    call nemtab(lun,nemo,idn,tab,iret)
    if(tab/='D') then
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
  if(tab/='D') then
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

  if(ntamc+1>mxtamc) call bort('BUFRLIB: TABSUB - MXTAMC OVERFLOW')
  inodtamc(ntamc+1) = node
  ntco(ntamc+1) = 0
  ltamc = .false.

  ! The following loop resolves all entities in the subset by emulating recursion via explicit goto statements.

  11 do n=ntag(limb,1),ntag(limb,2)

    ntag(limb,1) = n+1
    drop(limb) = n==ntag(limb,2)

    call nemtab(lun,nem(n,limb),idn,tab,itab)
    nems = nem(n,limb)

    if(tab=='C') then
      ! Special treatment for certain operator descriptors.
      read(nems,'(3X,I3)') iyyy
      if(itab==1) then
        if(iyyy/=0) then
          if(icdw/=0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE DATA WIDTH OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          icdw = iyyy-128
        else
          icdw = 0
        endif
      elseif(itab==2) then
        if(iyyy/=0) then
          if(icsc/=0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE DATA SCALE OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          icsc = iyyy-128
        else
          icsc = 0
        endif
      elseif(itab==3) then
        if(iyyy==0) then
          ! Stop applying new reference values to subset nodes.  Instead, revert to the use of standard Table B values.
          if(ipfnrv==0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-03-000 OPERATOR WAS '// &
              'ENCOUNTERED WITHOUT ANY PRIOR 2-03-YYY OPERATOR FOR INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          do jj=ipfnrv,nnrv
            ienrv(jj) = ntab
          enddo
          ipfnrv = 0
        elseif(iyyy==255) then
          ! End the definition of new reference values.
          ibtnrv = 0
        else
          ! Begin the definition of new reference values.
          if(ibtnrv/=0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE REF VALUE OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          ibtnrv = iyyy
        endif
      elseif(itab==7) then
        if(iyyy>0) then
          if(icdw/=0) then
            write(bort_str,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '// &
              'CHANGE DATA WIDTH OPERATIONS IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(icsc/=0) then
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
      elseif(itab==8) then
        incw = iyyy
      elseif((itab>=21).and.(iokoper(nems)==1)) then
        ! Save the location of this operator within the jump/link table, for possible later use.
        if(.not.ltamc) then
          ltamc = .true.
          ntamc = ntamc+1
        end if
        if(ntco(ntamc)+1>mxtco) call bort('BUFRLIB: TABSUB - MXTCO OVERFLOW')
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

    if(tab=='D') then
      ! Note here how a new tree "limb" is created (and is then immediately recursively resolved) whenever a Table D mnemonic
      ! contains another Table D mnemonic as one of its children.
      limb = limb+1
      if(limb>maxlim) then
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
        if(limb==0) then
          if(icrv/=1) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-07-YYY OPERATOR WAS '// &
              'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(icdw/=0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-01-YYY OPERATOR WAS '// &
              'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(icsc/=0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-02-YYY OPERATOR WAS '// &
              'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(incw/=0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-08-YYY OPERATOR WAS '// &
              'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(ibtnrv/=0) then
            write(bort_str,'("BUFRLIB: TABSUB - A 2-03-YYY OPERATOR WAS '// &
              'APPLIED WITHOUT ANY SUBSEQUENT 2-03-255 OPERATOR FOR INPUT MNEMONIC ",A)') nemo
            call bort(bort_str)
          endif
          if(ipfnrv/=0) then
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
    elseif(tab/='C') then
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
!> @param irep - Positional index within internal arrays, for use when nemo is replicated:
!>   - 0, if nemo is not replicated
!> @param iknt - Number of replications, for use when nemo is replicated using F=1 regular (i.e., non-delayed) replication:
!>   - 0, if nemo is not replicated using F=1 regular (i.e., non-delayed) replication
!> @param jum0 - Index value to be stored for nemo within internal jump/link table array jmpb(*)
!>
!> @author Woollen @date 1994-01-06
subroutine tabent(lun,nemo,tab,itab,irep,iknt,jum0)

  use modv_vars, only: mxnrv, typs, reps, lens

  use moda_tables
  use moda_nrv203

  implicit none

  integer, intent(in) :: lun, itab, irep, iknt, jum0
  integer icdw, icsc, icrv, incw, i, jm0, node, iscl, iref, ibit

  character*24 unit
  character*10 rtag
  character*8, intent(in) :: nemo
  character, intent(in) :: tab
  character*3 typt

  common /tabccc/ icdw, icsc, icrv, incw

  jm0 = jum0

  ! Make a jump/link table entry for a replicator

  if(irep/=0) then
    rtag = reps(irep)//nemo
    do i=1,10
      if(rtag(i:i)==' ') then
        rtag(i:i) = reps(irep+5)
        call inctab(rtag,typs(irep),node)
        jump(node) = node+1
        jmpb(node) = jm0
        link(node) = 0
        ibt (node) = lens(irep)
        irf (node) = 0
        isc (node) = 0
        if(irep==1) irf(node) = iknt
        jm0 = node
        exit
      endif
    enddo
  endif

  ! Make a jump/link entry for an element or a sequence

  if(tab=='B') then
    call nemtbb(lun,itab,unit,iscl,iref,ibit)
    if(unit(1:5)=='CCITT') then
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
    if(unit(1:4)=='CODE') then
      typt = 'COD'
    elseif(unit(1:4)=='FLAG') then
      typt = 'FLG'
    endif
    if( (typt=='NUM') .and. (ibtnrv/=0) ) then
      ! This node contains a new (redefined) reference value.
      if(nnrv+1>mxnrv) call bort('BUFRLIB: TABENT - MXNRV OVERFLOW')
      nnrv = nnrv+1
      tagnrv(nnrv) = nemo
      inodnrv(nnrv) = node
      isnrv(nnrv) = node+1
      ibt(node) = ibtnrv
      if(ipfnrv==0) ipfnrv = nnrv
    elseif( (typt=='NUM') .and. (nemo(1:3)/='204') ) then
      ibt(node) = ibt(node) + icdw
      isc(node) = isc(node) + icsc
      irf(node) = irf(node) * icrv
    elseif( (typt=='CHR') .and. (incw>0) ) then
      ibt(node) = incw * 8
    endif
  else ! tab=='D'
    if(irep==0) then
      typt = 'SEQ'
    else
      typt = typs(irep+5)
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
  if(ntab>maxjl) then
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

  if(node<inode(lun)) then
    write(bort_str,'("BUFRLIB: LSTJPB - TABLE NODE (",I7,") IS OUT OF BOUNDS, < LOWER BOUNDS (",I7,")")') node,inode(lun)
    call bort(bort_str)
  endif
  if(node>isc(inode(lun))) then
    write(bort_str,'("BUFRLIB: LSTJPB - TABLE NODE (",I7,") IS OUT OF BOUNDS, > UPPER BOUNDS (",I7,")")') node,isc(inode(lun))
    call bort(bort_str)
  endif

  nod = node

  !  Find this or the previous node of type jbtyp

  do while (nod/=0)
    if(typ(nod)==jbtyp) exit
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

  if ( ( ntba(lud) >= 1 ) .and. ( ntba(lud) == ntba(lun) ) ) then
    ii = 1
    iret = 1
    do while ( ( ii <= ntba(lud) ) .and. ( iret == 1 ) )
      if ( ( mtab(ii,lud) /= 0 ) .and. ( mtab(ii,lud) == mtab(ii,lun) ) ) then
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
  if ( iret == 1 ) return

  ! Otherwise, check whether the internal Table A, B and D entries are all identical between the two units.

  if ( ( ntba(lud) == 0 ) .or. ( ntba(lun) /= ntba(lud) ) ) return
  do i = 1, ntba(lud)
    if ( idna(i,lun,1) /= idna(i,lud,1) ) return
    if ( idna(i,lun,2) /= idna(i,lud,2) ) return
    if ( taba(i,lun) /= taba(i,lud) ) return
  enddo

  if ( ( ntbb(lud) == 0 ) .or. ( ntbb(lun) /= ntbb(lud) ) ) return
  do i = 1, ntbb(lud)
    if ( idnb(i,lun) /= idnb(i,lud) ) return
    if ( tabb(i,lun) /= tabb(i,lud) ) return
  enddo

  if ( ( ntbd(lud) == 0 ) .or. ( ntbd(lun) /= ntbd(lud) ) ) return
  do i = 1, ntbd(lud)
    if ( idnd(i,lun) /= idnd(i,lud) ) return
    if ( tabd(i,lun) /= tabd(i,lud) ) return
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
      if(node==0) return
      if(typ(node)=='DRS' .or. typ(node)=='DRB') then
        invn = invwin(node,lun,inv1,inv2)
        if(invn>0) then
          call usrtpl(lun,invn,1)
          call newwin(lun,inv1,inv2)
          invn = invwin(inod,lun,invn,inv2)
          if(invn>0) return
          exit
        endif
      endif
    enddo
  enddo

  return
end subroutine drstpl

!> Get the scale factor, reference value and bit width associated with a specified occurrence of a Table B mnemonic
!>
!> Given a Table B mnemonic defined within a data subset, this
!> subroutine returns the scale factor, reference value and bit
!> width of a specified occurrence of that mnemonic within the
!> overall data subset definition, counting from the beginning
!> of the subset.
!>
!> The values returned include the application of any Table C
!> operators (e.g. 2-01-YYY, 2-02-YYY, 2-03-YYY, 2-07-YYY,
!> 2-08-YYY) which may be in effect for the specified occurrence
!> of the mnemonic.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param nemo - Table B mnemonic
!> @param nnemo - Ordinal occurrence of nemo for which information is to be returned, counting from the beginning of the
!> overall subset definition
!> @param nscl - Scale factor in effect for (nnemo)th occurrence of nemo
!> @param nref - Reference value in effect for (nnemo)th occurrence of nemo
!> @param nbts - Bit width in effect for (nnemo)th occurrence of nemo
!> @param iret - Return code
!>    - 0 = normal return
!>    - -1 = nemo could not be found, or some other error occurred
!>
!> A data subset must already be in scope within the NCEPLIBS-bufr
!> internal arrays for lunit, either via a previous call to one
!> of the [subset-reading subroutines](@ref hierarchy)
!> (when reading BUFR data subsets) or via a previous call to one
!> of the [message-writing subroutines](@ref hierarchy)
!> (when writing BUFR data subsets).
!>
!> @author J. Ator @date 2014-10-02
recursive subroutine nemspecs ( lunit, nemo, nnemo, nscl, nref, nbts, iret )

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_tables
  use moda_nrv203

  implicit none

  integer, intent(in) :: lunit, nnemo
  integer, intent(out) :: nscl, nref, nbts, iret
  integer my_lunit, my_nnemo, lun, il, im, nidx, ierfst, node, ltn, jj

  character*(*), intent(in) :: nemo
  character*10 tagn

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(nnemo,my_nnemo,1)
    call nemspecs(my_lunit,nemo,my_nnemo,nscl,nref,nbts,iret)
    call x48(nscl,nscl,1)
    call x48(nref,nref,1)
    call x48(nbts,nbts,1)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  iret = -1

  ! Get lun from lunit.

  call status( lunit, lun, il, im )
  if ( il == 0 ) return
  if ( inode(lun) /= inv(1,lun) ) return

  ! Starting from the beginning of the subset, locate the (nnemo)th occurrence of nemo.

  call fstag( lun, nemo, nnemo, 1, nidx, ierfst )
  if ( ierfst /= 0 ) return

  ! Confirm that nemo is a Table B mnemonic.

  node = inv(nidx,lun)
  if ( ( typ(node) /= 'NUM' ) .and. ( typ(node) /= 'CHR' ) ) return

  ! Get the scale factor, reference value and bit width, including accounting for any Table C operators which may be in
  ! scope for this particular occurrence of nemo.

  iret = 0

  nscl = isc(node)
  nbts = ibt(node)
  nref = irf(node)

  if ( nnrv > 0 ) then

    ! There are nodes containing redefined reference values (from one or more 2-03-YYY operators) in the jump/link table,
    ! so we need to check if this node is one of them.

    tagn = ' '
    call strsuc( nemo, tagn, ltn )
    if ( ( ltn <= 0 ) .or. ( ltn > 8 ) ) return

    do jj = 1, nnrv
      if ( ( node /= inodnrv(jj) ) .and. ( tagn(1:8) == tagnrv(jj) ) .and. &
        ( node >= isnrv(jj) ) .and. ( node <= ienrv(jj) ) ) then
        nref = int(nrv(jj))
        return
      end if
    end do

  end if

  return
end subroutine nemspecs

!> Search for a specified occurrence of a specified mnemonic within a data subset definition, starting from a specified location.
!>
!> Search for the (nutag)th occurrence of mnemonic
!> utag within the current overall subset definition, starting from
!> parameter #(nin) within the subset.  The subroutine searches forward
!> from nin if nutag is positive or else backward if nutag is negative.
!>
!> @param lun - File ID
!> @param utag - Mnemonic
!> @param nutag - Ordinal occurrence of utag to search for within the overall subset definition, counting from parameter #(nin)
!> within the subset. The subroutine will search in a forward direction from parameter #(nin) if nutag is positive or else in a
!> backward direction if nutag is negative.
!> @param nin - Location within the overall subset definition from which to begin searching for utag
!> @param nout - Location of (nutag)th occurrence of utag
!> @param iret - Return code:
!> - 0 = Normal return
!> - -1 = Requested mnemonic could not be found, or some other error occurred
!>
!> @author J. Ator @date 2014-10-02
subroutine fstag ( lun, utag, nutag, nin, nout, iret )

  use moda_usrint
  use moda_tables

  implicit none

  integer, intent(in) :: lun, nutag, nin
  integer, intent(out) :: nout, iret
  integer, parameter :: maxtg = 15
  integer ntg, istep, itagct

  character*(*), intent(in) :: utag
  character*10 tgs(maxtg)

  iret = -1

  ! Confirm that there's only one mnemonic in the input string.

  call parstr( utag, tgs, maxtg, ntg, ' ', .true. )
  if ( ntg /= 1 ) return

  ! Starting from nin, search either forward or backward for the (nutag)th occurrence of utag.

  if ( nutag == 0 ) return
  istep = isign( 1, nutag )
  itagct = 0
  nout = nin + istep
  do while ( ( nout >= 1 ) .and. ( nout <= nval(lun) ) )
    if ( tgs(1) == tag(inv(nout,lun)) ) then
      itagct = itagct + 1
      if ( itagct == iabs(nutag) ) then
        iret = 0
        return
      endif
    endif
    nout = nout + istep
  enddo

  return
end subroutine fstag

!> Get the parent for a specified occurrence of a Table B or Table D mnemonic within a data subset definition.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param tagch - Table B or Table D mnemonic
!> @param ntagch - Ordinal occurrence of TAGCH for which the parent Table D mnemonic is to be returned, counting from the
!> beginning of the overall subset definition
!> @param tagpr - Table D mnemonic corresponding to parent sequence of (ntagch)th occurrence of tagch
!> @param iret - Return code
!>    - 0 = normal return
!>    - -1 = tagpr could not be found, or some other error occurred
!>
!> A data subset must already be in scope within the NCEPLIBS-bufr
!> internal arrays for LUNIT, either via a previous call to one
!> of the [subset-reading subroutines](@ref hierarchy)
!> (when reading BUFR data subsets) or via a previous call to one
!> of the [message-writing subroutines](@ref hierarchy)
!> (when writing BUFR data subsets).
!>
!> @author J. Ator @date 2012-09-12
recursive subroutine gettagpr ( lunit, tagch, ntagch, tagpr, iret )

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lunit, ntagch
  integer, intent(out) :: iret
  integer my_lunit, my_ntagch, lun, il, im, nch

  character*(*), intent(in) :: tagch
  character*(*), intent(out) :: tagpr

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84 ( lunit, my_lunit, 1 )
    call x84 ( ntagch, my_ntagch, 1 )
    call gettagpr ( my_lunit, tagch, my_ntagch, tagpr, iret )
    call x48 ( iret, iret, 1 )

    im8b=.true.
    return
  endif

  iret = -1

  ! Get lun from lunit.

  call status( lunit, lun, il, im )
  if ( il == 0 ) return
  if ( inode(lun) /= inv(1,lun) ) return

  ! Get tagpr from the (ntagch)th occurrence of tagch.

  call fstag( lun, tagch, ntagch, 1, nch, iret )
  if ( iret /= 0 ) return

  tagpr = tag(jmpb(inv(nch,lun)))

  return
end subroutine gettagpr

!> Search for a specified mnemonic within a specified portion of the current data subset.
!>
!> This function is similar to function invwin(),
!> except that invwin() searches based on the actual node within the
!> internal jump/link table, rather than on the mnemonic corresponding
!> to that node.
!>
!> @param node - Jump/link table index of mnemonic to look for
!> @param lun - File ID
!> @param inv1 - Starting index of the portion of the subset buffer in which to look
!> @param inv2 - Ending index of the portion of the subset buffer in which to look
!>
!> @return - Location index of node within specified portion of subset buffer:
!> - 0 = Not found
!>
!> @author Woollen @date 1994-01-06
integer function invtag(node,lun,inv1,inv2) result(iret)

  use moda_usrint
  use moda_tables

  implicit none

  integer, intent(in) :: node, lun, inv1, inv2
  integer iprt

  character*10 tagn

  common /quiet/ iprt

  if(node/=0) then
    tagn = tag(node)
    ! Search between inv1 and inv2
    do iret=inv1,inv2
      if(tag(inv(iret,lun))==tagn) return
    enddo
  endif

  iret = 0

  if(iprt>=2) then
    call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
    call errwrt('BUFRLIB: INVTAG - RETURNING WITH A VALUE OF 0')
    call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
    call errwrt(' ')
  endif

  return
end function invtag

!> Search for a specified node within a specified portion of the current data subset.
!>
!> This function is similar to function invtag(), except that
!> invtag() searches based on the mnemonic corresponding to the node.
!>
!> @param node - Jump/link table index to look for
!> @param lun - File ID
!> @param inv1 - Starting index of the portion of the subset buffer in which to look
!> @param inv2 - Ending index of the portion of the subset buffer in which to look
!>
!> @return - Location index of node within specified portion of subset buffer:
!> - 0 = Not found
!>
!> @author Woollen @date 1994-01-06
integer function invwin(node,lun,inv1,inv2) result(iret)

  use moda_usrint

  implicit none

  integer, intent(in) :: node, lun, inv1, inv2
  integer iprt, idx

  character*80 errstr

  common /quiet/ iprt

  iret = 0
  if(node/=0) then
    ! Search between inv1 and inv2
    do idx=inv1,inv2
      if(inv(idx,lun)==node) then
        iret = idx
        exit
      endif
    enddo
  endif

  if(iprt>=3) then
    write(errstr,'(a,3i8)') 'invwin i1,i2,in ', inv1, inv2, iret
    call errwrt(errstr)
  endif

  return
end function invwin

!> Look for a window containing a specified node within the internal jump/link table.
!>
!> Given a node index within the internal jump/link table, this
!> subroutine looks within the current subset buffer for a "window"
!> (see below remarks) which contains this node. If found, it returns
!> the starting and ending indices of this window within the current
!> subset buffer. For example, if the node is found within the subset
!> but is not part of a delayed replication sequence, then the returned
!> indices define the start and end of the entire subset buffer.
!> Otherwise, the returned indices define the start and end of the next
!> available delayed replication sequence iteration which contains the
!> node. If no further iterations of the sequence can be found, then
!> the starting index is returned with a value of zero.
!>
!> @note
!> This is one of a number of subroutines which operate on "windows"
!> (i.e. contiguous portions) of the internal subset buffer. The
!> subset buffer is an array of values arranged according to the
!> overall template definition for a subset. A window can be any
!> contiguous portion of the subset buffer up to and including the
!> entire subset buffer itself. For the purposes of these "window
!> operator" subroutines, a window essentially consists of all of the
!> elements within a particular delayed replication group, since such
!> groups effectively define the dimensions within a BUFR subset for
!> subroutines such as ufbint()
!> which read/write individual data values. A BUFR subset with no
!> delayed replication groups is considered to have only one
!> dimension, and therefore only one "window" which spans the entire
!> subset. On the other hand, each delayed replication sequence
!> within a BUFR subset consists of some number of "windows", which
!> are a de-facto second dimension of the subset and where the number
!> of windows is the delayed descriptor replication factor (i.e. the
!> number of iterations) of the sequence. If nested delayed
!> replication is used, then there may be three or more dimensions
!> within the subset.
!>
!> @param node - Jump/link table index of mnemonic to look for
!> @param lun - File ID
!> @param iwin - Starting index of the current window iteration which contains node
!>     - 0 = Not found or no more iterations available
!> @param jwin - Ending index of the current window iteration which contains node
!>
!> @author Woollen @date 1994-01-06
subroutine getwin(node,lun,iwin,jwin)

  use moda_usrint

  implicit none

  integer, intent(in) :: node, lun
  integer, intent(out) :: iwin, jwin
  integer irpc, lstjpb, invwin

  character*128 bort_str

  irpc = lstjpb(node,lun,'RPC')

  if(irpc==0) then
    iwin = invwin(node,lun,jwin,nval(lun))
    if(iwin==0 .and. jwin>1) return
    iwin = 1
    jwin = nval(lun)
    return
  else
    iwin = invwin(irpc,lun,jwin,nval(lun))
    if(iwin==0) return
    if(val(iwin,lun)==0.) then
      iwin = 0
      return
    endif
  endif

  jwin = invwin(irpc,lun,iwin+1,nval(lun))
  if(jwin==0) then
    write(bort_str,'("BUFRLIB: GETWIN - SEARCHED BETWEEN",I5," AND",I5,", MISSING BRACKET")') iwin+1, nval(lun)
    call bort(bort_str)
  endif

  return
end subroutine getwin

!> Search consecutive subset buffer segments for a conditional node.
!>
!> Search consecutive subset buffer segments for an
!> element identified in the user string as a conditional node. A conditional
!> node is an element which must meet a condition in order to be read
!> from or written to a data subset. If a conditional element is
!> found and it conforms to the condition, then return the internal subset
!> buffer indices of the "window" (see below).
!>
!> The four conditions which can be exercised are:
!> - '<' - less than
!> - '>' - greater than
!> - '=' - equal
!> - '!' - not equal
!>
!> Each condition in a string is applied to one element, and all
!> conditions are 'and'ed to evaluate an outcome. For example, if the
!> condition string is: "POB<500 TOB>30 TQM<4" then the only levels of
!> data read or written are those with pressure less than 500 mb, temperature
!> greater than 30 degrees, and temperature quality mark less than 4.
!>
!> See the docblock in subroutine getwin() for an explanation of "windows" within the context of a BUFR data subset.
!>
!> Subroutine conwin() works with function invcon() to identify subset
!> buffer segments which conform to the set of conditions.
!>
!> @param lun - File ID
!> @param inc1 - Subset buffer start index
!> @param inc2 - Subset buffer ending index
!>
!> @author Woollen @date 1994-01-06
subroutine conwin(lun,inc1,inc2)

  use moda_usrint

  implicit none

  integer, intent(in) :: lun
  integer, intent(out) :: inc1, inc2
  integer nnod, ncon, nods, nodc, ivls, kons, nc, invcon

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)

  if(ncon==0) then
    ! There are no condition nodes in the string
    inc1 = 1
    inc2 = nval(lun)
    return
  endif

  outer: do while (.true.)
    call getwin(nodc(1),lun,inc1,inc2)
    if(inc1>0) then
      do nc=1,ncon
        if(invcon(nc,lun,inc1,inc2)==0) cycle outer
      enddo
    endif
    exit
  enddo outer

  return
end subroutine conwin

!> Search a specified window for a conditional node.
!>
!> Search a "window" (see below remarks) for an
!> element identified in the user string as a conditional node.
!> A conditional node is an element which must meet a condition in order to be
!> read from or written to a data subset.
!> If a conditional element is found and it conforms to the
!> condition, then the index of the element within the window is returned;
!> otherwise a value of zero is returned.
!>
!> See the docblock in subroutine getwin() for an explanation of "windows" within the context of a BUFR data subset.
!>
!> @param nc - Condition code:
!> - 1 = '=' (equal)
!> - 2 = '!' (not equal)
!> - 3 = '<' (less than)
!> - 4 = '>' (greater than)
!> @param lun - File ID
!> @param inv1 - First index of window to search
!> @param inv2 - Last index of window to search
!>
!> @return integer: index within window of conditional node
!> conforming to specified condition.
!> - 0 none found.
!>
!> @author Woollen @date 1994-01-06
integer function invcon(nc,lun,inv1,inv2) result(iret)

  use moda_usrint

  implicit none

  integer, intent(in) :: nc, lun, inv1, inv2
  integer nnod, ncon, nods, nodc, ivls, kons, iprt

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)
  common /quiet/ iprt

  if(inv1>0 .and. inv1<=nval(lun) .and. inv2>0 .and. inv2<=nval(lun)) then
    do iret=inv1,inv2
      if(inv(iret,lun)==nodc(nc)) then
        if(kons(nc)==1 .and. val(iret,lun)==ivls(nc)) return
        if(kons(nc)==2 .and. val(iret,lun)/=ivls(nc)) return
        if(kons(nc)==3 .and. val(iret,lun)<ivls(nc)) return
        if(kons(nc)==4 .and. val(iret,lun)>ivls(nc)) return
      endif
    enddo
  endif

  iret = 0
  if(iprt>=2) then
    call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
    call errwrt('BUFRLIB: INVCON - RETURNING WITH A VALUE OF 0')
    call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
    call errwrt(' ')
  endif

  return
end function invcon

!> Compute the ending index of the window.
!>
!> Given an index within the internal jump/link table which
!> points to the start of an "RPC" window (which is the iteration of an 8-bit
!> or 16-bit delayed replication sequence), this subroutine computes
!> the ending index of the window.  Alternatively, if the given index
!> points to the start of a "SUB" window (which is the first node of a
!> subset), then the subroutine returns the index of the last node.
!>
!> See the docblock in subroutine getwin() for an explanation of "windows" within the context of a BUFR data subset.
!>
!> @param lun - File ID
!> @param iwin - Starting index of window iteration
!> @param jwin - Ending index of window iteration
!>
!> @author J. Woollen @date 1994-01-06
subroutine newwin(lun,iwin,jwin)

  use moda_usrint

  implicit none

  integer, intent(in) :: lun, iwin
  integer, intent(out) :: jwin
  integer node, lstjpb

  character*128 bort_str

  if(iwin==1) then
    ! This is a "SUB" (subset) node, so return jwin as pointing to the last value of the entire subset.
    jwin = nval(lun)
    return
  endif

  ! Confirm that iwin points to an "RPC" node and then compute jwin.
  node = inv(iwin,lun)
  if(lstjpb(node,lun,'RPC')/=node) then
    write(bort_str,'("BUFRLIB: NEWWIN - LSTJPB FOR NODE",I6,'// &
      '" (LSTJPB=",I5,") DOES NOT EQUAL VALUE OF NODE, NOT RPC (IWIN =",I8,")")') node, lstjpb(node,lun,'RPC'), iwin
    call bort(bort_str)
  endif
  jwin = iwin+nint(val(iwin,lun))

  return
end subroutine newwin

!> Compute the start and end indices of the next window.
!>
!> Given indices within the internal jump/link table which
!> point to the start and end of an "RPC" window (which is an iteration of
!> an 8-bit or 16-bit delayed replication sequence), this subroutine
!> computes the start and end indices of the next sequential window.
!>
!> See the docblock in subroutine getwin() for an explanation of "windows" within the context of a BUFR data subset.
!>
!> @param lun - File ID
!> @param iwin - Starting index:
!>  - On input, contains starting index of current window iteration.
!>  - On output, contains starting index of next window iteration.
!> @param jwin - Ending index:
!>  - On input, contains ending index of current window iteration.
!>  - On output, contains ending index of next window iteration.
!>
!> @author J. Woollen @date 1994-01-06
subroutine nxtwin(lun,iwin,jwin)

  use moda_usrint

  implicit none

  integer, intent(in) :: lun
  integer, intent(inout) :: iwin, jwin
  integer node, lstjpb

  character*128 bort_str

  if(jwin==nval(lun)) then
    iwin = 0
    return
  endif

  node = inv(iwin,lun)
  if(lstjpb(node,lun,'RPC')/=node) then
    write(bort_str,'("BUFRLIB: NXTWIN - LSTJPB FOR NODE",I6," '// &
      '(LSTJPB=",I5,") DOES NOT EQUAL VALUE OF NODE, NOT RPC (IWIN =",I8,")")') node, lstjpb(node,lun,'RPC'), iwin
    call bort(bort_str)
  endif
  if(val(jwin,lun)==0) then
    iwin = 0
  else
    iwin = jwin
    jwin = iwin+nint(val(iwin,lun))
  endif

  return
end subroutine nxtwin

!> Search for all occurrences of a specified node within a specified portion of the current data subset.
!>
!> Search for and return all occurrences of a
!> specified node within the portion of the current subset buffer
!> bounded by the indices inv1 and inv2. The resulting list is a
!> stack of "event" indices for the requested node.
!>
!> @param node - Jump/link table index to look for
!> @param lun - File ID
!> @param inv1 - Starting index of the portion of the subset buffer in which to look
!> @param inv2 - Ending index of the portion of the subset buffer in which to look
!> @param invn - Array of stack "event" indices for node
!> @param nmax - Dimensioned size of invn; used by the function to ensure that it doesn't overflow the invn array
!>
!> @return - Number of indices within invn.
!>
!> @author Woollen @date 1994-01-06
integer function nvnwin(node,lun,inv1,inv2,invn,nmax) result(iret)

  use moda_usrint

  implicit none

  integer, intent(in) :: node, lun, inv1, inv2, nmax
  integer, intent(out) :: invn(*)
  integer iprt, i, n

  character*128 bort_str

  common /quiet/ iprt

  iret = 0

  if(node==0) then
    if(iprt>=1) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt('BUFRLIB: NVNWIN - NODE=0, IMMEDIATE RETURN')
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  endif

  do i=1,nmax
    invn(i) = 1E9
  enddo

  ! Search between inv1 and inv2

  do n=inv1,inv2
    if(inv(n,lun)==node) then
      if(iret+1>nmax) then
        write(bort_str,'("BUFRLIB: NVNWIN - THE NUMBER OF EVENTS EXCEEDS THE LIMIT NMAX (",I5,")")') nmax
        call bort(bort_str)
      endif
      iret = iret+1
      invn(iret) = n
    endif
  enddo

  return
end function nvnwin
