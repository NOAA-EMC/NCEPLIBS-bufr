!> @file
!> @brief Print the contents of a data subset or DX BUFR table.
!>
!> @authors J. Woollen, J. Ator, D. Keyser @date 1994-01-06

!> Print a verbose listing of the contents of a data
!> subset, including all data values and replicated sequences, as well
!> as jump/link table information and other internal subset pointers.
!>
!> This subroutine is similar to subroutine ufdump(), but it prints
!> different characteristics of each data subset, and in a slightly
!> different format. However, both subroutines can be useful for
!> different diagnostic purposes, and both can also be run
!> interactively to scroll through the contents of a data subset.
!>
!> Logical unit abs(lunin) should have already been opened for
!> input operations via a previous call to subroutine openbf(), and a
!> BUFR data subset should have already been read into internal arrays
!> via a previous call to one of the
!> [subset-reading subroutines](@ref hierarchy).
!>
!> Except when luprt = 0, logical unit luprt must already be
!> associated with a filename on the local system, typically via a
!> Fortran "OPEN" statement. When luprt = 0, the subroutine will run
!> interactively and print to standard output, scrolling 20 lines at
!> a time and prompting each time whether to quit and return to the
!> application program (by typing 'q' then '&lt;Enter&gt;') or continue
!> scrolling (by typing anything else).
!>
!> @param lunin - Absolute value is Fortran logical unit number for BUFR file
!> - If lunin > 0, data values are printed to luprt using the format descriptor code 'G15.6', meaning that all values will
!> be printed (since the format adapts to the order of magnitude of each value), but values won't necessarily be lined up
!> with the decimal point in the same column
!> - If lunin < 0, data values are printed to luprt using the format descriptor code 'F15.6', meaning that all values will
!> be lined up with the decimal point in the same column, but values exceeding the format width of 15 characters will print
!> as overflow (e.g. '***************')
!> @param luprt - Fortran logical unit number for print output:
!> - 0 = Run interactively, printing to standard output
!>
!> @authors J. Woollen, J. Ator, D. Keyser @date 1994-01-06
recursive subroutine ufbdmp(lunin,luprt)

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_tababd
  use moda_tables

  implicit none

  integer, intent(in) :: lunin, luprt
  integer, parameter :: mxfv = 31
  integer ifv(mxfv), my_lunin, my_luprt, luout, lunit, lun, il, im, nv, nd, it, ib, is, ir, jp, lk, jb, &
    idn, nifv, nchr, n, ii, ipt, isz, isize, ibfms, icbfms

  character lchr2*120, lchr*20, pmiss*20, bits*14, tg*10, tg_rj*10, vc*8, fmtf*7, tp*3, tab, you

  real*8 vl

  equivalence (vl,vc)

  data pmiss /'             MISSING'/
  data you /'Y'/

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunin,my_lunin,1)
    call x84(luprt,my_luprt,1)
    call ufbdmp(my_lunin,my_luprt)

    im8b=.true.
    return
  endif

  if(luprt.eq.0) then
    luout = 6
  else
    luout = luprt
  endif

  ! Check the file status and inode

  lunit = abs(lunin)
  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFBDMP - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: UFBDMP - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) call bort('BUFRLIB: UFBDMP - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
  if(inode(lun).ne.inv(1,lun)) call bort('BUFRLIB: UFBDMP - LOCATION OF INTERNAL TABLE FOR '// &
    'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  ! Dump the contents of @ref moda_usrint for unit abs(lunin)

  do nv=1,nval(lun)
    if(luprt.eq.0 .and. mod(nv,20).eq.0) then

      ! When luprt=0, the output will be scrolled, 20 elements at a time

      print*,'(<enter> for MORE, q <enter> to QUIT)'
      read(5,'(A1)') you

      ! If the terminal enters "q" followed by "<enter>", then scrolling will end and the subroutine will return to the
      ! calling program

      if(you.eq.'q') THEN
        print*
        print*,'==> You have chosen to stop the dumping of this subset'
        print*
        return
      endif
    endif
    nd = inv (nv,lun)
    vl = val (nv,lun)
    tg = tag (nd)
    tp = typ (nd)
    it = itp (nd)
    ib = ibt (nd)
    is = isc (nd)
    ir = irf (nd)
    jp = jump(nd)
    lk = link(nd)
    jb = jmpb(nd)
    tg_rj = adjustr(tg)
    if(tp.ne.'CHR') then
      bits = '              '
      if(it.eq.2) then
        call nemtab(lun,tg,idn,tab,n)
        if(tabb(n,lun)(71:75).eq.'FLAG') then

          ! Print a listing of the bits corresponding to this value.

          call upftbv(lunit,tg,vl,mxfv,ifv,nifv)
          if(nifv.gt.0) then
            bits(1:1) = '('
            ipt = 2
            do ii=1,nifv
              isz = isize(ifv(ii))
              write(fmtf,'(A2,I1,A4)') '(I', isz, ',A1)'
              if((ipt+isz).le.14) then
                write(bits(ipt:ipt+isz),fmtf) ifv(ii), ','
                ipt = ipt + isz + 1
              else
                bits(2:13) = 'MANY BITS ON'
                ipt = 15
              endif
            enddo
            bits(ipt-1:ipt-1) = ')'
          endif
        endif
      endif
      if(ibfms(vl).ne.0) then
        write(luout,'(I5,1X,A3,A1,I1,1X,A10,1X,   A20,  14X,7(1X,I5))') nv,tp,'-',it,tg_rj,pmiss,ib,is,ir,nd,jp,lk,jb
      else
        if(lunit.eq.lunin) then
          write(luout,'(I5,1X,A3,A1,I1,1X,A10,5X,G15.6,1X,A14,7(1X,I5))') nv,tp,'-',it,tg_rj,vl,bits,ib,is,ir,nd,jp,lk,jb
        else
          write(luout,'(I5,1X,A3,A1,I1,1X,A10,5X,F15.6,1X,A14,7(1X,I5))') nv,tp,'-',it,tg_rj,vl,bits,ib,is,ir,nd,jp,lk,jb
        endif
      endif
    else
      nchr=ib/8
      if(nchr.gt.8) then
        call readlc(lunit,lchr2,tg_rj)
        if (icbfms(lchr2,nchr).ne.0) then
          lchr = pmiss
        else
          lchr = lchr2(1:20)
        endif
      else
        if(ibfms(vl).ne.0) then
          lchr = pmiss
        else
          lchr = vc
        endif
      endif
      if ( nchr.le.20 .or. lchr.eq.pmiss ) then
        lchr = adjustr(lchr)
        write(luout,'(I5,1X,A3,A1,I1,1X,A10,1X,   A20,  14X,7(1X,I5))') nv,tp,'-',it,tg_rj,lchr,ib,is,ir,nd,jp,lk,jb
      else
        write(luout,'(I5,1X,A3,A1,I1,1X,A10,1X,     A,      7(1X,I5))') nv,tp,'-',it,tg_rj,lchr2(1:nchr),ib,is,ir,nd,jp,lk,jb
      endif
    endif
  enddo

  write(luout,'(/A/)') ' >>> END OF SUBSET <<< '

  return
end subroutine ufbdmp

!> Print a verbose listing of the contents of a data
!> subset, including all data values and replicated sequences, as well
!> as the meanings of data values which are code or flag table entries.
!>
!> This subroutine is similar to subroutine ufbdmp(), but it prints
!> different characteristics of each data subset, and in a slightly
!> different format. However, both subroutines can be useful for
!> different diagnostic purposes, and both can also be run
!> interactively to scroll through the contents of a data subset.
!>
!> Logical unit lunit should have already been opened for
!> input operations via a previous call to subroutine openbf(), and a
!> BUFR data subset should have already been read into internal arrays
!> via a previous call to one of the
!> [subset-reading subroutines](@ref hierarchy).
!>
!> Except when luprt = 0, logical unit luprt must already be
!> associated with a filename on the local system, typically via a
!> Fortran "OPEN" statement. When luprt = 0, the subroutine will run
!> interactively and print to standard output, scrolling 20 lines at
!> a time and prompting each time whether to quit and return to the
!> application program (by typing 'q' then '&lt;Enter&gt;') or continue
!> scrolling (by typing anything else).
!>
!> In order for the meanings of code and flag table values to be
!> included in the output, a previous call to subroutine codflg()
!> must have been made with argument cf = 'Y'. Otherwise, only the
!> code and flag table values themselves will be printed.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param luprt - Fortran logical unit number for print output
!>  - 0 = Run interactively, printing to standard output
!>
!> @authors J. Woollen, J. Ator @date 2002-05-14
recursive subroutine ufdump(lunit,luprt)

  use bufrlib

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_tababd
  use moda_tables
  use moda_tablef
  use moda_nrv203

  implicit none

  integer, intent(in) :: lunit, luprt
  integer, parameter :: mxfv = 31 , mxcfdp = 5, mxseq = 10, mxls = 10
  integer ifv(mxfv), icfdp(mxcfdp), idxrep(mxseq), numrep(mxseq), lsqnam(mxseq), lsct(mxls), my_lunit, my_luprt, &
    nseq, nls, lcfmeang, luout, lun, il, im, node, lnm2, lnm3, itmp, ityp, ii, jj, nifv, nv, n, nchr, idn, ipt, &
    nrfe, nout, lcfmg, ifvd, iersf, ierbd, ierft, isz, isize, ireadmt, ibfms, icbfms
  integer*8 ival

  real*8 rval

  character cfmeang*120, lchr2*120, fmt*80, desc*64, unit*24, lchr*20, pmiss*20, nemo3*15, nemo*10, nemo2*10, tagrfe*10, &
    seqnam(mxseq)*10, lsnemo(mxls)*10, nemod*8, cval*8, fmtf*7, numb*6, type*3, tab, you

  logical track, found, rdrv

  equivalence (rval,cval)

  data pmiss /'             MISSING'/
  data you /'Y'/

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(luprt,my_luprt,1)
    call ufdump(my_lunit,my_luprt)

    im8b=.true.
    return
  endif

  nseq = 0
  nls = 0
  lcfmeang = len(cfmeang)

  if(luprt.eq.0) then
    luout = 6
  else
    luout = luprt
  endif

  ! Check the file status and inode

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: UFDUMP - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: UFDUMP - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) call bort('BUFRLIB: UFDUMP - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
  if(inode(lun).ne.inv(1,lun)) call bort('BUFRLIB: UFDUMP - LOCATION OF INTERNAL TABLE FOR '// &
    'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL SUBSET ARRAY')

  write(luout,'(/,2A,/)') 'MESSAGE TYPE ',tag(inode(lun))

  ! If code/flag table details are being printed, and if this is the first subset of a new message, then
  ! make sure the appropriate master tables have been read in to memory for this message.

  if(cdmf.eq.'Y' .and. nsub(lun).eq.1) itmp = ireadmt(lun)

  ! Dump the contents of @ref moda_usrint for unit lunit

  do nv=1,nval(lun)
    if(luprt.eq.0 .and. mod(nv,20).eq.0) then

      ! When luprt=0, the output will be scrolled, 20 elements at a time

      print*,'(<enter> for MORE, q <enter> to QUIT)'
      read(5,'(A1)') you

      ! If the terminal enters "q" followed by "<enter>", then scrolling will end and the subroutine will return to the
      ! calling program

      if(you.eq.'q') then
        print*
        print*,'==> You have chosen to stop the dumping of this subset'
        print*
        return
      endif
    endif

    node = inv(nv,lun)
    nemo = tag(node)
    ityp = itp(node)
    type = typ(node)

    if(ityp.ge.1.and.ityp.le.3) then
      call nemtab(lun,nemo,idn,tab,n)
      if(n>0) then
         numb = tabb(n,lun)(1:6)
         desc = tabb(n,lun)(16:70)
         unit = tabb(n,lun)(71:94)
      endif
      rval = val(nv,lun)
    endif

    if((ityp.eq.0).or.(ityp.eq.1)) then

      ! Sequence descriptor or delayed descriptor replication factor

      if((type.eq.'REP').or.(type.eq.'DRP').or.(type.eq.'DRB').or.(type.eq.'DRS')) then

        ! Print the number of replications
        nseq = nseq+1
        if(nseq.gt.mxseq) call bort('BUFRLIB: UFDUMP - MXSEQ OVERFLOW')
        if(type.eq.'REP') then
          numrep(nseq) = irf(node)
        else
          numrep(nseq) = nint(rval)
        endif
        call strsuc(nemo,nemo2,lnm2)
        fmt = '(11X,A,I6,1X,A)'
        write(luout,fmt) nemo2(1:lnm2), numrep(nseq), 'REPLICATIONS'

        ! How many times is this sequence replicated?
        if(numrep(nseq).gt.1) then
          ! Track the sequence
          seqnam(nseq) = nemo2
          lsqnam(nseq) = lnm2
          idxrep(nseq) = 1
        else
          ! Don't bother
          nseq = nseq-1
        endif
      elseif( ((type.eq.'SEQ').or.(type.eq.'RPC').or.(type.eq.'RPS')) .and. (nseq.gt.0) ) then

        ! Is this one of the sequences being tracked?
        ii = nseq
        track = .false.
        call strsuc(nemo,nemo2,lnm2)
        do while ((ii.ge.1).and.(.not.track))
          if(nemo2(1:lnm2).eq.seqnam(ii)(2:lsqnam(ii)-1)) then
            track = .true.
            ! Mark this level in the output
            fmt = '(4X,A,2X,A,2X,A,I6,2X,A)'
            write(luout,fmt) '++++++', nemo2(1:lnm2), 'REPLICATION #', idxrep(ii), '++++++'
            if(idxrep(ii).lt.numrep(ii)) then
              ! There are more levels to come
              idxrep(ii) = idxrep(ii)+1
            else
              ! This was the last level for this sequence, so stop tracking it
              nseq = nseq-1
            endif
          else
            ii = ii-1
          endif
        enddo
      endif
    elseif(ityp.eq.2) then

      ! Numeric value.

      ! First check if this node contains a redefined reference value.  If so, modify the desc field to label it as such.
      jj = 1
      rdrv = .false.
      do while ((jj.le.nnrv).and.(.not.rdrv))
        if (node.eq.inodnrv(jj)) then
          rdrv = .true.
          desc = 'New reference value for ' // nemo
          unit = ' '
        else
          jj = jj + 1
        endif
      enddo

      ! Next check if this element refers to another element via a bitmap.  If so, modify the desc field to identify the
      ! referred element.
      nrfe = nrfelm(nv,lun)
      if(nrfe.gt.0) then
        tagrfe = tag(inv(nrfe,lun))
        jj = 48
        do while((jj.ge.1).and.(desc(jj:jj).eq.' '))
          jj = jj - 1
        enddo
        if(jj.le.33) desc(jj+1:jj+15) = ' for ' // tagrfe
      endif

      ! Now print the value
      if(ibfms(rval).ne.0) then
        ! The value is "missing".
        fmt = '(A6,2X,A10,2X,A20,2X,A24,6X,A48)'
        write(luout,fmt) numb,nemo,pmiss,unit,desc
      else
        fmt = '(A6,2X,A10,2X,      ,2X,A24,6X,A48)'
        ! Based upon the corresponding scale factor, select an appropriate format for the printing of this value.
        if(isc(node).gt.0) then
          write(fmt(15:20),'(A,I2)') 'F20.', isc(node)
        else
          write(fmt(18:20),'(A)') 'I20'
        endif
        if(unit(1:4).eq.'FLAG') then
          ! Print a listing of the bits corresponding to this value.
           call upftbv(lunit,nemo,rval,mxfv,ifv,nifv)
           if(nifv.gt.0) then
             unit(11:11) = '('
             ipt = 12
             do ii=1,nifv
               isz = isize(ifv(ii))
               write(fmtf,'(A2,I1,A4)') '(I', isz, ',A1)'
               if((ipt+isz).le.24) then
                 write(unit(ipt:ipt+isz),fmtf) ifv(ii), ','
                 ipt = ipt + isz + 1
               else
                 unit(12:23) = 'MANY BITS ON'
                 ipt = 25
               endif
             enddo
             unit(ipt-1:ipt-1) = ')'
           endif
        endif
        if(isc(node).gt.0) then
          write(luout,fmt) numb,nemo,rval,unit,desc
        else
          ival = nint(rval,8)
          write(luout,fmt) numb,nemo,ival,unit,desc
        endif
        if( (unit(1:4).eq.'FLAG' .or. unit(1:4).eq.'CODE') .and. (cdmf.eq.'Y') ) then
          ! Print the meanings of the code and flag values.
          fmt = '(31X,I8,A,A)'
          if(unit(1:4).eq.'CODE') then
            nifv = 1
            ifv(nifv) = nint(rval)
          endif
          do ii=1,nifv
            icfdp(1) = (-1)
            call srchtbf_c(idn,ifv(ii),icfdp(1),mxcfdp,-1,cfmeang,lcfmeang,lcfmg,iersf)
            if(iersf.eq.0) then
              write(luout,fmt) ifv(ii),' = ',cfmeang(1:lcfmg)
            elseif(iersf.lt.0) then
              write(luout,fmt) ifv(ii),' = ','***THIS IS AN ILLEGAL/UNDEFINED VALUE***'
            else
              ! The meaning of this value is dependent on the value of another mnemonic in the report.  Look for
              ! that other mnemonic within the report and then use it and its associated value to retrieve and print
              ! the proper meaning from the code/flag tables.
              ierft = (-1)
              jj = 0
              do while((jj.lt.iersf).and.(ierft.lt.0))
                jj = jj + 1
                call numtbd(lun,icfdp(jj),nemod,tab,ierbd)
                if((ierbd.gt.0).and.(tab.eq.'B')) call fstag(lun,nemod,-1,nv,nout,ierft)
              enddo
              if(ierft.eq.0) then
                ifvd = nint(val(nout,lun))
                if(jj.gt.1) icfdp(1) = icfdp(jj)
                call srchtbf_c(idn,ifv(ii),icfdp(1),mxcfdp,ifvd,cfmeang,lcfmeang,lcfmg,iersf)
                if(iersf.eq.0) write(luout,fmt) ifv(ii),' = ', cfmeang(1:lcfmg)
              endif
            endif
          enddo
        endif
      endif
    elseif(ityp.eq.3) then

      ! Character (CCITT IA5) value

      nchr = ibt(node)/8

      if(ibfms(rval).ne.0) then
        lchr = pmiss
      else if(nchr.le.8) then
        lchr = cval
      else
        ! Track the number of occurrences of this long character string, so that we can properly output each one.
        ii = 1
        found = .false.
        do while((ii.le.nls).and.(.not.found))
          if(nemo.eq.lsnemo(ii)) then
            found = .true.
          else
            ii = ii + 1
          endif
        enddo
        if(.not.found) then
          nls = nls+1
          if(nls.gt.mxls) call bort('BUFRLIB: UFDUMP - MXLS OVERFLOW')
          lsnemo(nls) = nemo
          lsct(nls) = 1
          nemo3 = nemo
        else
          call strsuc(nemo,nemo3,lnm3)
          lsct(ii) = lsct(ii) + 1
          write(fmtf,'(A,I1,A)') '(2A,I', isize(lsct(ii)), ')'
          write(nemo3,fmtf) nemo(1:lnm3), '#', lsct(ii)
        endif

        call readlc(lunit,lchr2,nemo3)
        if (icbfms(lchr2,nchr).ne.0) then
          lchr = pmiss
        else
          lchr = lchr2(1:20)
        endif
      endif

      if ( nchr.le.20 .or. lchr.eq.pmiss ) then
        lchr = adjustr(lchr)
        fmt = '(A6,2X,A10,2X,A20,2X,"(",I2,")",A24,2X,A48)'
        write(luout,fmt) numb,nemo,lchr,nchr,unit,desc
      else
        fmt = '(A6,2X,A10,2X,A,2X,"(",I3,")",A23,2X,A48)'
        write(luout,fmt) numb,nemo,lchr2(1:nchr),nchr,unit,desc
      endif
    endif

  enddo

  write(luout,'(/A/)') ' >>> END OF SUBSET <<< '

  return
end subroutine ufdump

!> Print a copy of the DX BUFR table associated with a specified Fortran logical unit.
!>
!> This subroutine is especially useful for learning the structure
!> of existing BUFR files which contain DX BUFR table information
!> embedded as BUFR messages within those files.
!> The DX BUFR table is printed using the same ASCII format
!> described in the documentation for
!> [DX BUFR Tables](@ref dfbftab), so the output file is suitable
!> for use as Fortran logical unit LUNDX in subsequent calls to
!> subroutine openbf() for reading or writing additional BUFR
!> files with the same structure.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param ldxot - Fortran logical unit number for print output
!>
!> Logical unit lunit must be open for either input or output
!> operations via a previous call to subroutine openbf().
!> Logical unit ldxot must already be associated with a filename
!> on the local system, typically via a Fortran "OPEN" statement.
!>
!> @remarks
!> - This subroutine only prints the DX BUFR table that is currently
!> in scope for logical unit lunit.  Therefore, if logical unit lunit
!> contains multiple embedded DX BUFR tables, then multiple calls to
!> this subroutine must be made to print out all of the tables,
!> once while each table is in scope for a data subset defined
!> within that particular table.
!>
!> @author J. Ator @date 2004-08-18
recursive subroutine dxdump(lunit,ldxot)

  use modv_vars, only: im8b, reps

  use moda_tababd
  use moda_nmikrp

  implicit none

  integer, intent(in) :: lunit, ldxot
  integer my_lunit, my_ldxot, lun, il, im, n, na, nc, nch, ic, icms, nseq

  character card*80, cardi1*80, cardi2*80, cardi3*80, cardi4*80, cmstr*20, wrk3*10, wrk1*8, wrk2*8, adn*6

  logical tbskip, tdskip, xtrci1

  data cardi1  /'|          |        |                                                          |'/
  data cardi2  /'|          |                                                                   |'/
  data cardi3  /'|          |      |             |     |                          |-------------|'/
  data cardi4  /'|------------------------------------------------------------------------------|'/

  ! Statement functions
  tbskip(adn) = ((adn.eq.'063000').or.(adn.eq.'063255').or.(adn.eq.'031000').or.(adn.eq.'031001').or.(adn.eq.'031002'))
  tdskip(adn) = ((adn.eq.'360001').or.(adn.eq.'360002').or.(adn.eq.'360003').or.(adn.eq.'360004'))

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(ldxot,my_ldxot,1)
    call dxdump(my_lunit,my_ldxot)

    im8b=.true.
    return
  endif

  ! Determine lun from lunit.

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: DXDUMP - BUFR FILE IS CLOSED, IT MUST BE OPEN')

  ! Create and write out (to ldxot) the header cards for the descriptor definition section.

  card=cardi4
  card( 1: 1)='.'
  card(80:80)='.'
  write (ldxot,'(A)') card

  card=cardi4
  card( 2: 2)=' '
  card(79:79)=' '
  card(15:64)='   USER DEFINITIONS FOR TABLE-A TABLE-B TABLE D   '
  write (ldxot,'(A)') card

  write (ldxot,'(A)') cardi4

  card=cardi1
  card( 3:10)='MNEMONIC'
  card(14:19)='NUMBER'
  card(23:33)='DESCRIPTION'
  write (ldxot,'(A)') card

  card=cardi4
  card(12:12)='|'
  card(21:21)='|'
  write (ldxot,'(A)') card

  ! Create and write out (to ldxot) the Table D descriptor definition cards.

  write (ldxot,'(A)') cardi1

  xtrci1=.false.
  do n=1,ntbd(lun)
    if(.not.tdskip(tabd(n,lun)(1:6))) then
      card=cardi1
      card( 3:10)=tabd(n,lun)( 7:14)
      card(14:19)=tabd(n,lun)( 1: 6)
      card(23:77)=tabd(n,lun)(16:70)
      ! Check if this Table D mnemonic is also a Table A mnemonic. If so, then label it as such and also check if it is the
      ! last of the Table A mnemonics, in which case an extra cardi1 line will be written to ldxot in order to separate
      ! the Table A mnemonics from the other Table D mnemonics.
      do na=1,ntba(lun)
        if(taba(na,lun)(4:11).eq.tabd(n,lun)(7:14)) then
          card(14:14)='A'
          if(na.eq.ntba(lun)) xtrci1=.true.
          exit
        end if
      end do
      write (ldxot,'(A)') card
      if(xtrci1) then
        write (ldxot,'(A)') cardi1
        xtrci1=.false.
      end if
    end if
  end do

  ! Create and write out (to ldxot) the Table B descriptor definition cards.

  write (ldxot,'(A)') cardi1

  do n=1,ntbb(lun)
    if(.not.tbskip(tabb(n,lun)(1:6))) then
      card=cardi1
      card( 3:10)=tabb(n,lun)( 7:14)
      card(14:19)=tabb(n,lun)( 1: 6)
      card(23:77)=tabb(n,lun)(16:70)
      write (ldxot,'(A)') card
    end if
  end do

  write (ldxot,'(A)') cardi1

  ! Create and write out (to ldxot) the header cards for the sequence definition section.

  write (ldxot,'(A)') cardi4

  card=cardi2
  card( 3:10)='MNEMONIC'
  card(14:21)='SEQUENCE'
  write (ldxot,'(A)') card

  card=cardi4
  card(12:12)='|'
  write (ldxot,'(A)') card

  ! Create and write out (to ldxot) the Table D sequence definition cards.

  write (ldxot,'(A)') cardi2

  do n=1,ntbd(lun)
    if(.not.tdskip(tabd(n,lun)(1:6))) then
      card=cardi2
      card( 3:10)=tabd(n,lun)( 7:14)
      ic = 14
      ! Get the list of child mnemonics for this Table D descriptor, and then add each one (including any replication tags)
      ! to the sequence definition card for this Table D descriptor.
      call nemtbd(lun,n,nseq,nem(1,1),irp(1,1),krp(1,1))
      if(nseq.gt.0) then
        do nc=1,nseq
          cmstr=' '
          icms=0
          call strsuc(nem(nc,1),wrk2,nch)
          if(irp(nc,1).ne.0) then
            ! Add the opening replication tag.
            icms=icms+1
            cmstr(icms:icms)=reps(irp(nc,1))
          end if
          cmstr(icms+1:icms+nch)=wrk2(1:nch)
          icms=icms+nch
          if(irp(nc,1).ne.0) then
            ! Add the closing replication tag.
            icms=icms+1
            cmstr(icms:icms)=reps(irp(nc,1)+5)
          end if
          if(krp(nc,1).ne.0) then
            ! Add the fixed replication count.
            wrk1=' '
            write (wrk1,'(I3)') krp(nc,1)
            call strsuc(wrk1,wrk2,nch)
            cmstr(icms+1:icms+nch)=wrk2(1:nch)
            icms=icms+nch
          end if
          ! Will this child (and its replication tags, if any) fit into the current sequence definition card?  If not, then
          ! write out (to ldxot) the current card and initialize a new one to hold this child.
          if(ic.gt.(79-icms)) then
            write (ldxot,'(A)') card
            card=cardi2
            card( 3:10)=tabd(n,lun)( 7:14)
            ic = 14
          end if
          card(ic:ic+icms-1)=cmstr(1:icms)
          ! Note that we want to leave 2 blank spaces between each child within the sequence definition card, to improve
          ! readability.
          ic=ic+icms+2
        end do
        write (ldxot,'(A)') card
        write (ldxot,'(A)') cardi2
      end if
    end if
  end do

  ! Create and write out (to ldxot) the header cards for the element definition section.

  write (ldxot,'(A)') cardi4

  card=cardi3
  card( 3:10)='MNEMONIC'
  card(14:17)='SCAL'
  card(21:29)='REFERENCE'
  card(35:37)='BIT'
  card(41:45)='UNITS'
  write (ldxot,'(A)') card

  card=cardi4
  card(12:12)='|'
  card(19:19)='|'
  card(33:33)='|'
  card(39:39)='|'
  card(66:66)='|'
  write (ldxot,'(A)') card

  ! Create and write out (to ldxot) the Table B element definition cards.

  write (ldxot,'(A)') cardi3

  do n=1,ntbb(lun)
    if(.not.tbskip(tabb(n,lun)(1:6))) then
      card=cardi3
      card( 3:10)=tabb(n,lun)( 7:14)
      card(41:64)=tabb(n,lun)(71:94)
      ! Add the scale factor.
      call strsuc(tabb(n,lun)(96:98),wrk2,nch)
      card(17-nch+1:17)=wrk2
      if(tabb(n,lun)(95:95).eq.'-') card(17-nch:17-nch)='-'
      ! Add the reference value.
      call strsuc(tabb(n,lun)(100:109),wrk3,nch)
      card(31-nch+1:31)=wrk3
      if(tabb(n,lun)(99:99).eq.'-') card(31-nch:31-nch)='-'
      ! Add the bit width.
      call strsuc(tabb(n,lun)(110:112),wrk2,nch)
      card(37-nch+1:37)=wrk2
      write (ldxot,'(a)') card
    end if
  end do

  write (ldxot,'(A)') cardi3

  ! Create and write out (to ldxot) the closing card.

  card=cardi4
  card( 1: 1)='`'
  card(80:80)=''''
  write (ldxot,'(A)') card

  return
end subroutine dxdump

!> Get Table B and Table D information from the internal DX BUFR tables.
!>
!> The information is returned in a memory array in a pre-defined ASCII format.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param itab - Dimensioned size of tabdb array; used by the subroutine to ensure that it doesn't overflow the tabdb array
!> @param tabdb - Internal Table B and Table D information
!> @param jtab - Number of entries stored within tabdb
!>
!> @author J. Ator @date 2005-11-29
recursive subroutine getabdb(lunit,tabdb,itab,jtab)

  use modv_vars, only: im8b

  use moda_tababd
  use moda_nmikrp

  implicit none

  integer, intent(in) :: lunit, itab
  integer, intent(out) :: jtab
  integer my_lunit, my_itab, lun, il, im, i, j, k, nseq

  character*128, intent(out) :: tabdb(*)
  character*8 nemo

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(itab,my_itab,1)
    call getabdb(my_lunit,tabdb,my_itab,jtab)
    call x48(jtab,jtab,1)
    im8b=.true.
    return
  endif

  jtab = 0

  ! Make sure the file is open

  call status(lunit,lun,il,im)
  if(il.eq.0) return

  ! Write out the Table D entries for this file

  do i=1,ntbd(lun)
    nemo = tabd(i,lun)(7:14)
    call nemtbd(lun,i,nseq,nem(1,1),irp(1,1),krp(1,1))
    do j=1,nseq,10
      jtab = jtab+1
      if(jtab.le.itab) then
        write(tabdb(jtab),fmt='(A,A8,10(1X,A10))') 'D ', nemo, (nem(k,1),k=j,min(j+9,nseq))
      endif
    enddo
  enddo

  ! Add the Table B entries

  do i=1,ntbb(lun)
    jtab = jtab+1
    if(jtab.le.itab) then
      write(tabdb(jtab),fmt='(A,A8,1X,A42)') 'B ', tabb(i,lun)(7:14), tabb(i,lun)(71:112)
    endif
  enddo

  return
end subroutine getabdb
