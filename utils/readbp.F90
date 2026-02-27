!> @file
!> @brief Read PREPBUFR file containing embedded DX BUFR tables,
!> and print each report one at a time.
!>
!> @author J. Woollen @date 1994-01-06

!> Read PREPBUFR file containing embedded DX BUFR tables,
!> and print each report one at a time. Options are listed
!> by running "readbp" without argumets.
!>
!> @return 0 for success, error code otherwise.
!>
!> @author J. Woollen @date 1994-01-06

!-----------------------------------------------------------------------
! Read and display an on29bufr file one report at a time
!-----------------------------------------------------------------------
program readbp

      character(120) ::  file
      character(50)  ::  optarg
      character(40)  ::  hstr,ostr,qstr
      character(10)  ::  val
      character(8)   ::  sid,sta,subset,msg,cmc(17)
      character(3)   ::  vars(8)
      integer        ::  iostat
      real(8)        ::  hdr(10),obs(10,255),qms(10,255),xob,yob
      logical        ::  window,steam,level,dump,hedr,exist

      data hstr/'SID XOB YOB DHR ELV T29 ITP TYP SRC PRG '/
      data ostr/'CAT POB QOB TOB ZOB UOB VOB PSL         '/
      data qstr/'PQM QQM TQM ZQM WQM PSQ                 '/

      data vars/'LVL','CAT','POB','SPH','TOB','ZOB','UOB','VOB'/
      data cmc /'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','*'/

      data lubfr  /8    /
      data sta    /'   '/
      data msg    /'   '/
      data pob    /0/
      data irt    /0/
      data itp    /0/
      data ikx    /0/
      data window /.false./
      data steam  /.false./
      data level  /.false./
      data dump   /.false./
      data hedr   /.false./

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  check for filename argument

      narg=command_argument_count()
1     if(narg<1) then
        call printx('                                                                                                        ')
        call printx('Usage: readbp <-s> <-w> <m> <-k> <-r> <-d> <-n> <-h>  prep bufrfile                                     ')
        call printx('                                                                                                        ')
        call printx('Search filter and/or print prepbufr reports in various ways                                             ')
        call printx('                                                                                                        ')
        call printx('-s "station_id " print reports where "station_id" matches the report id up to the len of "station_id"   ')
        call printx('-w "x1 x2 y1 y2" print reports within a lon/lat box                                                     ')
        call printx('-m "subset     " print reports with this subset name                                                    ')
        call printx('-k "gsi  rtype " print reports with this gsi report type                                                ')
        call printx('-r "on29 rtype " print reports with this on29 report type                                               ')
        call printx('-d               print reports using ufdump - note: this works with any NCEP BUFR file                  ')
        call printx('-n               no pause between reports output                                                        ')
        call printx('-h               print only report headers                                                              ')
        call printx('                                                                                                        ')
        call printx('Only a filename is required in which case step through the reports one at a time using "enter"          ')
        call printx('                                                                                                        ')
        call printx('Optional arguments can also be applied in the pause between reports output without using  a dash        ')
        call printx('                                                                                                        ')
        call printx('Optional arguments will be applied in concert in most cases                                             ')
        call printx('                                                                                                        ')
        stop 2
      endif

      iarg=1
      do while(iarg<=narg)
      call get_command_argument(iarg,file)
      if(file(1:1)=='-') then
         if(file(2:2)=='s') then
           iarg=iarg+1; call get_command_argument(iarg,sta); nsta=len_trim(sta)
         elseif(file(2:2)=='w') then
           iarg=iarg+1; call get_command_argument(iarg,val); read(val,*)x1
           iarg=iarg+1; call get_command_argument(iarg,val); read(val,*)x2
           iarg=iarg+1; call get_command_argument(iarg,val); read(val,*)y1
           iarg=iarg+1; call get_command_argument(iarg,val); read(val,*)y2
           window=.true.
         elseif(file(2:2)=='k') then
           iarg=iarg+1; call get_command_argument(iarg,val); read(val,*)ikx
         elseif(file(2:2)=='r') then
           iarg=iarg+1; call get_command_argument(iarg,val); read(val,*)irt
         elseif(file(2:2)=='m') then
           iarg=iarg+1; call get_command_argument(iarg,val); msg=val(1:8)
         elseif(file(2:2)=='d') then
           iarg=iarg+1; dump=.true.
         elseif(file(2:2)=='h') then
           iarg=iarg+1; hedr=.true.
         elseif(file(2:2)=='n') then
           iarg=iarg+1; steam=.true.
         else
           iarg=iarg+1
         endif
         file='nofile'
         cycle
      endif
      iarg=iarg+1
      enddo

! if file exists then open it, else stop

      narg=0
      if(file=='nofile') goto 1
      file = trim(adjustl(file))
      inquire(file=file,exist=exist)
      if (.not.exist) then
         print *, trim(file)//' does not exist'
         stop 3
      endif

!  open the bufr input file
!  ------------------------

      open(lubfr,file=file,form='unformatted')
      call openbf(lubfr,'IN',lubfr)
      call datelen(10)

!  Read a subset - read another message when no more subsets
!  ---------------------------------------------------------

      do while(ireadmg(lubfr,subset,idate)==0)
      do while(ireadsb(lubfr)==0)
      call ufbcnt(lubfr,irec,isub)

      IF(msg/=' ' .and. msg/=subset) stop

      if(dump) then
         call ufdump(lubfr,6)
         goto 99
      endif

!  Move subset contents into this program
!  --------------------------------------

      call ufbint(lubfr,hdr,10,  1,iret,hstr)
      sid = transfer(hdr(1),sid)
      xob = hdr(2)
      yob = hdr(3)
      jrt = nint(hdr(6))
      jtp = nint(hdr(7))
      jkx = nint(hdr(8))
      if(sta/=' ' .and. sta/=sid(1:nsta)) cycle
      if(irt/=0   .and. irt/=jrt) cycle
      if(itp/=0   .and. itp/=jtp) cycle
      if(ikx/=0   .and. ikx/=jkx) cycle
      if(window) then
         if(.not.(xob>=x1 .and. xob<=x2))cycle
         if(.not.(yob>=y1 .and. yob<=y2))cycle
      endif

      call ufbint(lubfr,obs,10,255,nlev,ostr)
      call ufbint(lubfr,qms,10,255,nleq,qstr)
      if(nlev/=nleq) stop 'NLEV<>NLEQ'

!  Move cat 8 data to print range
!  ------------------------------
      do l=1,nlev
        if(obs(1,l)==8) then
          obs(2,l) = obs(9,l)
          obs(3,l) = obs(10,l)
        endif
      enddo

!  Print a report 20 lines at a time
!  ---------------------------------

      if(hedr) then
        print'(a8,1x,a8,7(f8.2,1x))',subset,(hdr(i),i=1,8)
        if(steam) cycle
        goto 99
      else
        print'(80(''-''))'
        print'(''MESSAGE: '',a8,2(2x,i4),i12 )' , subset,irec,isub,idate
        print'(''STATION: '',a8,1x,2(f8.2,1X))' , (hdr(i),i= 1,3)
        print'(''TIME:    '',i10,2x,f8.2     )' , idate,hdr(4)
        print'(''ELV:     '',f8.2            )' , (hdr(5)       )
        print'(''TYPE:    '',3(f8.0,1x)      )' , (hdr(i),i= 6,8)
        print'(''DATA:    ''                 )'
      endif

      do l=1,nlev
        do i=1,7
          iqm = nint(qms(i,l))
          if(iqm<0)iqm=10e8
          iqm = min(iqm,16)
          qms(i,l) = transfer(cmc(iqm+1),qms(1,1))
        enddo
      enddo

      nlne = 7
      print'(2(1x,a3),6(8x,a3))',vars
      do l=1,nlev
        nlne = nlne+1
        print 11, l,nint(obs(1,l)),(obs(i,l),qms(min(i-1,5),l),i=2,7)
11      format(2i4,6(1x,f7.1,'(',a1,')'))
      enddo
      print'(80(''-''))'
      if(steam) cycle

!  Go to read the next subset if no 'Q'
!  ------------------------------------

99    read(5,'(a)',iostat=iostat) optarg
      if(optarg(1:1)=='q') then
         stop
      elseif(optarg(1:1)=='s') then
         read(optarg(2:50),*) sta
         nsta=len_trim(sta)
      elseif(optarg(1:1)=='w') then
         read(optarg(2:50),*) x1,x2,y1,y2
         window=.true.
      elseif(optarg(1:1)=='k') then
         read(optarg(2:50),*) ikx
      elseif(optarg(1:1)=='r') then
         read(optarg(2:50),*) irt
      elseif(optarg(1:1)=='m') then
         read(optarg(2:50),*) msg
      elseif(optarg(1:1)=='d') then
         call ufdump(lubfr,6)
      elseif(optarg(1:1)=='h') then
         hedr=.true.
      endif

      enddo  ! end of subset  loop
      enddo  ! end of message loop

!  Here when all messages have been read
!  -------------------------------------

      stop
end program readbp

!> Print long lines to stdout using advance=no format clause.
!>
!> @param str String to print.
!>
!> @author J. Woollen @date 1994-01-06
subroutine printx(str)
  character(*) :: str
  lens=len(str)
  do i=1,lens-1
    write(*,'(a1)',advance="no")str(i:i)
  enddo
  write(*,'(a1)')str(lens:lens)
end subroutine
