!-----------------------------------------------------------------------
! READ AND DISPLAY AN ON29BUFR FILE ONE REPORT AT A TIME
!-----------------------------------------------------------------------
      PROGRAM READBP

      character(120) ::  FILE
      character(50)  ::  optarg
      character(40)  ::  HSTR,OSTR,QSTR
      character(10)  ::  val
      character(8)   ::  sid,sta,subset,msg,cmc(17)
      character(3)   ::  vars(8)
      integer        ::  iostat
      real(8)        ::  hdr(10),obs(10,255),qms(10,255),qmc(17)
      logical        ::  window,steam,level,dump,hedr,exists

      EQUIVALENCE    (HDR(1),SID)
      EQUIVALENCE    (qmc,cmc)

      DATA HSTR/'SID XOB YOB DHR ELV T29 ITP TYP SRC PRG '/
      DATA OSTR/'CAT POB QOB TOB ZOB UOB VOB PSL         '/
      DATA QSTR/'PQM QQM TQM ZQM WQM PSQ                 '/

      DATA VARS/'LVL','CAT','POB','SPH','TOB','ZOB','UOB','VOB'/
      DATA CMC /'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','*'/

      DATA BMISS  /10d10/
      DATA LUBFR  /8    /
      DATA STA    /'   '/
      data msg    /'   '/
      DATA POB    /0/
      data irt    /0/
      data itp    /0/
      data ikx    /0/
      DATA WINDOW /.FALSE./
      DATA STEAM  /.FALSE./
      DATA LEVEL  /.FALSE./

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  check for filename argument

      narg=iargc()
1     if(narg<1) THEN
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
        call exit(2)
      ENDIF

      iarg=1
      do while(iarg<=narg)
      call getarg(iarg,file)
      if(file(1:1)=='-') then
         if(file(2:2)=='s') then
           iarg=iarg+1; call getarg(iarg,sta); nsta=len(trim(sta))
         elseif(file(2:2)=='w') then
           iarg=iarg+1; call getarg(iarg,val); read(val,*)x1
           iarg=iarg+1; call getarg(iarg,val); read(val,*)x2
           iarg=iarg+1; call getarg(iarg,val); read(val,*)y1
           iarg=iarg+1; call getarg(iarg,val); read(val,*)y2
           window=.true.
         elseif(file(2:2)=='k') then
           iarg=iarg+1; call getarg(iarg,val); read(val,*)ikx
         elseif(file(2:2)=='r') then
           iarg=iarg+1; call getarg(iarg,val); read(val,*)irt
         elseif(file(2:2)=='m') then
           iarg=iarg+1; call getarg(iarg,val); msg=val
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


! check if file exists, then open it, else abort

      narg=0
      if(file=='nofile') goto 1 
      file = trim(adjustl(file)) 
      inquire(file=file, exist=exists)
      if (.not.exists) goto 1

!  open the bufr input file
!  ------------------------

      open(lubfr,file=file,form='unformatted')
      call openbf(lubfr,'IN',lubfr)
      call datelen(10)

!  READ A SUBSET - READ ANOTHER MESSAGE WHEN NO MORE SUBSETS
!  ---------------------------------------------------------

      do while(ireadmg(lubfr,subset,idate)==0)
      do while(ireadsb(lubfr)==0)
      call ufbcnt(lubfr,irec,isub)

      IF(msg.ne.' ' .and. msg.ne.subset) exit

      if(dump) then
         call ufdump(lubfr,6)
         goto 99
      endif

!  MOVE SUBSET CONTENTS INTO THIS PROGRAM
!  --------------------------------------

      CALL UFBINT(LUBFR,HDR,10,  1,IRET,HSTR)
      XOB = HDR(2)
      YOB = HDR(3)
      jrt = hdr(6)
      jtp = hdr(7)
      jkx = hdr(8)
      IF(STA.NE.' ' .AND. STA.NE.SID(1:nsta)) cycle
      IF(irt.ne.0   .and. irt.ne.jrt) cycle
      IF(itp.ne.0   .and. itp.ne.jtp) cycle
      IF(ikx.ne.0   .and. ikx.ne.jkx) cycle
      IF(WINDOW) THEN
         IF(.NOT.(XOB.GE.X1 .AND. XOB.LE.X2))cycle
         IF(.NOT.(YOB.GE.Y1 .AND. YOB.LE.Y2))cycle
      ENDIF

      CALL UFBINT(LUBFR,OBS,10,255,NLEV,OSTR)
      CALL UFBINT(LUBFR,QMS,10,255,NLEQ,QSTR)
      IF(NLEV.NE.NLEQ) STOP 'NLEV<>NLEQ'

!  MOVE CAT 8 DATA TO PRINT RANGE
!  ------------------------------
      DO L=1,NLEV
      IF(OBS(1,L).EQ.8) THEN
         OBS(2,L) = OBS(9,L)
         OBS(3,L) = OBS(10,L)
      ENDIF
      ENDDO

!  PRINT A REPORT 20 LINES AT A TIME
!  ---------------------------------

      if(hedr) then
      print'(a8,1x,a8,7(f8.2,1x))',subset,(hdr(i),i=1,8)
      if(steam) cycle
      goto 99

      else

      print'(80(''-''))'
      PRINT'(''MESSAGE: '',A8,2(2X,I4),i12 )' , SUBSET,IREC,ISUB,idate
      PRINT'(''STATION: '',A8,1X,2(F8.2,1X))' , (HDR(I),I= 1,3)
      PRINT'(''TIME:    '',I10,2x,F8.2     )' , IDATE,HDR(4)
      PRINT'(''ELV:     '',F8.2            )' , (HDR(5)       )
      PRINT'(''TYPE:    '',3(F8.0,1X)      )' , (HDR(I),I= 6,8)
      PRINT'(''DATA:    ''                 )'

      endif

      do l=1,nlev
      do i=1,7
      iqm = qms(i,l)
      if(iqm<0)iqm=10e8
      iqm = min(iqm,16)
      qms(i,l) = qmc(iqm+1)
      enddo
      enddo

      NLNE = 7
      PRINT'(2(1X,A3),6(8X,A3))',VARS
      DO 12 L=1,NLEV
      NLNE = NLNE+1
      PRINT 11, L,NINT(OBS(1,L)),(OBS(I,L),QMS(MIN(I-1,5),L),I=2,7)
11    FORMAT(2I4,6(1X,F7.1,'(',A1,')'))
12    ENDDO
      PRINT'(80(''-''))'
      if(steam) cycle

!  GO TO READ THE NEXT SUBSET IF NO 'Q' YOU
!  ----------------------------------------

99    READ(5,'(a)',iostat=iostat) optarg
      IF(optarg(1:1)=='q') then
         stop
      elseif(optarg(1:1)=='s') then
         read(optarg(2:50),*) sta
         nsta=len(trim(sta))
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

!  HERE WHEN ALL MESSAGES HAVE BEEN READ
!  -------------------------------------

100   STOP
      END program
!-----------------------------------------------------------------------
! print long lines to stdout using advance=no format clause
!-----------------------------------------------------------------------
      subroutine printx(str)
      character(*) :: str
      lens=len(str)
      do i=1,lens-1             
      write(*,'(a1)',advance="no")str(i:i)  
      enddo
      write(*,'(a1)')str(lens:lens)  
      end subroutine

