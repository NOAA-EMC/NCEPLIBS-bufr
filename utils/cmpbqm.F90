!> @file
!> @brief Print inventory of observations from prepbufr file by variable, report type and quality mark.
!>
!> @author J Woollen @date 1997

!> Usage: cmpbqm \<prepbufrfile\> will print prep inventory by variable, report type, and qc mark.
!>
!> @return 0 for success, error message otherwise.
!>
!> @author J Woollen @date 1997
program cmpbqm

  character*255 file
  character*20, parameter :: vars(7) = &
    (/ 'PRESSURE            ', &
       'SPECIFIC HUMIDTY    ', &
       'TEMPERATURE         ', &
       'HEIGHT              ', &
       'WIND COMPONENTS     ', &
       'PRECIPITABLE H2O    ', &
       'RELATIVE HUMIDTY    ' /)
  character*8  subset,date
  dimension    knt(300,7,0:17),hdr(5),obs(8,255),qms(8,255)
  logical      exist
  real*8       hdr,obs,qms
  real*8, parameter :: vmax = 10E10

  integer, parameter :: lubfr = 8

  character*(*), parameter :: headr = 'SID XOB YOB DHR TYP'
  character*(*), parameter :: obstr = 'POB QOB TOB ZOB UOB PWO RHO VOB'
  character*(*), parameter :: qmstr = 'PQM QQM TQM ZQM WQM PWQ RHQ'

  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------

  irec = 0
  knt = 0

  !  Open a file - get a date
  !  ------------------------

  call get_command_argument(1,file); file=trim(adjustl(file))
  if (file == '') then
     print *, 'Usage: Usage: cmpbqm <prepbufrfile> will print prep inventory by variable, report type, and qc mark'
     stop 2
  endif
  inquire(file=file,exist=exist)
  if (.not.exist) then
     print *, trim(file)//' does not exist'
     stop 3
  endif

  open(lubfr,file=file,form='unformatted')
  call openbf(lubfr,'IN',lubfr)
  call readmg(lubfr,subset,idate,iret)
  if(iret/=0) call bort('CMPBQM - ERROR READING BUFR FILE ')
  write(date,'(i8)') idate
  do i=1,8
     if(date(i:i)==' ') date(i:i) = '0'
  enddo
  print'(''DATA  VALID AT  '',A8)',date

  !  Read thru the prepda records
  !  ----------------------------

10 call readsb(lubfr,iret)
  if(iret/=0) then
     call readmg(lubfr,subset,idate,iret)
     if(iret/=0) goto 100
     call ufbcnt(lubfr,irec,isub)
     goto 10
  endif
  qms = 10E10
  call ufbint(lubfr,hdr,5,1,iret,headr)
  call ufbint(lubfr,obs,8,255,nlev,obstr)
  call ufbint(lubfr,qms,8,255,nlev,qmstr)

  kx = nint(hdr(5))

  do l=1,nlev
     do k=1,7
        iq = -1
        if(k==5) obs(5,l) = max(obs(5,l),obs(8,l))
        if(obs(k,l)<vmax .and. qms(k,l)<vmax) then
           iq = nint(qms(k,l))
        elseif(obs(k,l)<vmax .and. qms(k,l)>=vmax) then
           iq = 16
        elseif(obs(k,l)>=vmax .and. qms(k,l)<vmax) then
           iq = 17
        endif
        if(iq>=0) knt(kx,k,iq) = knt(kx,k,iq)+1
     enddo
  enddo

  goto 10

  !  Finish up
  !  ---------

100 do k=1,7
     print*,vars(k)
     print*
     do kx=1,300
        itot = 0; igood=0; ifail=0
        do iq=0,17
           itot = itot+knt(kx,k,iq)
           if(iq<=3) then
              igood=igood+knt(kx,k,iq)
           elseif(iq<=7) then
              ifail=ifail+knt(kx,k,iq)
           endif
        enddo
        if(itot>0) print 101,kx,itot,igood,ifail,(knt(kx,k,iq),iq=8,17)
101     format(i3,i6,2('|', i6),&
             2('|', i6),&
             1('|',6i6),&
             2('|', i6))
     enddo
     print*
  enddo

  print*,'******CMPBQM PROCESSED ',IREC,' BUFR RECORDS******'
  stop
end program cmpbqm
