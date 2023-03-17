! This is a test for NCEPLIBS-bufr.
!
! This is a test for the newwin() subroutine.
!
! Ed Hartnett, J. Woollen, 3/15/2023
program newwint
  use moda_usrint
  use moda_tables
!  implicit none
  integer idate, ins1, ins2, iret, iwin, jwin, lun, m, n
  character*8 subset
  integer msg_num, NUM_MSG
  parameter(NUM_MSG = 12)
  real(8) ncdt(5,50)
  common /usrstr/ nnod,ncon,nods(20),nodc(10),ivls(10),kons(10)

  print *,'Testing newwin()...'
  
  ! open a file 
  open(unit = 20, file = 'testfiles/IN_2', form = 'UNFORMATTED')
  call openbf(20,'IN',20)

  ! Read each message in the file.
  do msg_num = 1, NUM_MSG
     iret = ireadmg(20,subset,idate)
     if (iret .ne. 0) stop 1
  end do

  ! read the first message and subset
  do while(ireadmg(20,subset,idate).eq.0)
     do while(ireadsb(20).eq.0)
        print*,subset,' message date=',i4dy(idate)
        call ufbint(20,ncdt,5,50,iret,"RCYR RCMO RCDY RCHR RCMI")
        print'(5(f5.0))',ncdt(:,1:iret)

        ! setup the window environment
        lun=1
        ins1=1
        ins2=nval(lun)

        ! cycle through the windows of the replicated sequence
        ! the output can be tested against stored answers
        do n=1,nnod
           print*,tag(nods(n)),iret
           if(tag(nods(n))=='RCHR') then
              iwin = jmpb(nods(n))
              do m=1,iret
                 print*,iwin,jwin
                 call newwin(lun,iwin,jwin)
                 iwin=jwin
              enddo
              exit
           endif
        enddo
        stop

        ! end of read loops - end of program
     enddo
  enddo
  
  print *,'SUCCESS!'
end program newwint
