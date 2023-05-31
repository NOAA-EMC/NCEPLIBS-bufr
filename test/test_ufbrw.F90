! This is a test for NCEPLIBS-bufr library subroutine ufbrw and its callees.
!
! This tests reads and writes a testfile exercising the mechanics in subroutine
! ufbrw which locate and extract or install values from or into the bufr
! testfile and test various user defined options available through subroutine
! ufbint which is the driver of ufbrw.
!
! J Woollen  4/14/23  

      program test_ufbrw

      character(255)file     
      character(55) brr(56),line,str1,str2
      character(20) cond    
      character(8)  subset
      real(8)       arr(10,255)

      data brr( 1) /"POB<1000"/
      data brr( 2) /"985.20 ******** ********    -2.80    -7.70"/
      data brr( 3) /"985.20 12968.00    31.30 ******** ********"/
      data brr( 4) /"998.00 ******** ********     4.60     2.20"/
      data brr( 5) /"850.00 ******** ********     2.00    -1.70"/
      data brr( 6) /"700.00 ******** ********    12.10    -4.40"/
      data brr( 7) /"998.00  8112.00    22.30 ******** ********"/
      data brr( 8) /"925.00  6312.00    14.10 ******** ********"/
      data brr( 9) /"850.00  2161.00    14.80 ******** ********"/
      data brr(10) /"700.00  2131.00     9.20 ******** ********"/
      data brr(11) /"300.00 ******** ******** ******** ********"/
      data brr(12) /""/
      data brr(13) /"POB<1000 UOB<1000"/
      data brr(14) /"985.20 ******** ********    -2.80    -7.70"/
      data brr(15) /"998.00 ******** ********     4.60     2.20"/
      data brr(16) /"850.00 ******** ********     2.00    -1.70"/
      data brr(17) /"700.00 ******** ********    12.10    -4.40"/
      data brr(18) /""/
      data brr(19) /"POB<800  TOB<1000"/
      data brr(20) /"700.00  2131.00     9.20 ******** ********"/
      data brr(21) /""/
      data brr(22) /"POB<851  POB>699"/
      data brr(23) /"850.00 ******** ********     2.00    -1.70"/
      data brr(24) /"700.00 ******** ********    12.10    -4.40"/
      data brr(25) /"850.00  2161.00    14.80 ******** ********"/
      data brr(26) /"700.00  2131.00     9.20 ******** ********"/
      data brr(27) /""/
      data brr(28) /"POB=850"/
      data brr(29) /"850.00 ******** ********     2.00    -1.70"/
      data brr(30) /"850.00  2161.00    14.80 ******** ********"/
      data brr(31) /""/
      data brr(32) /""/
      data brr(33) /"read/write from unit 20"/
      data brr(34) /"985.20 ******** ********    -2.80    -7.70"/
      data brr(35) /"985.20 12968.00    31.30 ******** ********"/
      data brr(36) /"998.00 ******** ********     4.60     2.20"/
      data brr(37) /"850.00 ******** ********     2.00    -1.70"/
      data brr(38) /"700.00 ******** ********    12.10    -4.40"/
      data brr(39) /"998.00  8112.00    22.30 ******** ********"/
      data brr(40) /"925.00  6312.00    14.10 ******** ********"/
      data brr(41) /"850.00  2161.00    14.80 ******** ********"/
      data brr(42) /"700.00  2131.00     9.20 ******** ********"/
      data brr(43) /"300.00 ******** ******** ******** ********"/
      data brr(44) /""/
      data brr(45) /"read/write from unit 50"/
      data brr(46) /"985.20 ******** ********    -2.80    -7.70"/
      data brr(47) /"985.20 12968.00    31.30 ******** ********"/
      data brr(48) /"998.00 ******** ********     4.60     2.20"/
      data brr(49) /"850.00 ******** ********     2.00    -1.70"/
      data brr(50) /"700.00 ******** ********    12.10    -4.40"/
      data brr(51) /"998.00  8112.00    22.30 ******** ********"/
      data brr(52) /"925.00  6312.00    14.10 ******** ********"/
      data brr(53) /"850.00  2161.00    14.80 ******** ********"/
      data brr(54) /"700.00  2131.00     9.20 ******** ********"/
      data brr(55) /"300.00 ******** ******** ******** ********"/

      print *, 'Testing ufbrw and helper routines.'

#ifdef KIND_8
      call setim8b(.true.)
#endif

! set the name of the bufr testfile to process

      file="testfiles/data/prepbufr2.ref"

! open the print file to record ufbrep test results

      open(55,file='ufbrw_prnt_out')

! test various reading options applying user specified filtering, testing conwin

      do i=1,5

      if(i==1) cond='POB<1000         '
      if(i==2) cond='POB<1000 UOB<1000'
      if(i==3) cond='POB<800  TOB<1000'
      if(i==4) cond='POB<851  POB>699 '
      if(i==5) cond='POB=850          '
      write(55,*) cond

! set the filename and open and read the testfile with various conditions specified
! print the results in a text file for verification

      open(20,file=file,form='unformatted')
      call openbf(20,'IN',20)

      do while(ireadmg(20,subset,idate)==0)
        do while(ireadsb(20)==0)
          call ufbint(20,arr,10,255,irt,cond//' POB QOB TOB UOB VOB')
          if(irt>0) write(55,'(5(1x,f8.2))')arr(1:5,1:irt)
        enddo
      enddo
      call closbf(20)
      write(55,*)
      enddo

! copy the testfile to test helpers getwin, invwin, newwin, nxtwin in output more
! print the results in a text file for verification

      open(20,file=file,form='unformatted')
      open(50,file='ufbrw_bufr_out',form='unformatted')

      call openbf(20,'IN ',20)
      call openbf(50,'OUT',20)
      
      write(55,*);write(55,*)'read/write from unit 20'

      do while(ireadmg(20,subset,idate)==0)
        do while(ireadsb(20)==0)
          call ufbint(20,arr,10,255,irt,'POB QOB TOB UOB VOB')
          write(55,'(5(1x,f8.2))')arr(1:5,1:irt)
          call openmb(50,subset,idate)
          call ufbint(50,arr,10,irt,jrt,'POB QOB TOB UOB VOB')
          call writsb(50)
        enddo
      enddo

! read the testfile copy and print the results in a text file for verification

      call closbf(50)
      open(50,file='ufbrw_bufr_out',form='unformatted')
      call openbf(50,'IN',50) 

      write(55,*);write(55,*)'read/write from unit 50'

      do while(ireadmg(50,subset,idate)==0)
        do while(ireadsb(50)==0)
          call ufbint(50,arr,10,255,irt,'POB QOB TOB UOB VOB')
          write(55,'(5(1x,f8.2))')arr(1:5,1:irt)
        enddo
      enddo

! close the testfile for writing and open it for reading

      close(55)
      open(55,file='ufbrw_prnt_out')

! verify the testfile contents against output stored in brr array strings

      do n=1,56     
        read(55,'(a55)',iostat=iret) line
        call strsuc(line  ,str1,len1)
        call strsuc(brr(n),str2,len2)
        if(n<=55.and.iret==0.and.str1/=str2) then
          print*,"str1:",str1  
          print*,"str2:",str2 
          stop 56 
        elseif(n>55.and.iret==0) then
          stop 56 
        endif
      enddo

! successful exit

      print*,'success'
      end program

