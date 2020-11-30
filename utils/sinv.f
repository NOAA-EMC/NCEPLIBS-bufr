C> @file
C> @brief Print inventory of BUFR satellite data file by platform and
C> instrument type

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PROGRAM SINV
 
      PARAMETER (MAXA=16000000)
      PARAMETER (MAXS=1000)
 
      CHARACTER(255) FILE   
      CHARACTER(8)  SUBSET
      CHARACTER     ci*16,cj*80
      DIMENSION     isat(0:maxs,0:maxs)  
      real(8)       arr(2,maxa),said(maxa),siid(maxa)
 
      DATA BMISS  /10E10/
      DATA LUNBF  /20/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      isat=0  
      said=0
      ssid=0

      read(5,'(a)') file
      !print*; print*,file; print*
      open(lunbf,file=file,form='unformatted')
 
      CALL OPENBF(LUNBF,'IN',LUNBF)

      call ufbtab(lunbf,said,1,maxa,nret,'SAID')
      !print*,nret
      call ufbtab(lunbf,siid,1,maxa,nrex,'SIID')
      !print*,nrex

      do n=1,nret
      i = said(n)
      j = siid(n)  
      if(i>maxs.or.i<0) i=0                 
      if(j>maxs.or.j<0) j=0                 
      isat(i,j) = isat(i,j)+1
      enddo

      !print1,'satellite     ','instrument    ','   count'
      !print'(40("-"))'
      print*
1     format(a14,2x,a14,2x,a8)
      do i=0,1000
      do j=0,1000
      if(isat(i,j).gt.0) then 
         call satcode(i,ci,j,cj)
         if(ci==' ') write(ci,'(i4.4)')i
         if(cj==' ') write(cj,'(i4.4)')j
         print'(i3.3,2x,a10,2x,i10,2x,a80)',i,ci,isat(i,j),cj
      endif
      enddo
      enddo; print*

      stop
      end
c-----------------------------------------------------------------------
c  looks up BUFR code table values for SAID (said) and SIID (instrument)
c-----------------------------------------------------------------------
      subroutine satcode(icode,csad,jcode,csid)

      character(16) csad,saic(1000)
      character(80) csid,siic(1000)
      integer said,siid
      logical first /.true./

      csad=' '; csid=' '

      if(first)then
         open(8)
         do i=1,1000
         read(8,*,end=2) said,saic(said)
         enddo
2        open(9)
         do i=1,1000
         read(9,4,end=3) siid,siic(siid)
         enddo
3        first=.false.
4        format(i3,4x,a80)
      endif

c  figure out what satellite this really is

      csad=saic(icode)
      csid=siic(jcode)

      return
      end
