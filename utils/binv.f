C> @file
C> @brief Print inventory of BUFR file by message type

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PROGRAM BINV

      use bufr_procedures
 
      PARAMETER (MAXSUB=100)
 
      CHARACTER*200 FILE   
      CHARACTER*8   SUBSET
      CHARACTER*8   SUB(MAXSUB)
      DIMENSION     NINV(3,MAXSUB)
 
      DATA BMISS  /10E10/
      DATA LUNBF  /20/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      NINV = 0
      NSUB = 0

      read(5,'(a)') file
      open(lunbf,file=file,form='unformatted')
 
C  COMPUTE AN MESSAGE INVENTORY BY SUBSETS
C  ---------------------------------------
 
      CALL OPENBF(LUNBF,'IN',LUNBF)
      DO WHILE(IREADMG(LUNBF,SUBSET,IDATE).EQ.0)
      ISUB = 0
      DO I=1,NSUB
      IF(SUBSET.EQ.SUB(I)) ISUB = I
      ENDDO
      IF(ISUB.EQ.0) THEN
         IF(NSUB+1.GT.MAXSUB) CALL BORT('NSUB TOO BIG')
         SUB(NSUB+1) = SUBSET
         NSUB = NSUB+1
         ISUB = NSUB
      ENDIF
      NINV(1,ISUB) = NINV(1,ISUB)+1
      NINV(2,ISUB) = NINV(2,ISUB)+NMSUB(LUNBF)
      NINV(3,ISUB) = NINV(3,ISUB)+NMBYT(LUNBF)
 
c     IOFF = 1
c     CALL STATUS(LUNBF,LUN,IL,IM)
c     call ufbcnt(lunbf,irec,isub)
c     DO I=1,NMSUB(LUNBF)
c     NBYS = IUPB(MBAY(1,LUN),MBYT(LUN)+IOFF,16)
c     print*,SUBSET,' m#',irec,' subt#',i,nbys
c     IOFF = IOFF+NBYS
c     ENDDO
      ENDDO

C  PRINT THE INVEBTORY
C  -------------------
 
      PRINT*
      PRINT'(a4,6x,3(a10,4x))','type','messages','subsets','bytes'
      PRINT*
      DO J=1,NSUB
      xmsg = ninv(1,j)
      xsub = ninv(2,j)
      PRINT'(A8,2X,3(I10,4X),f8.2)',SUB(J),(NINV(I,J),I=1,3),xsub/xmsg
      IF(J.GT.1) THEN
         NINV(1,1) = NINV(1,1)+NINV(1,J)
         NINV(2,1) = NINV(2,1)+NINV(2,J)
         NINV(3,1) = NINV(3,1)+NINV(3,J)
      ENDIF
      ENDDO

      PRINT'(A8,2X,3(I10,4X))','TOTAL   ',(NINV(I,1),I=1,3)
      PRINT*
 
      STOP
      END
      function nmbyt(lunit)
      nmbyt = iupvs01(lunit,'LENM')
      return
      end
