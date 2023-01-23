!> @file
!> @brief  Produce an inventory of subsets in a bufr file.
!>
!> @author J Woollen @date 1994

!> Usage: binv <bufrfile> will print bufrfile inventory by message type.
!>
!> @return 0 for success, error message otherwise.
!>
!> @author J Woollen @date 1994
PROGRAM BINV

  PARAMETER (MAXSUB=100)

  CHARACTER*255 FILE
  CHARACTER*8   SUBSET
  CHARACTER*8   SUB(MAXSUB)
  DIMENSION     NINV(3,MAXSUB)
  LOGICAL       EXIST

  DATA BMISS  /10E10/
  DATA LUNBF  /20/

  !-----------------------------------------------------------------------
  nmbyt(lunit)= iupvs01(lunit,'LENM')
  !-----------------------------------------------------------------------

  !  get filename

  NARG=IARGC()
  IF(NARG/=1) THEN
     PRINT *,'Usage: binv <bufrfile> will print bufrfile inventory by message type'
     CALL EXIT(2)
  ENDIF

  call getarg(1,file)
  file = TRIM(file)//CHAR(0)
  inquire(file=file,exist=exist)
  if (.not.exist) call bort(trim(file)//' does not exist')
  open(lunbf,file=file,form='unformatted')

  NINV = 0
  NSUB = 0


  !  COMPUTE AN MESSAGE INVENTORY BY SUBSETS
  !  ---------------------------------------

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
  ENDDO

  !  PRINT THE INVENTORY
  !  -------------------

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
END PROGRAM BINV
