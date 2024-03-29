!> @file
!> @brief  Produce an inventory of subsets in a bufr file.
!>
!> @author J Woollen @date 1994

!> Usage: binv \<bufrfile\> will print bufrfile inventory by message type.
!>
!> @return 0 for success, error message otherwise.
!>
!> @author J Woollen @date 1994
PROGRAM BINV

  PARAMETER (MAXSUB=100)

  CHARACTER*255 FILE
  CHARACTER*8   SUBSET
  CHARACTER*8   SUB(MAXSUB)
  integer*8     ninv(3,maxsub)
  real*8        xsub, xmsg
  logical       exist

  DATA LUNBF  /20/

  !-----------------------------------------------------------------------
  nmbyt(lunit)= iupvs01(lunit,'LENM')
  !-----------------------------------------------------------------------

  !  get filename

  narg=command_argument_count()
  if(narg/=1) then
     print *,'Usage: binv <bufrfile> will print bufrfile inventory by message type'
     call exit(2)
  endif

  call get_command_argument(1,file)
  file = TRIM(file)//CHAR(0)
  inquire(file=file,exist=exist)
  if (.not.exist) then
     print *,trim(file)//' does not exist'
     call exit(3)
  endif
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

  print*
  print'(a4,6x,2(a10,4x),a11)','type','messages','subsets','bytes'
  print*
  DO J=1,NSUB
     xmsg = ninv(1,j)
     xsub = ninv(2,j)
     print'(a8,2x,2(i10,4x),i11,4x,f8.2)',sub(j),(ninv(i,j),i=1,3),xsub/xmsg
     IF(J.GT.1) THEN
        NINV(1,1) = NINV(1,1)+NINV(1,J)
        NINV(2,1) = NINV(2,1)+NINV(2,J)
        NINV(3,1) = NINV(3,1)+NINV(3,J)
     ENDIF
  ENDDO

  print'(a8,2x,2(i10,4x),i11,4x)','TOTAL   ',(ninv(i,1),i=1,3)
  print*

  STOP
END PROGRAM BINV
