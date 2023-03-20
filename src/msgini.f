C> @file
C> @brief Initialize a new uncompressed BUFR message for output.
C>
C> @author Woollen @date 1994-01-06

C> This subroutine initializes, within the internal arrays, a new
C> uncompressed BUFR message for output. Arrays are filled in common blocks
C> msgptr and modules @ref moda_msgcwd and @ref moda_bitbuf.
C>
C> @param[in] LUN - integer: I/O stream index into internal memory arrays.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE MSGINI(LUN)

      USE MODA_MSGCWD
      USE MODA_UFBCPL
      USE MODA_BITBUF
      USE MODA_TABLES

      COMMON /PADESC/ IBCT,IPD1,IPD2,IPD3,IPD4
      COMMON /MSGPTR/ NBY0,NBY1,NBY2,NBY3,NBY4,NBY5

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBTAG
      CHARACTER*4   BUFR,SEVN
      CHARACTER*1   TAB

      DATA BUFR/'BUFR'/
      DATA SEVN/'7777'/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  GET THE MESSAGE TAG AND TYPE, AND BREAK UP THE DATE
C  ---------------------------------------------------

      SUBTAG = TAG(INODE(LUN))
c  .... Given SUBSET, NEMTBA returns MTYP,MSBT,INOD
      CALL NEMTBA(LUN,SUBTAG,MTYP,MSBT,INOD)
      IF(INODE(LUN).NE.INOD) GOTO 900
      CALL NEMTAB(LUN,SUBTAG,ISUB,TAB,IRET)
      IF(IRET.EQ.0) GOTO 901

C  DATE CAN BE YYMMDDHH OR YYYYMMDDHH
C  ----------------------------------

      MCEN = MOD(IDATE(LUN)/10**8,100)+1
      MEAR = MOD(IDATE(LUN)/10**6,100)
      MMON = MOD(IDATE(LUN)/10**4,100)
      MDAY = MOD(IDATE(LUN)/10**2,100)
      MOUR = MOD(IDATE(LUN)      ,100)
      MMIN = 0

c  .... DK: Can this happen?? (investigate)
      IF(MCEN.EQ.1) GOTO 902

      IF(MEAR.EQ.0) MCEN = MCEN-1
      IF(MEAR.EQ.0) MEAR = 100

C  INITIALIZE THE MESSAGE
C  ----------------------

      MBIT = 0
      NBY0 = 8
      NBY1 = 18
      NBY2 = 0
      NBY3 = 20
      NBY4 = 4
      NBY5 = 4
      NBYT = NBY0+NBY1+NBY2+NBY3+NBY4+NBY5

C  SECTION 0
C  ---------

      CALL PKC(BUFR ,  4 , MBAY(1,LUN),MBIT)
      CALL PKB(NBYT , 24 , MBAY(1,LUN),MBIT)
      CALL PKB(   3 ,  8 , MBAY(1,LUN),MBIT)

C  SECTION 1
C  ---------

      CALL PKB(NBY1 , 24 , MBAY(1,LUN),MBIT)
      CALL PKB(   0 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(   3 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(   7 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(   0 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(   0 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(MTYP ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(MSBT ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(  36 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(   0 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(MEAR ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(MMON ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(MDAY ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(MOUR ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(MMIN ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(MCEN ,  8 , MBAY(1,LUN),MBIT)

C  SECTION 3
C  ---------

      CALL PKB(NBY3 , 24 , MBAY(1,LUN),MBIT)
      CALL PKB(   0 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(   0 , 16 , MBAY(1,LUN),MBIT)
      CALL PKB(2**7 ,  8 , MBAY(1,LUN),MBIT)
      CALL PKB(IBCT , 16 , MBAY(1,LUN),MBIT)
      CALL PKB(ISUB , 16 , MBAY(1,LUN),MBIT)
      CALL PKB(IPD1 , 16 , MBAY(1,LUN),MBIT)
      CALL PKB(IPD2 , 16 , MBAY(1,LUN),MBIT)
      CALL PKB(IPD3 , 16 , MBAY(1,LUN),MBIT)
      CALL PKB(IPD4 , 16 , MBAY(1,LUN),MBIT)
      CALL PKB(   0 ,  8 , MBAY(1,LUN),MBIT)

C  SECTION 4
C  ---------

      CALL PKB(NBY4 , 24 , MBAY(1,LUN),MBIT)
      CALL PKB(   0 ,  8 , MBAY(1,LUN),MBIT)

C  SECTION 5
C  ---------

      CALL PKC(SEVN ,  4 , MBAY(1,LUN),MBIT)

C  DOUBLE CHECK INITIAL MESSAGE LENGTH
C  -----------------------------------

      IF(MOD(MBIT,8).NE.0) GOTO 903
      IF(MBIT/8.NE.NBYT  ) GOTO 904

      NMSG(LUN) = NMSG(LUN)+1
      NSUB(LUN) = 0
      MBYT(LUN) = NBYT

      LUNCPY(LUN)=0

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: MSGINI - MISMATCH BETWEEN INODE (=",'//
     . 'I7,") & POSITIONAL INDEX, INOD (",I7,") OF SUBTAG (",A,") IN '//
     . 'DICTIONARY")') INODE(LUN),INOD,SUBTAG
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: MSGINI - TABLE A MESSAGE TYPE '//
     . 'MNEMONIC ",A," NOT FOUND IN INTERNAL TABLE D ARRAYS")') SUBTAG
      CALL BORT(BORT_STR)
902   CALL BORT
     . ('BUFRLIB: MSGINI - BUFR MESSAGE DATE (IDATE) is 0000000000')
903   CALL BORT('BUFRLIB: MSGINI - INITIALIZED MESSAGE DOES NOT END '//
     . 'ON A BYTE BOUNDARY')
904   WRITE(BORT_STR,'("BUFRLIB: MSGINI - NUMBER OF BYTES STORED FOR '//
     . 'INITIALIZED MESSAGE (",I6,") IS NOT THE SAME AS FIRST '//
     . 'CALCULATED, NBYT (",I6)') MBIT/8,NBYT
      CALL BORT(BORT_STR)
      END
