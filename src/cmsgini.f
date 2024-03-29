C> @file
C> @brief Initialize a new compressed BUFR message for output.
C>
C> @author Woollen @date 2002-05-14

C> Initialize a new BUFR message for output in compressed format.
C>
C> @param[in] LUN - integer: file ID.
C> @param[out] MESG - integer(*): BUFR message.
C> @param[in] SUBSET - character*8: Table A mnemonic for type of BUFR message being written.
C> @param[in] IDATE - integer: date-time stored within Section 1 of BUFR message being written,
C>                    in format of either YYMMDDHH or YYYYMMDDHH, depending on datelen() value.
C> @param[in] NSUB - integer: number of subsets in MESG
C> @param[inout] NBYT - integer:
C>                      - On input, contains the length (in bytes) of Section 4, except for
C>                        the first 4 bytes
C>                      - On output, contains the length (in bytes) of the entire BUFR message, up
C>                        to the point in Section 4 where compressed data are to be written
C>
C> @author Woollen @date 2002-05-14

      SUBROUTINE CMSGINI(LUN,MESG,SUBSET,IDATE,NSUB,NBYT)

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET
      CHARACTER*4   BUFR
      CHARACTER*1   TAB
      DIMENSION     MESG(*)

      DATA BUFR/'BUFR'/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  GET THE MESSAGE TAG AND TYPE, AND BREAK UP THE DATE
C  ---------------------------------------------------

c  .... Given SUBSET, NEMTBA returns MTYP,MSBT,INOD
      CALL NEMTBA(LUN,SUBSET,MTYP,MSBT,INOD)
      CALL NEMTAB(LUN,SUBSET,ISUB,TAB,IRET)
      IF(IRET.EQ.0) GOTO 900

C  DATE CAN BE YYMMDDHH OR YYYYMMDDHH
C  ----------------------------------

      JDATE = I4DY(IDATE)
      MCEN = MOD(JDATE/10**8,100)+1
      MEAR = MOD(JDATE/10**6,100)
      MMON = MOD(JDATE/10**4,100)
      MDAY = MOD(JDATE/10**2,100)
      MOUR = MOD(JDATE      ,100)
      MMIN = 0

c  .... DK: Don't think this can happen, because IDATE=0 is returned
c           as 2000000000 by I4DY meaning MCEN would be 21
      IF(MCEN.EQ.1) GOTO 901

      IF(MEAR.EQ.0) MCEN = MCEN-1
      IF(MEAR.EQ.0) MEAR = 100

C  INITIALIZE THE MESSAGE
C  ----------------------

      MBIT = 0

C  SECTION 0
C  ---------

      CALL PKC(BUFR ,  4 , MESG,MBIT)

C     NOTE THAT THE ACTUAL SECTION 0 LENGTH WILL BE COMPUTED AND
C     STORED BELOW; FOR NOW, WE ARE REALLY ONLY INTERESTED IN
C     ADVANCING MBIT BY THE CORRECT AMOUNT, SO WE'LL JUST STORE
C     A DEFAULT VALUE OF 0.

      CALL PKB(   0 , 24 , MESG,MBIT)
      CALL PKB(   3 ,  8 , MESG,MBIT)

C  SECTION 1
C  ---------

      LEN1 = 18

      CALL PKB(LEN1 , 24 , MESG,MBIT)
      CALL PKB(   0 ,  8 , MESG,MBIT)
      CALL PKB(   3 ,  8 , MESG,MBIT)
      CALL PKB(   7 ,  8 , MESG,MBIT)
      CALL PKB(   0 ,  8 , MESG,MBIT)
      CALL PKB(   0 ,  8 , MESG,MBIT)
      CALL PKB(MTYP ,  8 , MESG,MBIT)
      CALL PKB(MSBT ,  8 , MESG,MBIT)
      CALL PKB(  36 ,  8 , MESG,MBIT)
      CALL PKB(   0 ,  8 , MESG,MBIT)
      CALL PKB(MEAR ,  8 , MESG,MBIT)
      CALL PKB(MMON ,  8 , MESG,MBIT)
      CALL PKB(MDAY ,  8 , MESG,MBIT)
      CALL PKB(MOUR ,  8 , MESG,MBIT)
      CALL PKB(MMIN ,  8 , MESG,MBIT)
      CALL PKB(MCEN ,  8 , MESG,MBIT)

C  SECTION 3
C  ---------

      LEN3 = 10

      CALL PKB(LEN3 , 24 , MESG,MBIT)
      CALL PKB(   0 ,  8 , MESG,MBIT)
      CALL PKB(NSUB , 16 , MESG,MBIT)
      CALL PKB( 192 ,  8 , MESG,MBIT)
      CALL PKB(ISUB , 16 , MESG,MBIT)
      CALL PKB(   0 ,  8 , MESG,MBIT)

C  SECTION 4
C  ---------

C     STORE THE TOTAL LENGTH OF SECTION 4.

C     REMEMBER THAT THE INPUT VALUE OF NBYT ONLY CONTAINS THE
C     LENGTH OF THE "COMPRESSED DATA PORTION" OF SECTION 4, SO
C     WE NEED TO ADD FOUR BYTES TO THIS NUMBER IN ORDER TO
C     ACCOUNT FOR THE TOTAL LENGTH OF SECTION 4.

      CALL PKB((NBYT+4) , 24 , MESG,MBIT)
      CALL PKB(       0 ,  8 , MESG,MBIT)

C     THE ACTUAL "COMPRESSED DATA PORTION" OF SECTION 4 WILL
C     BE FILLED IN LATER BY SUBROUTINE WRCMPS.

C  SECTION 5
C  ---------

C     THIS SECTION WILL BE FILLED IN LATER BY SUBROUTINE WRCMPS.

C  RETURN WITH THE CORRECT NEW MESSAGE BYTE COUNT
C  ----------------------------------------------

C     NOW, NOTING THAT MBIT CURRENTLY POINTS TO THE LAST BIT OF
C     THE FOURTH BYTE OF SECTION 4, THEN WE HAVE:
C     (TOTAL LENGTH OF BUFR MESSAGE (IN SECTION 0)) =
C            (LENGTH OF MESSAGE UP THROUGH FOURTH BYTE OF SECTION 4)
C         +  (LENGTH OF "COMPRESSED DATA PORTION" OF SECTION 4)
C         +  (LENGTH OF SECTION 5)
      MBYT =
     .       MBIT/8
     .    +  NBYT
     .    +  4

C     NOW, MAKE NBYT POINT TO THE CURRENT LOCATION OF MBIT
C     (I.E. THE BYTE AFTER WHICH TO ACTUALLY BEGIN WRITING THE
C     COMPRESSED DATA INTO SECTION 4).

      NBYT = MBIT/8

C     NOW, STORE THE TOTAL LENGTH OF THE BUFR MESSAGE (IN SECTION 0).

      MBIT = 32
      CALL PKB(MBYT,24,MESG,MBIT)

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: CMSGINI - TABLE A MESSAGE TYPE '//
     . 'MNEMONIC ",A," NOT FOUND IN INTERNAL TABLE D ARRAYS")') SUBSET
      CALL BORT(BORT_STR)
901   CALL BORT
     . ('BUFRLIB: CMSGINI - BUFR MESSAGE DATE (IDATE) is 0000000000')
      END
