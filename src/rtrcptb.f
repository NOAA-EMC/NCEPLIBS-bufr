      SUBROUTINE RTRCPTB(MBAY,IYR,IMO,IDY,IHR,IMI,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RTRCPTB
C   PRGMMR: ATOR            ORG: NP12       DATE: 2013-10-07
C
C ABSTRACT: THIS SUBROUTINE RETURNS THE TANK RECEIPT TIME STORED WITHIN
C   SECTION 1 OF THE BUFR MESSAGE IN ARRAY MBAY.
C
C PROGRAM HISTORY LOG:
C 2013-10-07  J. ATOR    -- ADAPTED FROM RTRCPT
C
C USAGE:    CALL RTRCPT (MBAY,IYR,IMO,IDY,IHR,IMI,IRET) 
C   INPUT ARGUMENT LIST:
C     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING
C                BUFR MESSAGE
C
C   OUTPUT ARGUMENT LIST:
C     IYR      - INTEGER: TANK RECEIPT YEAR
C     IMO      - INTEGER: TANK RECEIPT MONTH
C     IDY      - INTEGER: TANK RECEIPT DAY
C     IHR      - INTEGER: TANK RECEIPT HOUR
C     IMI      - INTEGER: TANK RECEIPT MINUTE
C     IRET     - INTEGER: RETURN CODE:
C                       0 = normal return
C                      -1 = no tank receipt time was present within MBAY
C
C REMARKS:
C    THIS ROUTINE CALLS:        IUPB     IUPBS01   
C    THIS ROUTINE IS CALLED BY: RTRCPT
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      DIMENSION	MBAY (*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = -1

C     Check whether the message contains a tank receipt time.

      IF(IUPBS01(MBAY,'BEN').EQ.4) THEN
	IS1BYT = 23
      ELSE
	IS1BYT = 19
      ENDIF
      IF( (IS1BYT+5) .GT. IUPBS01(MBAY,'LEN1') ) RETURN

C     Unpack the tank receipt time.

C     Note that IS1BYT is a starting byte number relative to the
C     beginning of Section 1, so we still need to account for
C     Section 0 when specifying the actual byte numbers to unpack
C     within the overall message.

      IMGBYT = IS1BYT + IUPBS01(MBAY,'LEN0')

      IYR = IUPB(MBAY,IMGBYT,16)
      IMO = IUPB(MBAY,IMGBYT+2,8)
      IDY = IUPB(MBAY,IMGBYT+3,8)
      IHR = IUPB(MBAY,IMGBYT+4,8)
      IMI = IUPB(MBAY,IMGBYT+5,8)

      IRET = 0

      RETURN
      END
