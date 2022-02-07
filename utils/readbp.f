C> @file
C> @brief Read prepbufr file and print each report one at a time

C-----------------------------------------------------------------------
C READ AND DISPLAY AN ON29BUFR FILE ONE REPORT AT A TIME
C-----------------------------------------------------------------------
      PROGRAM READBP

      use bufr_procedures

      CHARACTER*120 FILE
      CHARACTER*40 HSTR,OSTR,QSTR
      CHARACTER*8  YOU,SID,STA,SUBSET,MSG,cmc(17)
      CHARACTER*3  VARS(8)
      DIMENSION    HDR(10,1),OBS(10,255),QMS(10,255),QMC(17)
      EQUIVALENCE  (HDR(1,1),SID)
      EQUIVALENCE  (qmc,cmc)        
      LOGICAL      WINDOW,STEAM,LEVEL
      real*8       hdr,obs,qms,qmc
 
      DATA HSTR/'SID XOB YOB DHR ELV T29 ITP TYP SRC PRG '/
      DATA OSTR/'CAT POB QOB TOB ZOB UOB VOB PSL         '/
      DATA QSTR/'PQM QQM TQM ZQM WQM PSQ                 '/
 
      DATA VARS/'LVL','CAT','POB','SPH','TOB','ZOB','UOB','VOB'/
      DATA CMC /'0','1','2','3','4','5','6','7','8',
     .          '9','A','B','C','D','E','F','*'/
 
      DATA BMISS /10E10/
      DATA LUBFR /8    /
      DATA STA   /'   '/
      data msg   /' '/
      DATA POB   /0/
      data irt   /0/
      data itp   /0/
      data ikx   /0/
      DATA WINDOW /.FALSE./
      DATA STEAM  /.FALSE./
      DATA LEVEL  /.FALSE./
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      iprt = 0
      nsta = 8

      READ(4,'(A)',END=100) FILE; print*,file
      read(4,'(a8)',end=1) sta
      read(4,*     ,end=1) pob
      read(4,*     ,end=1) ikx
1     steam = sta.ne.' '
      level = pob.ne.0  
 
C  OPEN THE BUFR INPUT FILE
C  ------------------------
 
      OPEN(LUBFR,FILE=FILE,FORM='UNFORMATTED')

      CALL OPENBF(LUBFR,'IN',LUBFR)
      CALL READMG(LUBFR,SUBSET,IDATE,IRET)
      IF(IRET.NE.0) GOTO 100
      IF(.NOT.STEAM) PRINT*,'READING DATA FOR ',IDATE
 
C  READ A SUBSET - READ ANOTHER MESSAGE WHEN NO MORE SUBSETS
C  ---------------------------------------------------------
 
10    CALL READSB(LUBFR,IRET)
      IF(IRET.NE.0) THEN
         CALL READMG(LUBFR,SUBSET,IDATE,IRET)
         IF(IRET.NE.0) GOTO 100
         GOTO 10
      ENDIF
      CALL UFBCNT(LUBFR,IREC,ISUB)
 
C  MOVE SUBSET CONTENTS INTO THIS PROGRAM
C  --------------------------------------
 
      CALL UFBINT(LUBFR,HDR,10,  1,IRET,HSTR)
      XOB = HDR(2,1)
      YOB = HDR(3,1)
      jrt = hdr(6,1)
      jtp = hdr(7,1)
      jkx = hdr(8,1)
      IF(STA.NE.' ' .AND. STA.NE.SID(1:nsta)) GOTO 10
      IF(irt.ne.0   .and. irt.ne.jrt) GOTO 10
      IF(itp.ne.0   .and. itp.ne.jtp) GOTO 10
      IF(ikx.ne.0   .and. ikx.ne.jkx) GOTO 10
      IF(msg.ne.' ' .and. msg.ne.subset) goto 10
      IF(WINDOW) THEN
         IF(.NOT.(XOB.GE.X1 .AND. XOB.LE.X2))GOTO 10
         IF(.NOT.(YOB.GE.Y1 .AND. YOB.LE.Y2))GOTO 10
      ENDIF
 
      CALL UFBINT(LUBFR,OBS,10,255,NLEV,OSTR)
      CALL UFBINT(LUBFR,QMS,10,255,NLEQ,QSTR)
      IF(NLEV.NE.NLEQ) STOP 'NLEV<>NLEQ'
 
C  MOVE CAT 8 DATA TO PRINT RANGE
C  ------------------------------
      DO L=1,NLEV
      IF(OBS(1,L).EQ.8) THEN
         OBS(2,L) = OBS(9,L)
         OBS(3,L) = OBS(10,L)
      ENDIF
      ENDDO
 
C  PRINT A REPORT 20 LINES AT A TIME
C  ---------------------------------

      PRINT'(80(''-''))'

      if(level) then

      print'(1x,a8,7(f8.2,1x))',(hdr(i,1),i=1,8)
 
      else

      PRINT'(''MESSAGE: '',A8,2(2X,I4),i12 )' , SUBSET,IREC,ISUB,idate
      PRINT'(''STATION: '',A8,1X,2(F8.2,1X))' , (HDR(I,1),I= 1,3)
      PRINT'(''TIME:    '',I10,2x,F8.2     )' , IDATE,HDR(4,1) 
      PRINT'(''ELV:     '',F8.2            )' , (HDR(5,1)       )
      PRINT'(''PSL:     '',F8.2,1X,A1      )' ,  OBS(8,1),QMS(6,1)
      PRINT'(''TYPE:    '',3(F8.0,1X)      )' , (HDR(I,1),I= 6,8)
      PRINT'(''SOURCE:  '',3a8             )' , (HDR(I,1),I= 9,9)
      PRINT'(''SEQUENCE '',F10.0           )' , (HDR(10,1)      )
      PRINT'(''DATA:    ''                 )'

      endif
 
      do l=1,nlev
      do i=1,7
      iqm = qms(i,l)
      if(iqm<0)iqm=10e8
      iqm = min(iqm,16)
      qms(i,l) = qmc(iqm+1)
      enddo
      enddo
 
      NLNE = 7
      PRINT'(2(1X,A3),6(8X,A3))',VARS
      DO 12 L=1,NLEV
      dif = abs(obs(2,l)-pob)
      if(level .and. dif.gt..01) goto 12
      NLNE = NLNE+1
      PRINT11, L,NINT(OBS(1,L)),(OBS(I,L),QMS(MIN(I-1,5),L),I=2,7)
11    FORMAT(2I4,6(1X,F7.1,'(',A1,')'))
c     IF(NLNE.EQ.20 .and. .not.steam) THEN
c        READ(5,'(A1)') YOU
c        CALL CAPIT(YOU)
c        IF(YOU.EQ.'Q') STOP
c        PRINT'(2(1X,A3),6(8X,A3))',VARS
c        NLNE = 0
c     ENDIF
12    ENDDO
      PRINT'(80(''-''))'
      if(steam) goto 10
 
C  GO TO READ THE NEXT SUBSET IF NO 'Q' YOU
C  ----------------------------------------
 
99    READ(5,'(A8)') YOU
      CALL CAPIT(YOU)
      IF(YOU.EQ.'Q') STOP
      IF(YOU.EQ.'S') THEN
         READ(5,'(A8)') STA
         DO I=1,8
         IF(STA(I:I).NE.' ') NSTA = I
         ENDDO
      ENDIF
      IF(YOU.EQ.'R') READ(5,'(i3,1x,i2)') irt,itp
      IF(YOU.EQ.'K') READ(5,'(i3)') ikx
      IF(YOU.EQ.'M') READ(5,'(a8)') msg
      IF(YOU.EQ.'W') READ(5,*) X1,X2,Y1,Y2
      IF(YOU.EQ.'W') WINDOW = .TRUE.
      IF(YOU.EQ.'D') THEN               
         CALL ufdump(LUBFR,6)
         GOTO 99
      ENDIF
      IF(YOU.EQ.'E') THEN               
         CALL ufbdmp(LUBFR,6)
         GOTO 99
      ENDIF
      GOTO 10
 
C  HERE WHEN ALL MESSAGES HAVE BEEN READ
C  -------------------------------------
 
100   STOP
      END
