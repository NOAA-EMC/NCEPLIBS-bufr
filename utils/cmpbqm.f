C-----------------------------------------------------------------------
C  MAIN PROGRAM CMPBQM
C-----------------------------------------------------------------------
      PROGRAM CMPBQM
 
      CHARACTER*200 FILE
      CHARACTER*50 HEADR,OBSTR,QMSTR,FCSTR,ERSTR,QMSFC
      CHARACTER*20 VARS(7)
      CHARACTER*8  SUBSET,DATE
      DIMENSION    KNT(300,7,0:17),HDR(5),OBS(8,255),QMS(8,255)
      LOGICAL      SKIP
      REAL*8       HDR,OBS,QMS
 
      DATA HEADR /'SID XOB YOB DHR TYP              '/
      DATA OBSTR /'POB QOB TOB ZOB UOB PWO RHO VOB  '/
      DATA QMSTR /'PQM QQM TQM ZQM WQM PWQ RHQ      '/
 
      DATA VARS   /'PRESSURE        ',
     .             'SPECIFIC HUMIDTY',
     .             'TEMPERATURE     ',
     .             'HEIGHT          ',
     .             'WIND COMPONENTS ',
     .             'PRECIPITABLE H2O',
     .             'RELATIVE HUMIDTY'/
 
      DATA LUBFR /8    /
      DATA VMAX  /10E10/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      IREC = 0
      KNT = 0
 
C  OPEN A FILE - GET A DATE
C  ------------------------
 
      read(5,'(a)',end=100) file
      open(lubfr,file=file,form='unformatted')
      CALL OPENBF(LUBFR,'IN',LUBFR)
      CALL READMG(LUBFR,SUBSET,IDATE,IRET)
      IF(IRET.NE.0) GOTO 900
      WRITE(DATE,'(I8)') IDATE
      DO I=1,8
      IF(DATE(I:I).EQ.' ') DATE(I:I) = '0'
      ENDDO
      PRINT'(''DATA  VALID AT  '',A8)',DATE
 
C  READ THRU THE PREPDA RECORDS
C  ----------------------------
 
10    CALL READSB(LUBFR,IRET)
      IF(IRET.NE.0) THEN
         CALL READMG(LUBFR,SUBSET,IDATE,IRET)
         IF(IRET.NE.0) GOTO 100
         CALL UFBCNT(LUBFR,IREC,ISUB)
         GOTO 10
      ENDIF
      QMS = 10E10
      CALL UFBINT(LUBFR,HDR,5,1,IRET,HEADR)
      CALL UFBINT(LUBFR,OBS,8,255,NLEV,OBSTR)
      CALL UFBINT(LUBFR,QMS,8,255,NLEV,QMSTR)
 
      KX = HDR(5)
 
      DO L=1,NLEV
      DO K=1,7
      IQ = -1
      IF(K.EQ.5) OBS(5,L) = MAX(OBS(5,L),OBS(8,L))
      IF(OBS(K,L).LT.VMAX .AND. QMS(K,L).LT.VMAX) THEN
         IQ = QMS(K,L)
      ELSEIF(OBS(K,L).LT.VMAX .AND. QMS(K,L).GE.VMAX) THEN
         IQ = 16
      ELSEIF(OBS(K,L).GE.VMAX .AND. QMS(K,L).LT.VMAX) THEN
         IQ = 17
      ENDIF
      IF(IQ.GE.0) KNT(KX,K,IQ) = KNT(KX,K,IQ)+1
      ENDDO
      ENDDO
 
      GOTO 10
 
C  FINISH UP
C  ---------
 
100   DO K=1,7
      PRINT*,VARS(K)
      PRINT*
      DO KX=1,300
      ITOT = 0; igood=0; ifail=0
      DO IQ=0,17
      ITOT = ITOT+KNT(KX,K,IQ)
      if(iq.le.3) then 
         igood=igood+KNT(KX,K,IQ)
      elseif(iq.le.7) then
         ifail=ifail+KNT(KX,K,IQ)   
      endif
      ENDDO
      IF(ITOT.GT.0) PRINT101,KX,ITOT,igood,ifail,(KNT(KX,K,IQ),IQ=8,17)
101   FORMAT(I3,I6,2('|', I6),
     .             2('|', I6),
     .             1('|',6I6),
     .             2('|', I6))
      ENDDO
      PRINT*
      ENDDO
 
      PRINT*,'******CMPBQM PROCESSED ',IREC,' BUFR RECORDS******'
      STOP
900   CALL BORT('CMPBQM - ERROR READING BUFR FILE ')
      END
