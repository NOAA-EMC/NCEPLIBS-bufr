C> @file
C> @brief Read/write one or more data values from/to a data subset.
C>
C> @author J. Woollen @date 1994-01-06

C> Write or read specified values to or from
C> the current BUFR data subset within internal arrays, with the
C> direction of the data transfer determined by the context of IO.
C> The data values correspond to internal arrays representing parsed
C> strings of mnemonics which are part of a delayed-replication
C> sequence, or for which there is no replication at all.
C>
C> This subroutine should never be directly called by an application
C> program; instead, an application program should directly call ufbint()
C> which will internally call this subroutine.
C>
C> @param[in] LUN - integer: file ID.
C> @param[inout] USR - real*8(*,*): Data values
C> @param[in] I1 - integer: length of first dimension of USR.
C> @param[in] I2 - integer: length of second dimension of USR.
C> @param[in] IO - integer: status indicator for BUFR file associated
C> with LUN:
C> - 0 input file
C> - 1 output file
C> @param[out] IRET - integer: number of "levels" of data values read
C> from or written to data subset
C> - -1 none of the mnemonics in the string passed to ufbint() were found
C> in the data subset template
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE UFBRW(LUN,USR,I1,I2,IO,IRET)

      USE MODV_BMISS
      USE MODA_USRINT
      USE MODA_TABLES

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
      COMMON /QUIET / IPRT

      CHARACTER*128 ERRSTR
      REAL*8       USR(I1,I2)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IRET = 0

C  LOOP OVER COND WINDOWS
C  ----------------------

      INC1 = 1
      INC2 = 1

1     CALL CONWIN(LUN,INC1,INC2)
      IF(NNOD.EQ.0) THEN
         IRET = I2
         GOTO 100
      ELSEIF(INC1.EQ.0) THEN
         GOTO 100
      ELSE
         DO I=1,NNOD
         IF(NODS(I).GT.0) THEN
            INS2 = INC1
            CALL GETWIN(NODS(I),LUN,INS1,INS2)
            IF(INS1.EQ.0) GOTO 100
            GOTO 2
         ENDIF
         ENDDO
         IRET = -1
         GOTO 100
      ENDIF

C  LOOP OVER STORE NODES
C  ---------------------

2     IRET = IRET+1

      IF(IPRT.GE.2)  THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
         WRITE ( UNIT=ERRSTR, FMT='(5(A,I7))' )
     .      'BUFRLIB: UFBRW - IRET:INS1:INS2:INC1:INC2 = ',
     .      IRET, ':', INS1, ':', INS2, ':', INC1, ':', INC2
         CALL ERRWRT(ERRSTR)
         KK = INS1
         DO WHILE ( ( INS2 - KK ) .GE. 5 )
            WRITE ( UNIT=ERRSTR, FMT='(5A10)' )
     .         (TAG(INV(I,LUN)),I=KK,KK+4)
            CALL ERRWRT(ERRSTR)
            KK = KK+5
         ENDDO
         WRITE ( UNIT=ERRSTR, FMT='(5A10)' )
     .      (TAG(INV(I,LUN)),I=KK,INS2)
         CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  WRITE USER VALUES
C  -----------------

      IF(IO.EQ.1 .AND. IRET.LE.I2) THEN
         DO I=1,NNOD
         IF(NODS(I).GT.0) THEN
            IF(IBFMS(USR(I,IRET)).EQ.0) THEN
               INVN = INVWIN(NODS(I),LUN,INS1,INS2)
               IF(INVN.EQ.0) THEN
                  CALL DRSTPL(NODS(I),LUN,INS1,INS2,INVN)
                  IF(INVN.EQ.0) THEN
                     IRET = 0
                     GOTO 100
                  ENDIF
                  CALL NEWWIN(LUN,INC1,INC2)
                  VAL(INVN,LUN) = USR(I,IRET)
               ELSEIF(LSTJPB(NODS(I),LUN,'RPS').EQ.0) THEN
                  VAL(INVN,LUN) = USR(I,IRET)
               ELSEIF(IBFMS(VAL(INVN,LUN)).NE.0) THEN
                  VAL(INVN,LUN) = USR(I,IRET)
               ELSE
                  CALL DRSTPL(NODS(I),LUN,INS1,INS2,INVN)
                  IF(INVN.EQ.0) THEN
                     IRET = 0
                     GOTO 100
                  ENDIF
                  CALL NEWWIN(LUN,INC1,INC2)
                  VAL(INVN,LUN) = USR(I,IRET)
               ENDIF
            ENDIF
         ENDIF
         ENDDO
      ENDIF

C  READ USER VALUES
C  ----------------

      IF(IO.EQ.0 .AND. IRET.LE.I2) THEN
         DO I=1,NNOD
         USR(I,IRET) = BMISS
         IF(NODS(I).GT.0) THEN
            INVN = INVWIN(NODS(I),LUN,INS1,INS2)
            IF(INVN.GT.0) USR(I,IRET) = VAL(INVN,LUN)
         ENDIF
         ENDDO
      ENDIF

C  DECIDE WHAT TO DO NEXT
C  ----------------------

      IF(IO.EQ.1.AND.IRET.EQ.I2) GOTO 100
      CALL NXTWIN(LUN,INS1,INS2)
      IF(INS1.GT.0 .AND. INS1.LT.INC2) GOTO 2
      IF(NCON.GT.0) GOTO 1

C  EXIT
C  ----

100   RETURN
      END
