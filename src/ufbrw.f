C> @file
C> @brief Write or read specified values to or from
c> the current bufr data subset within internal arrays.
C>
C> ### Program History Log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 1994-01-06 | J. Woollen | original author
C> 1996-12-11 | J. Woollen | removed a hard abort for users who try to write non-existing mnemonics
C> 1998-07-08 | J. Woollen | improved machine portability
C> 1998-10-27 | J. Woollen | modified to correct problems caused by in- lining code with fpp directives
C> 1999-11-18 | J. Woollen | the number of bufr files which can be opened at one time increased from 10 to 32
C> 2002-05-14 | J. Woollen | removed old cray compiler directives
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation
C> 2007-01-19 | J. Ator    | use function ibfms
C> 2009-03-31 | J. Woollen | add documentation.
C> 2009-04-21 | J. Ator    | use errwrt; use lstjpb instead of lstrps
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine writes or reads specified values to or from
c> the current bufr data subset within internal arrays, with the
c> direction of the data transfer determined by the context of io
c> (i.e., if io indicates lun points to a bufr file that is open for
c> input, then data values are read from the internal data subset;
C> otherwise, data values are written to the internal data subset).
C> The data values correspond to internal arrays representing parsed
c> strings of mnemonics which are part of a delayed-replication
c> sequence, or for which there is no replication at all.
C>
C> This subroutine should never be called by any application program;
c> instead, application programs should always call ufbint().
C>
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C> @param[inout] USR - only if bufr file open for output:.
C> real*8: (i1,i2) starting address of data values
c> written to data subset
C> Out: only if bufr file open for input: real*8: (i1,i2) starting
C> address of data values read from data subset.
C> @param[in] I1 - integer: length of first dimension of usr.
C> @param[in] I2 - integer: length of second dimension of usr.
C> @param[in] IO - integer: status indicator for bufr file associated.
C> with lun:
c> - 0 input file
C> - 1 output file
C> @param[out] IRET - integer:.
C> - if bufr file open for input: number of "levels" of
c> data values read from data subset (must be no
c> larger than i2)
c> - -1 = none of the mnemonics in the string passed
c> to ufbint were found in the subset template
c> - if bufr file open for output: number of "levels"
c> of data values written to data subset (should be
c> same as i2)
c> - -1 = none of the mnemonics in the string passed
c> to ufbint were found in the subset template.
c>
C> @author Woollen @date 1994-01-06
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
