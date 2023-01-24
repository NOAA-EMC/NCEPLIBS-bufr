C> @file
C> @brief Write or read specified values to or
c> from the current bufr data subset within internal arrays
C>
C> ### Program History Log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 1994-01-06 | J. Woollen | original author.
C> 1998-07-08 | J. Woollen | improved machine portability.
C> 1999-11-18 | J. Woollen | the number of bufr files which can be opened at one time increased from 10 to 32.
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation
C> 2009-03-31 | J. Woollen | add documentation
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine writes or reads specified values to or
c> from the current bufr data subset within internal arrays, with the
c> direction of the data transfer determined by the context of io
c> (i.e., if io indicates lun points to a bufr file that is open for
c> input, then data values are read from the internal data subset;
c> otherwise, data values are written to the internal data subset).
C> The data values correspond to internal arrays representing parsed
c> strings of mnemonics which are either:
C> 1. part of a regular (i.e., non-delayed) replication sequence
c> 2. replicated by being directly listed more than once within an
c> overall subset definition
c>
C> This subroutine should never be called by any application program;
c> instead, application programs should always call ufbrep().
C>
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C> @param[inout] USR - only if bufr file open for output:.
C> real*8: (i1,i2) starting address of data values
c> written to data subset.
C> Out: - only if bufr file open for input:
c> real*8: (i1,i2) starting address of data values
c> read from data subset
C> @param[in] I1 - integer: length of first dimension of usr.
C> @param[in] I2 - integer: length of second dimension of usr.
C> @param[in] IO - integer: status indicator for bufr file associated
C> with lun:
C> - 0 input file
C> - 1 output file
C> @param[out] IRET - integer:.
C> - if bufr file open for input: number of "levels" of
c> data values read from data subset (must be no
c> larger than i2)
c> - if bufr file open for output: number of "levels"
c> of data values written to data subset (should be
c> same as i2)
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE UFBRP(LUN,USR,I1,I2,IO,IRET)

      USE MODA_USRINT

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)

      REAL*8 USR(I1,I2)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IRET = 0
      INS1 = 0
      INS2 = 0

C  FIND FIRST NON-ZERO NODE IN STRING
C  ----------------------------------

      DO NZ=1,NNOD
      IF(NODS(NZ).GT.0) GOTO 1
      ENDDO
      GOTO 100

C  FRAME A SECTION OF THE BUFFER - RETURN WHEN NO FRAME
C  ----------------------------------------------------

1     IF(INS1+1.GT.NVAL(LUN)) GOTO 100
      IF(IO.EQ.1 .AND. IRET.EQ.I2) GOTO 100
      INS1 = INVTAG(NODS(NZ),LUN,INS1+1,NVAL(LUN))
      IF(INS1.EQ.0) GOTO 100

      INS2 = INVTAG(NODS(NZ),LUN,INS1+1,NVAL(LUN))
      IF(INS2.EQ.0) INS2 = NVAL(LUN)
      IRET = IRET+1

C  READ USER VALUES
C  ----------------

      IF(IO.EQ.0 .AND. IRET.LE.I2) THEN
         DO I=1,NNOD
         IF(NODS(I).GT.0) THEN
            INVN = INVTAG(NODS(I),LUN,INS1,INS2)
            IF(INVN.GT.0) USR(I,IRET) = VAL(INVN,LUN)
         ENDIF
         ENDDO
      ENDIF

C  WRITE USER VALUES
C  -----------------

      IF(IO.EQ.1 .AND. IRET.LE.I2) THEN
         DO I=1,NNOD
         IF(NODS(I).GT.0) THEN
            INVN = INVTAG(NODS(I),LUN,INS1,INS2)
            IF(INVN.GT.0) VAL(INVN,LUN) = USR(I,IRET)
         ENDIF
         ENDDO
      ENDIF

C  GO FOR NEXT FRAME
C  -----------------

      GOTO 1

C  EXIT
C  ----

100   RETURN
      END
