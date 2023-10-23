C> @file
C> @brief Read/write one or more data values from/to a data subset.
C>
C> @author J. Woollen @date 1999-11-18

C> Write or read specified values to or
C> from the current BUFR data subset within internal arrays, with the
C> direction of the data transfer determined by the context of IO.
C> The data values correspond to internal arrays representing parsed
C> strings of mnemonics which are either part of a fixed (i.e. non-delayed)
C> replication sequence, or for mnememonics which are replicated by being
C> directly listed more than once within an overall subset definition.
C>
C> This subroutine should never be directly called by an application
C> program; instead, an application program should directly call ufbstp()
C> which will internally call this subroutine.
C>
C> This subroutine is similar to subroutine ufbrp(), but it is designed
C> for different use cases.  For a more detailed explanation of how
C> subroutine ufbstp() differs from subroutine ufbrep(), and therefore
C> how this subroutine differs from subroutine ufbrp(), see the
C> discussion in [DX BUFR Tables](@ref ufbsubs).
C>
C> @param[in] LUN - integer: I/O stream index into internal memory arrays.
C> @param[inout] USR - real*8(*,*): Data values
C> @param[in] I1 - integer: length of first dimension of USR.
C> @param[in] I2 - integer: length of second dimension of USR.
C> @param[in] IO - integer: status indicator for BUFR file associated
C> with LUN:
C> - 0 input file
C> - 1 output file
C> @param[out] IRET - integer: number of "levels" of data values read
C> from or written to data subset
C>
C> @author J. Woollen @date 1999-11-18
      SUBROUTINE UFBSP(LUN,USR,I1,I2,IO,IRET)

      USE MODA_USRINT

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)

      REAL*8  USR(I1,I2)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IRET = 0
      INS1 = 0
      INS2 = 0

C  FRAME A SECTION OF THE BUFFER - RETURN WHEN NO FRAME
C  ----------------------------------------------------

1     IF(INS1+1.GT.NVAL(LUN)) GOTO 100
      INS1 = INVTAG(NODS(1),LUN,INS1+1,NVAL(LUN))
      IF(INS1.EQ.0) GOTO 100

      INS2 = INVTAG(NODS(1),LUN,INS1+1,NVAL(LUN))
      IF(INS2.EQ.0) INS2 = NVAL(LUN)
      IRET = IRET+1

C  READ USER VALUES
C  ----------------

      IF(IO.EQ.0 .AND. IRET.LE.I2) THEN
         INVM = INS1
         DO I=1,NNOD
         IF(NODS(I).GT.0) THEN
            INVN = INVTAG(NODS(I),LUN,INVM,INS2)
            IF(INVN.GT.0) USR(I,IRET) = VAL(INVN,LUN)
            INVM = MAX(INVN,INVM)
         ENDIF
         ENDDO
      ENDIF

C  WRITE USER VALUES
C  -----------------

      IF(IO.EQ.1 .AND. IRET.LE.I2) THEN
         INVM = INS1
         DO I=1,NNOD
         IF(NODS(I).GT.0) THEN
            INVN = INVTAG(NODS(I),LUN,INVM,INS2)
            IF(INVN.GT.0) VAL(INVN,LUN) = USR(I,IRET)
            INVM = MAX(INVN,INVM)
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
