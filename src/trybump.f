C> @file
C> @brief Try to expand a delayed replication sequence.
C>
C> @author Woollen @date 1994-01-06

C> This subroutine checks the first node associated with a
C> character string (parsed into arrays in common block /usrstr/) in
C> order to determine if it represents a delayed replication sequence.
C> If so, then the delayed replication sequence is initialized and
C> expanded (i.e. "bumped") to the value of input argument I2.
C> A call is then made to subroutine ufbrw() in order to write user data
C> into the newly expanded replication sequence.
C>
C> trybump() is usually called from ufbint() after ufbint() receives a
C> non-zero return code from ufbrw(). The cause of a bad return from
C> ufbrw() is usually a delayed replication sequence which isn't
C> expanded enough to hold the array of data the user is trying to
C> write. So trybump() is one last chance to resolve that situation.
C>
C> @note Argument IO is always passed in with a value of 1 at the present
C> time. In the future the subroutine may be expanded to allow it
C> to operate on input files.
C>
C> @param[in] LUN - integer: file ID of open BUFR file
C> @param[in] USR - real*8: (i1,i2) starting address of data values to be
C> written to data subset.
C> @param[in] I1 - integer: length of first dimension of USR.
C> @param[in] I2 - integer: number of "levels" of data values to be
C> written to data subset.
C> @param[in] IO - integer: status indicator for BUFR file
C> - 0 Input file (possible future use)
C> - 1 Output file
C> @param[out] IRET - integer: number of "levels" of data values
C> written to data subset.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE TRYBUMP(LUN,USR,I1,I2,IO,IRET)

      USE MODA_USRINT

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)

      REAL*8 USR(I1,I2)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  SEE IF THERE IS A DELAYED REPLICATION GROUP INVOLVED
C  ----------------------------------------------------

      NDRP = LSTJPB(NODS(1),LUN,'DRP')
      IF(NDRP.LE.0) GOTO 100

C  IF SO, CLEAN IT OUT AND BUMP IT TO I2
C  -------------------------------------

      INVN = INVWIN(NDRP,LUN,1,NVAL(LUN))
      VAL(INVN,LUN) = 0
      JNVN = INVN+1
      DO WHILE(NINT(VAL(JNVN,LUN)).GT.0)
         JNVN = JNVN+NINT(VAL(JNVN,LUN))
      ENDDO
      DO KNVN=1,NVAL(LUN)-JNVN+1
      INV(INVN+KNVN,LUN) = INV(JNVN+KNVN-1,LUN)
      VAL(INVN+KNVN,LUN) = VAL(JNVN+KNVN-1,LUN)
      ENDDO
      NVAL(LUN) = NVAL(LUN)-(JNVN-INVN-1)
      CALL USRTPL(LUN,INVN,I2)

C  FINALLY, CALL THE MNEMONIC WRITER
C  ----------------------------------------

      CALL UFBRW(LUN,USR,I1,I2,IO,IRET)

C  EXIT
C  ----

100   RETURN
      END
