C> @file
C> @brief Copy DX BUFR table information within internal memory.
C>
C> @author Woollen @date 1994-01-06

C> Copy all of the DX BUFR table information from
C> one unit to another within internal memory.
C>
C> @param[in] LUD - integer: file ID for input unit
C> @param[in] LUN - integer: file ID for output unit
C>
C> @author Woollen @date 1994-01-06

      SUBROUTINE CPBFDX(LUD,LUN)

      USE MODA_MSGCWD
      USE MODA_TABABD

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  INITIALIZE THE DICTIONARY TABLE PARTITION
C  -----------------------------------------

      CALL DXINIT(LUN,0)

C  COPY ONE TABLE PARTITION TO ANOTHER
C  -----------------------------------

c  .... Positional index for Table A mnem.
      INODE(LUN) = INODE(LUD)

c  .... Set the number of Table A entries
      NTBA(LUN) = NTBA(LUD)
c  .... Set the number of Table B entries
      NTBB(LUN) = NTBB(LUD)
c  .... Set the number of Table D entries
      NTBD(LUN) = NTBD(LUD)

c  .... Copy Table A entries
      DO I=1,NTBA(LUD)
c  .... Message type
      IDNA(I,LUN,1) = IDNA(I,LUD,1)
c  .... Message subtype
      IDNA(I,LUN,2) = IDNA(I,LUD,2)
c  .... Table A entries
      TABA(I,LUN) = TABA(I,LUD)
c  .... Pointer indices into internal tbl
      MTAB(I,LUN) = MTAB(I,LUD)
      ENDDO

c  .... Copy Table B entries
      DO I=1,NTBB(LUD)
c  .... Integer repr. of FXY descr.
      IDNB(I,LUN) = IDNB(I,LUD)
c  .... Table B entries
      TABB(I,LUN) = TABB(I,LUD)
      ENDDO

c  .... Copy Table D entries
      DO I=1,NTBD(LUD)
c  .... Integer repr. of FXY descr.
      IDND(I,LUN) = IDND(I,LUD)
c  .... Table B entries
      TABD(I,LUN) = TABD(I,LUD)
      ENDDO

      RETURN
      END
