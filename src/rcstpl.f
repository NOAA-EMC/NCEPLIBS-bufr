C> @file
C> @brief Store the subset template into internal arrays.
C>
C> @author Woollen @date 1994-01-06

C> Initialize space in internal subset arrays
C> in modules @ref moda_usrint and @ref moda_usrbit, according
C> to the subset definition from subroutine makestab(). This is in
C> preparation for the actual unpacking of the subset in rdtree().
C>
C> @param[in] LUN - integer: file ID
C> @param[out] IRET - integer: return code:
C> - 0 Normal return.
C> - -1 An error occurred, possibly due to a corrupt subset in the input message.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE RCSTPL(LUN,IRET)

      use modv_vars, only: bmiss, maxjl, maxss

      use moda_usrint
      use moda_usrbit
      use moda_msgcwd
      use moda_bitbuf
      use moda_tables
      use moda_usrtmp

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR
      DIMENSION     NBMP(2,MAXRCR),NEWN(2,MAXRCR)
      DIMENSION     KNX(MAXRCR)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  SET THE INITIAL VALUES FOR THE TEMPLATE
C  ---------------------------------------

c  .... Positional index of Table A mnem.
      INV(1,LUN) = INODE(LUN)
      VAL(1,LUN) = 0
      NBMP(1,1) = 1
      NBMP(2,1) = 1
      NODI = INODE(LUN)
      NODE = INODE(LUN)
      MBMP = 1
      KNVN = 1
      NR   = 0

      DO I=1,MAXRCR
      KNX(I) = 0
      ENDDO

C  SET UP THE PARAMETERS FOR A LEVEL OF RECURSION
C  ----------------------------------------------

10    CONTINUE

      NR = NR+1
      IF(NR.GT.MAXRCR) GOTO 900
      NBMP(1,NR) = 1
      NBMP(2,NR) = MBMP

      N1 = ISEQ(NODE,1)
      N2 = ISEQ(NODE,2)
      IF(N1.EQ.0         ) GOTO 901
      IF(N2-N1+1.GT.MAXJL) THEN
        IF(IPRT.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: RCSTPL - MAXJL OVERFLOW; SUBSET SKIPPED')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
        ENDIF
        IRET = -1
        RETURN
      ENDIF
      NEWN(1,NR) = 1
      NEWN(2,NR) = N2-N1+1

      DO N=1,NEWN(2,NR)
      NN = JSEQ(N+N1-1)
      IUTMP(N,NR) = NN
      VUTMP(N,NR) = VALI(NN)
      ENDDO

C  STORE NODES AT SOME RECURSION LEVEL
C  -----------------------------------

20    DO I=NBMP(1,NR),NBMP(2,NR)
      IF(KNX(NR).EQ.0000) KNX(NR) = KNVN
      IF(I.GT.NBMP(1,NR)) NEWN(1,NR) = 1
      DO J=NEWN(1,NR),NEWN(2,NR)
      IF(KNVN+1.GT.MAXSS) THEN
        IF(IPRT.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: RCSTPL - MAXSS OVERFLOW; SUBSET SKIPPED')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
        ENDIF
        IRET = -1
        RETURN
      ENDIF
      KNVN = KNVN+1
      NODE = IUTMP(J,NR)
c  .... INV is positional index in internal jump/link table for packed
c       subset element KNVN in MBAY
      INV(KNVN,LUN) = NODE
c  .... MBIT is the bit in MBAY pointing to where the packed subset
c       element KNVN begins
      MBIT(KNVN) = MBIT(KNVN-1)+NBIT(KNVN-1)
c  .... NBIT is the number of bits in MBAY occupied by packed subset
c       element KNVN
      NRFELM(KNVN,LUN) = IGETRFEL(KNVN,LUN)
      NBIT(KNVN) = IBT(NODE)
      IF(TAG(NODE)(1:5).EQ.'DPRI ') THEN
c  .... This is a bitmap entry, so get and store the corresponding value
        CALL UPBB(IDPRI,NBIT(KNVN),MBIT(KNVN),MBAY(1,LUN))
        IF(IDPRI.EQ.0) THEN
          VAL(KNVN,LUN) = 0.0
        ELSE
          VAL(KNVN,LUN) = BMISS
        ENDIF
        CALL STRBTM(KNVN,LUN)
      ENDIF
c  .... Actual unpacked subset values (VAL) are initialized here
c       (numbers as BMISS)
      VAL(KNVN,LUN) = VUTMP(J,NR)
      IF(ITP(NODE).EQ.1) THEN
         CALL UPBB(MBMP,NBIT(KNVN),MBIT(KNVN),MBAY(1,LUN))
         NEWN(1,NR) = J+1
         NBMP(1,NR) = I
         GOTO 10
      ENDIF
      ENDDO
      NEW = KNVN-KNX(NR)
      VAL(KNX(NR)+1,LUN) = VAL(KNX(NR)+1,LUN) + NEW
      KNX(NR) = 0
      ENDDO

C  CONTINUE AT ONE RECURSION LEVEL BACK
C  ------------------------------------

      IF(NR-1.NE.0) THEN
         NR = NR-1
         GOTO 20
      ENDIF

C  FINALLY STORE THE LENGTH OF (NUMBER OF ELEMENTS IN) SUBSET TEMPLATE
C  -------------------------------------------------------------------

      NVAL(LUN) = KNVN

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: RCSTPL - THE NUMBER OF RECURSION '//
     . 'LEVELS EXCEEDS THE LIMIT (",I3,")")') MAXRCR
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: RCSTPL - UNSET EXPANSION SEGMENT ",A)')
     . TAG(NODI)
      CALL BORT(BORT_STR)
      END
