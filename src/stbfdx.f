C> @file
C> @brief Store a DX BUFR tables message into internal arrays.
C>
C> @author J Ator @date 2009-03-23

C> Copy a DX BUFR tables message into the internal memory arrays in
C> module @ref moda_tababd.
C>
C> @param[in] LUN - integer: file ID
C> @param[in] MESG - integer(*): DX BUFR tables message
C>
C> @author J. Ator @date 2009-03-23
      SUBROUTINE STBFDX(LUN,MESG)

      USE MODV_MAXCD
      USE MODA_TABABD

      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)

      CHARACTER*128 BORT_STR
      CHARACTER*128 TABB1,TABB2
      CHARACTER*56  DXSTR
      CHARACTER*55  CSEQ
      CHARACTER*50  DXCMP
      CHARACTER*24  UNIT
      CHARACTER*8   NEMO
      CHARACTER*6   NUMB,CIDN
      DIMENSION     LDXBD(10),LDXBE(10)

      DIMENSION     MESG(*)

      DATA LDXBD /38,70,8*0/
      DATA LDXBE /42,42,8*0/

C-----------------------------------------------------------------------
      JA(I) = IA+1+LDA*(I-1)
      JB(I) = IB+1+LDB*(I-1)
C-----------------------------------------------------------------------

C  GET SOME PRELIMINARY INFORMATION FROM THE MESSAGE
C  -------------------------------------------------

      IDXS = IUPBS01(MESG,'MSBT')+1
      IF(IDXS.GT.IDXV+1) IDXS = IUPBS01(MESG,'MTVL')+1
      IF(LDXA(IDXS).EQ.0) GOTO 901
      IF(LDXB(IDXS).EQ.0) GOTO 901
      IF(LDXD(IDXS).EQ.0) GOTO 901

      CALL GETLENS(MESG,3,LEN0,LEN1,LEN2,LEN3,L4,L5)
      I3 = LEN0+LEN1+LEN2
      DXCMP = ' '
      JBIT = 8*(I3+7)
      CALL UPC(DXCMP,NXSTR(IDXS),MESG,JBIT,.FALSE.)
      IF(DXCMP.NE.DXSTR(IDXS)) GOTO 902

C  SECTION 4 - READ DEFINITIONS FOR TABLES A, B AND D
C  --------------------------------------------------

      LDA  = LDXA (IDXS)
      LDB  = LDXB (IDXS)
      LDD  = LDXD (IDXS)
      LDBD = LDXBD(IDXS)
      LDBE = LDXBE(IDXS)
      L30  = LD30 (IDXS)

      IA = I3+LEN3+5
      LA = IUPB(MESG,IA,8)
      IB = JA(LA+1)
      LB = IUPB(MESG,IB,8)
      ID = JB(LB+1)
      LD = IUPB(MESG,ID,8)

C  TABLE A
C  -------

      DO I=1,LA
        N = IGETNTBI(LUN,'A')
        JBIT = 8*(JA(I)-1)
        CALL UPC(TABA(N,LUN),LDA,MESG,JBIT,.TRUE.)
        NUMB = '   '//TABA(N,LUN)(1:3)
        NEMO = TABA(N,LUN)(4:11)
        CSEQ = TABA(N,LUN)(13:67)
        CALL STNTBIA(N,LUN,NUMB,NEMO,CSEQ)
      ENDDO

C  TABLE B
C  -------

      DO I=1,LB
        N = IGETNTBI(LUN,'B')
        JBIT = 8*(JB(I)-1)
        CALL UPC(TABB1,LDBD,MESG,JBIT,.TRUE.)
        JBIT = 8*(JB(I)+LDBD-1)
        CALL UPC(TABB2,LDBE,MESG,JBIT,.TRUE.)
        TABB(N,LUN) = TABB1(1:LDXBD(IDXV+1))//TABB2(1:LDXBE(IDXV+1))
        NUMB = TABB(N,LUN)(1:6)
        NEMO = TABB(N,LUN)(7:14)
        CALL NENUBD(NEMO,NUMB,LUN)
        IDNB(N,LUN) = IFXY(NUMB)
        UNIT = TABB(N,LUN)(71:94)
        CALL CAPIT(UNIT)
        TABB(N,LUN)(71:94) = UNIT
        NTBB(LUN) = N
      ENDDO

C  TABLE D
C  -------

      DO I=1,LD
        N = IGETNTBI(LUN,'D')
        JBIT = 8*ID
        CALL UPC(TABD(N,LUN),LDD,MESG,JBIT,.TRUE.)
        NUMB = TABD(N,LUN)(1:6)
        NEMO = TABD(N,LUN)(7:14)
        CALL NENUBD(NEMO,NUMB,LUN)
        IDND(N,LUN) = IFXY(NUMB)
        ND = IUPB(MESG,ID+LDD+1,8)
        IF(ND.GT.MAXCD) GOTO 903
        DO J=1,ND
          NDD = ID+LDD+2 + (J-1)*L30
          JBIT = 8*(NDD-1)
          CALL UPC(CIDN,L30,MESG,JBIT,.TRUE.)
          IDN = IDN30(CIDN,L30)
          CALL PKTDD(N,LUN,IDN,IRET)
          IF(IRET.LT.0) GOTO 904
        ENDDO
        ID = ID+LDD+1 + ND*L30
        IF(IUPB(MESG,ID+1,8).EQ.0) ID = ID+1
        NTBD(LUN) = N
      ENDDO

C  EXITS
C  -----

      RETURN
901   CALL BORT('BUFRLIB: STBFDX - UNEXPECTED DICTIONARY MESSAGE '//
     . 'SUBTYPE OR LOCAL VERSION NUMBER (E.G., L.V.N. HIGHER THAN '//
     . 'KNOWN)')
902   CALL BORT('BUFRLIB: STBFDX - UNEXPECTED DICTIONARY MESSAGE '//
     . 'CONTENTS')
903   WRITE(BORT_STR,'("BUFRLIB: STBFDX - NUMBER OF DESCRIPTORS IN '//
     . 'TABLE D ENTRY ",A," IN BUFR TABLE (",I4,") EXCEEDS THE LIMIT '//
     . ' (",I4,")")') NEMO,ND,MAXCD
      CALL BORT(BORT_STR)
904   CALL BORT('BUFRLIB: STBFDX - BAD RETURN FROM BUFRLIB ROUTINE '//
     . 'PKTDD, SEE PREVIOUS WARNING MESSAGE')
      END
