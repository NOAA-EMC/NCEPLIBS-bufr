C> @file
C> @brief Store information about a child mnemonic within the internal arrays
C>
C> @author Woollen @date 1994-01-06

C> This subroutine stores information about a "child"
C> mnemonic within the internal bufr table D entry (in module
C> tababd) for a table D sequence ("parent") mnemonic when the
C> "child" mnemonic is contained within the sequence represented by
C> the "parent" mnemonic (as determined within seqsdx()).
C>
C> @param[in] ID - integer: positional index of parent mnemonic within internal bufr table d array tabd(*,*).
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C> @param[in] IDN - integer: bit-wise representation of fxy value corresponding to child mnemonic.
C> - 0 = delete all information about all child mnemonics from within TABD(ID,LUN).
C> @param[out] IRET - integer: total number of child mnemonics stored thus far
C> (including idn) for the parent mnemonic given by tabd(id,lun).
C> - 0 information was cleared from TABD(ID,LUN) because input IDN value was 0
C> - -1 bad counter value or maximum number of child mnemonics already stored
C> for this parent mnemonic
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE PKTDD(ID,LUN,IDN,IRET)

      USE MODV_MAXCD
      USE MODA_TABABD

      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)
      COMMON /QUIET / IPRT

      CHARACTER*128 ERRSTR
      CHARACTER*56  DXSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LDD = LDXD(IDXV+1)+1

C     LDD points to the byte within TABD(ID,LUN) which contains (in
C     packed integer format) a count of the number of child mnemonics
C     stored thus far for this parent mnemonic.

C  ZERO THE COUNTER IF IDN IS ZERO
C  -------------------------------

      IF(IDN.EQ.0) THEN
         CALL IPKM(TABD(ID,LUN)(LDD:LDD),1,0)
         IRET = 0
         GOTO 100
      ENDIF

C  UPDATE THE STORED DESCRIPTOR COUNT FOR THIS TABLE D ENTRY
C  ---------------------------------------------------------

      ND = IUPM(TABD(ID,LUN)(LDD:LDD),8)

C     ND is the (unpacked) count of the number of child mnemonics
C     stored thus far for this parent mnemonic.

      IF(ND.LT.0 .OR. ND.EQ.MAXCD) THEN
      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
         IF(ND.LT.0) THEN
            WRITE ( UNIT=ERRSTR, FMT='(A,I4,A)' )
     .        'BUFRLIB: PKTDD - BAD COUNTER VALUE (=', ND,
     .        ') - RETURN WITH IRET = -1'
         ELSE
            WRITE ( UNIT=ERRSTR, FMT='(A,I4,A,A)' )
     .        'BUFRLIB: PKTDD - MAXIMUM NUMBER OF CHILD MNEMONICS (=',
     .        MAXCD, ') ALREADY STORED FOR THIS PARENT - RETURN WITH ',
     .        'IRET = -1'
         ENDIF
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
         IRET = -1
         GOTO 100
      ELSE
         ND = ND+1
         CALL IPKM(TABD(ID,LUN)(LDD:LDD),1,ND)
         IRET = ND
      ENDIF

C  PACK AND STORE THE DESCRIPTOR
C  -----------------------------

      IDM = LDD+1 + (ND-1)*2

C     IDM points to the starting byte within TABD(ID,LUN) at which
C     the IDN value for this child mnemonic will be stored (as a
C     packed integer of width = 2 bytes).

      CALL IPKM(TABD(ID,LUN)(IDM:IDM),2,IDN)

C  EXIT
C  ----

100   RETURN
      END
