C> @file
C> @brief Store the subset template into internal arrays.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine stores the subset template into internal
C> subset arrays in module @ref moda_usrint for cases of node expansion,
C> such as when the node is either a Table A mnemonic or a delayed
C> replication factor.
C>
C> @param[in] LUN - integer: I/O stream index into internal memory arrays.
C> @param[in] INVN - integer: starting jump/link table index of the node
C> to be expanded within the subset template.
C> @param[in] NBMP - integer: number of times by which INVN is to be
C> expanded (i.e. number of replications of node).
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE USRTPL(LUN,INVN,NBMP)

      USE MODV_MAXJL
      USE MODV_MAXSS

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_TABLES
      USE MODA_IVTTMP

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      LOGICAL       DRP,DRS,DRB,DRX

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(IPRT.GE.2)  THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
         WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,I7,A,I5,A,A10)' )
     .      'BUFRLIB: USRTPL - LUN:INVN:NBMP:TAG(INODE(LUN)) = ',
     .      LUN, ':', INVN, ':', NBMP, ':', TAG(INODE(LUN))
         CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

      IF(NBMP.LE.0) THEN
         IF(IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT('BUFRLIB: USRTPL - NBMP .LE. 0 - IMMEDIATE RETURN')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ENDIF

      DRP = .FALSE.
      DRS = .FALSE.
      DRX = .FALSE.

C  SET UP A NODE EXPANSION
C  -----------------------

      IF(INVN.EQ.1) THEN
c  .... case where node is a Table A mnemonic (nodi is positional index)
         NODI = INODE(LUN)
         INV(1,LUN) = NODI
         NVAL(LUN)  = 1
         IF(NBMP.NE.1) GOTO 900
      ELSEIF(INVN.GT.0 .AND. INVN.LE.NVAL(LUN)) THEN
c  .... case where node is (hopefully) a delayed replication factor
         NODI = INV(INVN,LUN)
         DRP  = TYP(NODI) .EQ. 'DRP'
         DRS  = TYP(NODI) .EQ. 'DRS'
         DRB  = TYP(NODI) .EQ. 'DRB'
         DRX  = DRP .OR. DRS .OR. DRB
         IVAL = VAL(INVN,LUN)
         JVAL = 2**IBT(NODI)-1
         VAL(INVN,LUN) = IVAL+NBMP
         IF(DRB.AND.NBMP.NE.1) GOTO 901
         IF(.NOT.DRX         ) GOTO 902
         IF(IVAL.LT.0.       ) GOTO 903
         IF(IVAL+NBMP.GT.JVAL) GOTO 904
      ELSE
         GOTO 905
      ENDIF

C  RECALL A PRE-FAB NODE EXPANSION SEGMENT
C  ---------------------------------------

      NEWN = 0
      N1 = ISEQ(NODI,1)
      N2 = ISEQ(NODI,2)

      IF(N1.EQ.0          ) GOTO 906
      IF(N2-N1+1.GT.MAXJL)  GOTO 907

      DO N=N1,N2
      NEWN = NEWN+1
      ITMP(NEWN) = JSEQ(N)
      VTMP(NEWN) = VALI(JSEQ(N))
      ENDDO

C  MOVE OLD NODES - STORE NEW ONES
C  -------------------------------

      IF(NVAL(LUN)+NEWN*NBMP.GT.MAXSS) GOTO 908

      DO J=NVAL(LUN),INVN+1,-1
      INV(J+NEWN*NBMP,LUN) = INV(J,LUN)
      VAL(J+NEWN*NBMP,LUN) = VAL(J,LUN)
      ENDDO

      IF(DRP.OR.DRS) VTMP(1) = NEWN
      KNVN = INVN

      DO I=1,NBMP
      DO J=1,NEWN
      KNVN = KNVN+1
      INV(KNVN,LUN) = ITMP(J)
      VAL(KNVN,LUN) = VTMP(J)
      ENDDO
      ENDDO

C  RESET POINTERS AND COUNTERS
C  ---------------------------

      NVAL(LUN) = NVAL(LUN) + NEWN*NBMP

      IF(IPRT.GE.2)  THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
         WRITE ( UNIT=ERRSTR, FMT='(A,A,A10,2(A,I5),A,I7)' )
     .      'BUFRLIB: USRTPL - TAG(INV(INVN,LUN)):NEWN:NBMP:',
     .      'NVAL(LUN) = ', TAG(INV(INVN,LUN)), ':', NEWN, ':',
     .      NBMP, ':', NVAL(LUN)
         CALL ERRWRT(ERRSTR)
         DO I=1,NEWN
           WRITE ( UNIT=ERRSTR, FMT='(2(A,I5),A,A10)' )
     .      'For I = ', I, ', ITMP(I) = ', ITMP(I),
     .      ', TAG(ITMP(I)) = ', TAG(ITMP(I))
           CALL ERRWRT(ERRSTR)
         ENDDO
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

      IF(DRX) THEN
         NODE = NODI
         INVR = INVN
4        NODE = JMPB(NODE)
         IF(NODE.GT.0) THEN
            IF(ITP(NODE).EQ.0) THEN
               DO INVR=INVR-1,1,-1
               IF(INV(INVR,LUN).EQ.NODE) THEN
                  VAL(INVR,LUN) = VAL(INVR,LUN)+NEWN*NBMP
                  GOTO 4
               ENDIF
               ENDDO
               GOTO 909
            ELSE
               GOTO 4
            ENDIF
         ENDIF
      ENDIF

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: USRTPL - THIRD ARGUMENT (INPUT) = ",'//
     . 'I4,", MUST BE 1 WHEN SECOND ARGUMENT (INPUT) IS 1 (SUBSET '//
     . 'NODE) (",A,")")') NBMP,TAG(NODI)
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: USRTPL - THIRD ARGUMENT (INPUT) = ",'//
     . 'I4,", MUST BE 1 WHEN NODE IS DRB (1-BIT DELAYED REPL. FACTOR)'//
     . ' (",A,")")') NBMP,TAG(NODI)
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: USRTPL - NODE IS OF TYPE ",A," - IT '//
     . 'MUST BE EITHER A SUBSET OR DELAYED REPL. FACTOR (",A,")")')
     .  TYP(NODI),TAG(NODI)
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: USRTPL - REPLICATION FACTOR IS '//
     . 'NEGATIVE (=",I5,") (",A,")")') IVAL,TAG(NODI)
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: USRTPL - REPLICATION FACTOR OVERFLOW'//
     . ' (EXCEEDS MAXIMUM OF",I6," (",A,")")') JVAL,TAG(NODI)
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: USRTPL - INVENTORY INDEX {FIRST '//
     . 'ARGUMENT (INPUT)} OUT OF BOUNDS (=",I5,", RANGE IS 1 TO",I6,"'//
     . ') (",A,")")') INVN,NVAL(LUN),TAG(NODI)
      CALL BORT(BORT_STR)
906   WRITE(BORT_STR,'("BUFRLIB: USRTPL - UNSET EXPANSION SEGMENT (",'//
     . 'A,")")') TAG(NODI)
      CALL BORT(BORT_STR)
907   WRITE(BORT_STR,'("BUFRLIB: USRTPL - TEMPLATE ARRAY OVERFLOW, '//
     . 'EXCEEDS THE LIMIT (",I6,") (",A,")")') MAXJL,TAG(NODI)
      CALL BORT(BORT_STR)
908   WRITE(BORT_STR,'("BUFRLIB: USRTPL - INVENTORY OVERFLOW (",I6,")'//
     . ', EXCEEDS THE LIMIT (",I6,") (",A,")")')
     . NVAL(LUN)+NEWN*NBMP,MAXSS,TAG(NODI)
      CALL BORT(BORT_STR)
909   WRITE(BORT_STR,'("BUFRLIB: USRTPL - BAD BACKUP STRATEGY (",A,'//
     . '")")') TAG(NODI)
      CALL BORT(BORT_STR)
      END
