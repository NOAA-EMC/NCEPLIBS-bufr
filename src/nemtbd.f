C> @file
C> @brief Get information about a Table D descriptor
C>
C> @author J. Woollen @date 1994-01-06

C> Return information about a Table D descriptor
C> from the internal DX BUFR tables.
C>
C> @param[in] LUN - integer: file ID associated with DX BUFR tables
C> @param[in] ITAB - integer: Positional index of descriptor within
C>                    internal Table D
C> @param[out] NSEQ - integer: Number of child mnemonics for descriptor
C> @param[out] NEMS - character*8(*): Child mnemonics
C> @param[out] IRPS - integer(*): Array of values corresponding to NEMS
C>                     - 5, if corresponding NEMS value is a Table D
C>                       mnemonic using 1-bit delayed replication
C>                     - 4, if corresponding NEMS value is a Table D
C>                       mnemonic using 8-bit delayed (stack) replication
C>                     - 3, if corresponding NEMS value is a Table D
C>                       mnemonic using 8-bit delayed replication
C>                     - 2, if corresponding NEMS value is a Table D
C>                       mnemonic using 16-bit delayed replication
C>                     - 1, if corresponding NEMS value is a Table D
C>                       mnemonic using regular (non-delayed) replication
C>                     - 0, otherwise
C> @param[out] KNTS -- integer(*): Array of values corresponding to NEMS
C>                     - Number of replications, if corresponding NEMS
C>                       value is a Table D mnemonic using regular
C>                       (non-delayed) replication
C>                     - 0, otherwise
C>
C> @remarks
C> - This subroutine does not recursively resolve any child mnemonics
C> which may themselves be Table D mnemonics.  Instead, this subroutine
C> only returns the list of mnemonics which are direct children of the
C> descriptor referenced by ITAB.  This information should have already
C> been stored into internal arrays via previous calls to subroutine
C> pktdd().
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE NEMTBD(LUN,ITAB,NSEQ,NEMS,IRPS,KNTS)

      USE MODV_MAXCD
      USE MODA_TABABD

      CHARACTER*128 BORT_STR
      CHARACTER*8   NEMO,NEMS,NEMT,NEMF
      CHARACTER*6   ADN30,CLEMON
      CHARACTER*1   TAB
      DIMENSION     NEMS(*),IRPS(*),KNTS(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(ITAB.LE.0 .OR. ITAB.GT.NTBD(LUN)) GOTO 900

C  CLEAR THE RETURN VALUES
C  -----------------------

      NSEQ = 0

      DO I=1,MAXCD
      NEMS(I) = ' '
      IRPS(I) = 0
      KNTS(I) = 0
      ENDDO

C  PARSE THE TABLE D ENTRY
C  -----------------------

      NEMO = TABD(ITAB,LUN)(7:14)
      IDSC = IDND(ITAB,LUN)
      CALL UPTDD(ITAB,LUN,0,NDSC)

C     Loop through each child mnemonic.

c  .... DK: What happens here if NDSC=0 ?
      DO J=1,NDSC
      IF(NSEQ+1.GT.MAXCD) GOTO 903
      CALL UPTDD(ITAB,LUN,J,IDSC)
c  .... get NEMT from IDSC
      CALL NUMTAB(LUN,IDSC,NEMT,TAB,IRET)
      IF(TAB.EQ.'R') THEN
         IF(IRET.LT.0) THEN

C           F=1 regular (i.e. non-delayed) replication.

            IRPS(NSEQ+1) = 1
            KNTS(NSEQ+1) = ABS(IRET)
         ELSEIF(IRET.GT.0) THEN

C           Delayed replication.

            IRPS(NSEQ+1) = IRET
         ENDIF
      ELSEIF(TAB.EQ.'F') THEN

C            Replication factor.

         IRPS(NSEQ+1) = IRET
      ELSEIF(TAB.EQ.'D'.OR.TAB.EQ.'C') THEN
         NSEQ = NSEQ+1
         NEMS(NSEQ) = NEMT
      ELSEIF(TAB.EQ.'B') THEN
         NSEQ = NSEQ+1
         IF((NEMT(1:1).EQ.'.').AND.(J.LT.NDSC)) THEN

C            This is a "following value" mnemonic.

            CALL UPTDD(ITAB,LUN,J+1,IDSC)
c  .... get NEMF from IDSC
            CALL NUMTAB(LUN,IDSC,NEMF,TAB,IRET)
            CALL RSVFVM(NEMT,NEMF)
         ENDIF
         NEMS(NSEQ) = NEMT
      ELSE
         GOTO 905
      ENDIF
      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NEMTBD - ITAB (",I7,") NOT FOUND IN '//
     . 'TABLE D")') ITAB
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: NEMTBD - THERE ARE MORE THAN '//
     . '(",I4,") DESCRIPTORS (THE LIMIT) IN TABLE D SEQUENCE '//
     . 'MNEMONIC ",A)') MAXCD, NEMO
      CALL BORT(BORT_STR)
905   CLEMON = ADN30(IDSC,6)
      WRITE(BORT_STR,'("BUFRLIB: NEMTBD - UNRECOGNIZED DESCRIPTOR '//
     . '",A," IN TABLE D SEQUENCE MNEMONIC ",A)') CLEMON,NEMO
      CALL BORT(BORT_STR)
      END
