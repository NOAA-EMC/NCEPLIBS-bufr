C> @file
C> @brief Build the internal jump/link table.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine constructs the internal jump/link table within
C> module tables, using all of the internal BUFR table array information
C> from module @ref moda_tababd for all of the internal I/O streams that are
C> currently defined to the library in module @ref moda_stbfr.
C>
C> The entire jump/link table will always be completely reconstructed
C> from scratch, even if some of the information within the internal
C> BUFR table arrays already existed there at the time of the previous
C> call to this subroutine, because there may have been other events
C> that have taken place since the previous call to this subroutine and
C> which haven't yet been reflected within the internal jump/link table.
C> For example, an I/O stream may have recently been unlinked from the
C> library via an internal call to subroutine closbf(), or the DX BUFR
C> tables associated with an I/O stream may have changed.
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE MAKESTAB

      USE MODV_BMISS
      USE MODV_MAXJL
      USE MODV_NFILES

      USE MODA_USRINT
      USE MODA_STBFR
      USE MODA_LUSHR
      USE MODA_XTAB
      USE MODA_TABABD
      USE MODA_TABLES
      USE MODA_NRV203
      USE MODA_BITMAPS

      COMMON /QUIET/  IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8   NEMO
      LOGICAL       EXPAND

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  RESET POINTER TABLE AND STRING CACHE
C  ------------------------------------

      NTAB = 0
      NNRV = 0
      NTAMC = 0
      CALL STRCLN

C  FIGURE OUT WHICH UNITS SHARE TABLES
C  -----------------------------------

C     The LUS array is static between calls to this subroutine, and it
C     keeps track of which logical units share dictionary table
C     information:
C        if LUS(I) = 0, then IOLUN(I) does not share dictionary table
C                       information with any other logical unit
C        if LUS(I) > 0, then IOLUN(I) shares dictionary table
C                       information with logical unit IOLUN(LUS(I))
C        if LUS(I) < 0, then IOLUN(I) does not now, but at one point in
C                       the past, shared dictionary table information
C                       with logical unit IOLUN(ABS(LUS(I)))

C     The XTAB array is non-static and is recomputed within the below
C     loop during each call to this subroutine:
C        if XTAB(I) = .TRUE., then the dictionary table information
C                             has changed for IOLUN(I) since the last
C                             call to this subroutine
C        if XTAB(I) = .FALSE., then the dictionary table information
C                              has not changed for IOLUN(I) since the
C                              last call to this subroutine

      DO LUN=1,NFILES
        XTAB(LUN) = .FALSE.
        IF(IOLUN(LUN).EQ.0) THEN

C          Logical unit IOLUN(LUN) is not defined to the BUFRLIB.

           LUS(LUN) = 0
        ELSE IF(MTAB(1,LUN).EQ.0) THEN

C          New dictionary table information has been read for logical
C          unit IOLUN(LUN) since the last call to this subroutine.

           XTAB(LUN) = .TRUE.
           IF(LUS(LUN).NE.0) THEN
             IF(IOLUN(ABS(LUS(LUN))).EQ.0) THEN
               LUS(LUN) = 0
             ELSE IF(LUS(LUN).GT.0) THEN

C              IOLUN(LUN) was sharing table information with logical
C              unit IOLUN(LUS(LUN)), so check whether the table
C              information has really changed.  If not, then IOLUN(LUN)
C              just re-read a copy of the exact same table information
C              as before, and therefore it can continue to share with
C              logical unit IOLUN(LUS(LUN)).

               IF(ICMPDX(LUS(LUN),LUN).EQ.1) THEN
                 XTAB(LUN) = .FALSE.
                 CALL CPBFDX(LUS(LUN),LUN)
               ELSE
                 LUS(LUN) = (-1)*LUS(LUN)
               ENDIF
             ELSE IF(ICMPDX(ABS(LUS(LUN)),LUN).EQ.1) THEN

C              IOLUN(LUN) was not sharing table information with logical
C              unit IOLUN(LUS(LUN)), but it did at one point in the past
C              and now once again has the same table information as that
C              logical unit.  Since the two units shared table
C              information at one point in the past, allow them to do
C              so again.

               XTAB(LUN) = .FALSE.
               LUS(LUN) = ABS(LUS(LUN))
               CALL CPBFDX(LUS(LUN),LUN)
             ENDIF
           ENDIF
        ELSE IF(LUS(LUN).GT.0) THEN

C          Logical unit IOLUN(LUN) is sharing table information with
C          logical unit IOLUN(LUS(LUN)), so make sure that the latter
C          unit is still defined to the BUFRLIB.

           IF(IOLUN(LUS(LUN)).EQ.0) THEN
             LUS(LUN) = 0
           ELSE IF( XTAB(LUS(LUN)) .AND.
     +             (ICMPDX(LUS(LUN),LUN).EQ.0) ) THEN

C            The table information for logical unit IOLUN(LUS(LUN))
C            just changed (in midstream).  If IOLUN(LUN) is an output
C            file, then we will have to update it with the new table
C            information later on in this subroutine.  Otherwise,
C            IOLUN(LUN) is an input file and is no longer sharing
C            tables with IOLUN(LUS(LUN)).

             IF(IOLUN(LUN).LT.0) LUS(LUN) = (-1)*LUS(LUN)
           ENDIF
        ELSE

C          Determine whether logical unit IOLUN(LUN) is sharing table
C          information with any other logical units.

           LUM = 1
           DO WHILE ((LUM.LT.LUN).AND.(LUS(LUN).EQ.0))
              IF(ISHRDX(LUM,LUN).EQ.1) THEN
                 LUS(LUN) = LUM
              ELSE
                 LUM = LUM+1
              ENDIF
           ENDDO
        ENDIF
      ENDDO

C  INITIALIZE JUMP/LINK TABLES WITH SUBSETS/SEQUENCES/ELEMENTS
C  -----------------------------------------------------------

      DO LUN=1,NFILES

       IF(IOLUN(LUN).NE.0 .AND. NTBA(LUN).GT.0) THEN

C        Reset any existing inventory pointers.

         IF(IOMSG(LUN).NE.0) THEN
            IF(LUS(LUN).LE.0) THEN
              INC = (NTAB+1)-MTAB(1,LUN)
            ELSE
              INC = MTAB(1,LUS(LUN))-MTAB(1,LUN)
            ENDIF
            DO N=1,NVAL(LUN)
              INV(N,LUN) = INV(N,LUN)+INC
            ENDDO
         ENDIF

         IF(LUS(LUN).LE.0) THEN

C           The dictionary table information corresponding to logical
C           unit IOLUN(LUN) has not yet been written into the internal
C           jump/link table, so add it in now.

            CALL CHEKSTAB(LUN)
            DO ITBA=1,NTBA(LUN)
              INOD = NTAB+1
              NEMO = TABA(ITBA,LUN)(4:11)
              CALL TABSUB(LUN,NEMO)
              MTAB(ITBA,LUN) = INOD
              ISC(INOD)      = NTAB
            ENDDO
         ELSE IF( XTAB(LUS(LUN)) .AND.
     +           (ICMPDX(LUS(LUN),LUN).EQ.0) ) THEN

C           Logical unit IOLUN(LUN) is an output file that is sharing
C           table information with logical unit IOLUN(LUS(LUN)) whose
C           table just changed (in midstream).  Flush any existing data
C           messages from IOLUN(LUN), then update the table information
C           for this logical unit with the corresponding new table
C           information from IOLUN(LUS(LUN)), then update IOLUN(LUN)
C           itself with a copy of the new table information.

            LUNIT = ABS(IOLUN(LUN))
            IF(IOMSG(LUN).NE.0) CALL CLOSMG(LUNIT)
            CALL CPBFDX(LUS(LUN),LUN)
            LUNDX = ABS(IOLUN(LUS(LUN)))
            CALL WRDXTB(LUNDX,LUNIT)
         ENDIF

       ENDIF

      ENDDO

C  STORE TYPES AND INITIAL VALUES AND COUNTS
C  -----------------------------------------

      DO NODE=1,NTAB
      IF(TYP(NODE).EQ.'SUB') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 1
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'SEQ') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 1
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'RPC') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 0
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'RPS') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 0
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'REP') THEN
         VALI(NODE) = BMISS
         KNTI(NODE) = IRF(NODE)
         ITP (NODE) = 0
      ELSEIF(TYP(NODE).EQ.'DRS') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 1
         ITP (NODE) = 1
      ELSEIF(TYP(NODE).EQ.'DRP') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 1
         ITP (NODE) = 1
      ELSEIF(TYP(NODE).EQ.'DRB') THEN
         VALI(NODE) = 0
         KNTI(NODE) = 0
         ITP (NODE) = 1
      ELSEIF(TYP(NODE).EQ.'NUM') THEN
         VALI(NODE) = BMISS
         KNTI(NODE) = 1
         ITP (NODE) = 2
      ELSEIF(TYP(NODE).EQ.'CHR') THEN
         VALI(NODE) = BMISS
         KNTI(NODE) = 1
         ITP (NODE) = 3
      ELSE
         GOTO 901
      ENDIF
      ENDDO

C  SET UP EXPANSION SEGMENTS FOR TYPE 'SUB', 'DRP', AND 'DRS' NODES
C  ----------------------------------------------------------------

      NEWN = 0

      DO N=1,NTAB
      ISEQ(N,1) = 0
      ISEQ(N,2) = 0
      EXPAND = TYP(N).EQ.'SUB' .OR. TYP(N).EQ.'DRP' .OR. TYP(N).EQ.'DRS'
     .                         .OR. TYP(N).EQ.'REP' .OR. TYP(N).EQ.'DRB'
      IF(EXPAND) THEN
         ISEQ(N,1) = NEWN+1
         NODA = N
         NODE = N+1
         DO K=1,MAXJL
         KNT(K) = 0
         ENDDO
         IF(TYP(NODA).EQ.'REP') KNT(NODE) = KNTI(NODA)
         IF(TYP(NODA).NE.'REP') KNT(NODE) = 1

1        NEWN = NEWN+1
         IF(NEWN.GT.MAXJL) GOTO 902
         JSEQ(NEWN) = NODE
         KNT(NODE) = MAX(KNTI(NODE),KNT(NODE))
2        IF(JUMP(NODE)*KNT(NODE).GT.0) THEN
            NODE = JUMP(NODE)
            GOTO 1
         ELSE IF(LINK(NODE).GT.0) THEN
            NODE = LINK(NODE)
            GOTO 1
         ELSE
            NODE = JMPB(NODE)
            IF(NODE.EQ.NODA) GOTO 3
            IF(NODE.EQ.0   ) GOTO 903
            KNT(NODE) = MAX(KNT(NODE)-1,0)
            GOTO 2
         ENDIF
3        ISEQ(N,2) = NEWN
      ENDIF
      ENDDO

C  PRINT THE SEQUENCE TABLES
C  ------------------------

      IF(IPRT.GE.2) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
         DO I=1,NTAB
           WRITE ( UNIT=ERRSTR, FMT='(A,I5,2X,A10,A5,6I8)' )
     .      'BUFRLIB: MAKESTAB ', I, TAG(I), TYP(I), JMPB(I), JUMP(I),
     .      LINK(I), IBT(I), IRF(I), ISC(I)
           CALL ERRWRT(ERRSTR)
         ENDDO
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: MAKESTAB - MNEMONIC ",A," IS '//
     . 'DUPLICATED IN SUBSET: ",A)') NEMO,TAG(N1)
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: MAKESTAB - UNKNOWN TYPE ",A)')TYP(NODE)
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: MAKESTAB - NUMBER OF JSEQ ENTRIES IN'//
     . ' JUMP/LINK TABLE EXCEEDS THE LIMIT (",I6,")")') MAXJL
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: MAKESTAB - NODE IS ZERO, FAILED TO '//
     . 'CIRCULATE (TAG IS ",A,")")') TAG(N)
      CALL BORT(BORT_STR)
      END
