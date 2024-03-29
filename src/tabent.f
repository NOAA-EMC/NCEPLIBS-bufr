C> @file
C> @brief Store an entry for a Table B or D mnemonic within the internal jump/link table.
C>
C> @author Woollen @date 1994-01-06

C> Build and store an entry for a Table B or
C> Table D mnemonic within the internal jump/link table.
C>
C> @param[in] LUN - integer: file ID.
C> @param[in] NEMO - character*8: Table B or D mnemonic to store in jump/link table.
C> @param[in] TAB - character*1: internal BUFR table array ('B' or 'D') in which NEMO is defined.
C> @param[in] ITAB - integer: positional index of NEMO within TAB.
C> @param[in] IREP - integer: positional index within common /reptab/ arrays,
C> for use when NEMO is replicated:
C> - 0, if NEMO is not replicated.
C> @param[in] IKNT - integer: number of replications, for use when NEMO is
C> replicated using F=1 regular (i.e., non-delayed) replication:
C> - 0, if NEMO is not replicated using F=1 regular (i.e., non-delayed) replication
C> @param[in] JUM0 - integer: index value to be stored for NEMO within internal jump/link table array jmpb(*).
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE TABENT(LUN,NEMO,TAB,ITAB,IREP,IKNT,JUM0)

      use modv_vars, only: mxnrv

      use moda_tables
      use moda_nrv203

C     Note that the values within the COMMON /REPTAB/ arrays were
C     initialized within subroutine BFRINI.

      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)

      COMMON /TABCCC/ ICDW,ICSC,ICRV,INCW

      CHARACTER*24  UNIT
      CHARACTER*10  RTAG
      CHARACTER*8   NEMO
      CHARACTER*3   TYPS,TYPT
      CHARACTER*1   REPS,TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  MAKE A JUMP/LINK TABLE ENTRY FOR A REPLICATOR
C  ---------------------------------------------

      IF(IREP.NE.0) THEN
         RTAG = REPS(IREP,1)//NEMO
         DO I=1,10
         IF(RTAG(I:I).EQ.' ') THEN
            RTAG(I:I) = REPS(IREP,2)
            CALL INCTAB(RTAG,TYPS(IREP,1),NODE)
            JUMP(NODE) = NODE+1
            JMPB(NODE) = JUM0
            LINK(NODE) = 0
            IBT (NODE) = LENS(IREP)
            IRF (NODE) = 0
            ISC (NODE) = 0
            IF(IREP.EQ.1) IRF(NODE) = IKNT
            JUM0 = NODE
            GOTO 1
         ENDIF
         ENDDO
      ENDIF

C  MAKE AN JUMP/LINK ENTRY FOR AN ELEMENT OR A SEQUENCE
C  ----------------------------------------------------

1     IF(TAB.EQ.'B') THEN

         CALL NEMTBB(LUN,ITAB,UNIT,ISCL,IREF,IBIT)
         IF(UNIT(1:5).EQ.'CCITT') THEN
            TYPT = 'CHR'
         ELSE
            TYPT = 'NUM'
         ENDIF
         CALL INCTAB(NEMO,TYPT,NODE)
         JUMP(NODE) = 0
         JMPB(NODE) = JUM0
         LINK(NODE) = 0
         IBT (NODE) = IBIT
         IRF (NODE) = IREF
         ISC (NODE) = ISCL
         IF(UNIT(1:4).EQ.'CODE') THEN
            TYPT = 'COD'
         ELSEIF(UNIT(1:4).EQ.'FLAG') THEN
            TYPT = 'FLG'
         ENDIF

         IF( (TYPT.EQ.'NUM') .AND. (IBTNRV.NE.0) ) THEN

C           This node contains a new (redefined) reference value.

            IF(NNRV+1.GT.MXNRV) GOTO 902
            NNRV = NNRV+1
            TAGNRV(NNRV) = NEMO
            INODNRV(NNRV) = NODE
            ISNRV(NNRV) = NODE+1
            IBT(NODE) = IBTNRV
            IF(IPFNRV.EQ.0) IPFNRV = NNRV
         ELSEIF( (TYPT.EQ.'NUM') .AND. (NEMO(1:3).NE.'204') ) THEN
            IBT(NODE) = IBT(NODE) + ICDW
            ISC(NODE) = ISC(NODE) + ICSC
            IRF(NODE) = IRF(NODE) * ICRV
         ELSEIF( (TYPT.EQ.'CHR') .AND. (INCW.GT.0) ) THEN
            IBT(NODE) = INCW * 8
         ENDIF

      ELSE  ! TAB.EQ.'D'

         IF(IREP.EQ.0) THEN
            TYPT = 'SEQ'
         ELSE
            TYPT = TYPS(IREP,2)
         ENDIF
         CALL INCTAB(NEMO,TYPT,NODE)
         JUMP(NODE) = NODE+1
         JMPB(NODE) = JUM0
         LINK(NODE) = 0
         IBT (NODE) = 0
         IRF (NODE) = 0
         ISC (NODE) = 0

      ENDIF

C  EXITS
C  -----

      RETURN
902   CALL BORT('BUFRLIB: TABENT - MXNRV OVERFLOW')
      END
