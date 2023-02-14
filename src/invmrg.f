C> @file
C> @brief Merge parts of data subsets
C>
C> @author J. Woollen @date 1996-10-09

C> This subroutine merges parts of data subsets which have duplicate
C> space and time coordinates but different or unique observational data.
C>
C> @param[in] LUBFI   -- integer: Fortran logical unit number for input
C>                       BUFR file
C> @param[in] LUBFJ   -- integer: Fortran logical unit number for output
C>                       BUFR file
C>
C> <p>Logical unit LUBFI should have already been opened for input
C> operations via a previous call to subroutine openbf().
C>
C> <p>Logical unit LUBFJ should have already been opened for output
C> operations via a previous call to subroutine openbf().
C>
C> @remarks
C> - This subroutine cannot merge parts of data subsets which are
C> contained within replication sequences.
C>
C> @author J. Woollen @date 1996-10-09
      RECURSIVE SUBROUTINE INVMRG(LUBFI,LUBFJ)

      USE MODA_USRINT
      USE MODA_TABLES
      USE MODV_IM8B

      COMMON /MRGCOM/ NRPL,NMRG,NAMB,NTOT

      CHARACTER*128 BORT_STR
      LOGICAL       HEREI,HEREJ,MISSI,MISSJ,SAMEI

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUBFI,MY_LUBFI,1)
         CALL X84(LUBFJ,MY_LUBFJ,1)
         CALL INVMRG(MY_LUBFI,MY_LUBFJ)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IS = 1
      JS = 1

C  GET THE UNIT POINTERS
C  ---------------------

      CALL STATUS(LUBFI,LUNI,IL,IM)
      CALL STATUS(LUBFJ,LUNJ,JL,JM)

C  STEP THROUGH THE BUFFERS COMPARING THE INVENTORY AND MERGING DATA
C  -----------------------------------------------------------------

      DO WHILE(IS.LE.NVAL(LUNI))

C  CHECK TO SEE WE ARE AT THE SAME NODE IN EACH BUFFER
C  ---------------------------------------------------

      NODE = INV(IS,LUNI)
      NODJ = INV(JS,LUNJ)
      IF(NODE.NE.NODJ) GOTO 900

      ITYP = ITP(NODE)

C  FOR TYPE 1 NODES DO AN ENTIRE SEQUENCE REPLACEMENT
C  --------------------------------------------------

      IF(ITYP.EQ.1) THEN
         IF(TYP(NODE).EQ.'DRB') IOFF = 0
         IF(TYP(NODE).NE.'DRB') IOFF = 1
         IWRDS = NWORDS(IS,LUNI)+IOFF
         JWRDS = NWORDS(JS,LUNJ)+IOFF
         IF(IWRDS.GT.IOFF .AND. JWRDS.EQ.IOFF) THEN
            DO N=NVAL(LUNJ),JS+1,-1
            INV(N+IWRDS-JWRDS,LUNJ) = INV(N,LUNJ)
            VAL(N+IWRDS-JWRDS,LUNJ) = VAL(N,LUNJ)
            ENDDO
            DO N=0,IWRDS
            INV(JS+N,LUNJ) = INV(IS+N,LUNI)
            VAL(JS+N,LUNJ) = VAL(IS+N,LUNI)
            ENDDO
            NVAL(LUNJ) = NVAL(LUNJ)+IWRDS-JWRDS
            JWRDS = IWRDS
            NRPL = NRPL+1
         ENDIF
         IS = IS+IWRDS
         JS = JS+JWRDS
      ENDIF

C  FOR TYPES 2 AND 3 FILL MISSINGS
C  -------------------------------

      IF((ITYP.EQ.2).OR.(ITYP.EQ.3)) THEN
         HEREI = IBFMS(VAL(IS,LUNI)).EQ.0
         HEREJ = IBFMS(VAL(JS,LUNJ)).EQ.0
         MISSI = .NOT.(HEREI)
         MISSJ = .NOT.(HEREJ)
         SAMEI = VAL(IS,LUNI).EQ.VAL(JS,LUNJ)
         IF(HEREI.AND.MISSJ) THEN
            VAL(JS,LUNJ) = VAL(IS,LUNI)
            NMRG = NMRG+1
         ELSEIF(HEREI.AND.HEREJ.AND..NOT.SAMEI) THEN
            NAMB = NAMB+1
         ENDIF
      ENDIF

C  BUMP THE COUNTERS AND GO CHECK THE NEXT PAIR
C  --------------------------------------------

      IS = IS + 1
      JS = JS + 1
      ENDDO

      NTOT = NTOT+1

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: INVMRG - NODE FROM INPUT BUFR FILE '//
     . '(",I7,") DOES NOT EQUAL NODE FROM OUTPUT BUFR FILE (",I7,"), '//
     . 'TABULAR MISMATCH")') NODE,NODJ
      CALL BORT(BORT_STR)
      END
