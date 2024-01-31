C> @file
C> @brief Search for all occurrences of a specified node within a
C> specified portion of the current data subset.
C>
C> @author Woollen @date 1994-01-06

C> Search for and return all occurrences of a
C> specified node within the portion of the current subset buffer
C> bounded by the indices inv1 and inv2. The resulting list is a
C> stack of "event" indices for the requested node.
C>
C> @param[in] NODE - integer: jump/link table index to look for.
C> @param[in] LUN - integer: file ID.
C> @param[in] INV1 - integer: starting index of the portion of the subset buffer in which to look.
C> @param[in] INV2 - integer: ending index of the portion of the subset buffer in which to look.
C> @param[out] INVN - integer: array of stack "event" indices for node.
C> @param[in] NMAX - integer: dimensioned size of invn; used by the function to ensure
C> that it does not overflow the invn array.
C>
C> @return number of indices within invn.
C>
C> @author Woollen @date 1994-01-06
      FUNCTION NVNWIN(NODE,LUN,INV1,INV2,INVN,NMAX)

      USE MODA_USRINT

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR
      DIMENSION     INVN(NMAX)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      NVNWIN = 0

      IF(NODE.EQ.0) THEN
         IF(IPRT.GE.1) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT('BUFRLIB: NVNWIN - NODE=0, IMMEDIATE RETURN')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ENDIF

      DO I=1,NMAX
         INVN(I) = 1E9
      ENDDO

C  SEARCH BETWEEN INV1 AND INV2
C  ----------------------------

      DO N=INV1,INV2
         IF(INV(N,LUN).EQ.NODE) THEN
            IF(NVNWIN+1.GT.NMAX) GOTO 900
            NVNWIN = NVNWIN+1
            INVN(NVNWIN) = N
         ENDIF
      ENDDO

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NVNWIN - THE NUMBER OF EVENTS '//
     . 'EXCEEDS THE LIMIT NMAX (",I5,")")') NMAX
      CALL BORT(BORT_STR)
      END
