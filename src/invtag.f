C> @file
C> @brief Search for a specified mnemonic within a specified portion
C> of the current data subset.
C>
C> @author Woollen @date 1994-01-06

C> Search for a specified mnemonic within the
C> portion of the current subset buffer bounded by the indices inv1
C> and inv2.
C>
C> This function is similar to library function invwin(),
C> except that invwin() searches based on the actual node within the
C> internal jump/link table, rather than on the mnemonic corresponding
C> to that node.
C>
C> @param[in] NODE - integer: jump/link table index of mnemonic to look for
C> @param[in] LUN - integer: file ID
C> @param[in] INV1 - integer: starting index of the portion of the subset buffer in which to look
C> @param[in] INV2 - integer: ending index of the portion of the subset buffer in which to look
C>
C> @return - integer: location index of node within specified portion of subset buffer
C> - 0 not found
C>
C> @author Woollen @date 1994-01-06
      FUNCTION INVTAG(NODE,LUN,INV1,INV2)

      USE MODA_USRINT
      USE MODA_TABLES

      COMMON /QUIET/  IPRT

      CHARACTER*10 TAGN

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      INVTAG = 0
      IF(NODE.EQ.0) GOTO 200
      TAGN = TAG(NODE)

C  SEARCH BETWEEN INV1 AND INV2
C  ----------------------------

      DO INVTAG=INV1,INV2
      IF(TAG(INV(INVTAG,LUN)).EQ.TAGN) GOTO 100
      ENDDO

      INVTAG = 0

200   IF(IPRT.GE.2) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: INVTAG - RETURNING WITH A VALUE OF 0')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
