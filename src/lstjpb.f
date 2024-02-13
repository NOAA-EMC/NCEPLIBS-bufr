C> @file
C> @brief Search backwards from a specified node of the jump/link table
C> for a node of a specified type.
C>
C> @author Woollen @date 1994-01-06

C> Search backwards, beginning from a given
C> node within the jump/link table, until finding the most recent
C> node of type jbtyp.  The internal jmpb array is used to jump
C> backwards within the jump/link table, and the function returns
C> the table index of the found node.  If the input node itself is
C> of type jbtyp, then the function simply returns the index of that
C> same node.
C>
C> @note See module @ref moda_tables for an
C> explanation of the various node types present within an internal
C> jump/link table.
C>
C> @param[in] NODE - integer: jump/link table index of entry to begin
C> searching backwards from
C> @param[in] LUN - integer: file ID
C> @param[in] JBTYP - character*(*): type of node for which to search
C>
C> @return - integer: index of first node of type jbtyp found by
C> jumping backwards from input node
C> - 0 no such node found
C>
C> @author Woollen @date 1994-01-06
      FUNCTION LSTJPB(NODE,LUN,JBTYP)

      use moda_msgcwd
      use moda_tables

      CHARACTER*(*) JBTYP
      CHARACTER*128 BORT_STR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IF(NODE.LT.INODE(LUN)) GOTO 900
      IF(NODE.GT.ISC(INODE(LUN))) GOTO 901

      NOD = NODE

C  FIND THIS OR THE PREVIOUS "JBTYP" NODE
C  --------------------------------------

10    IF(TYP(NOD).NE.JBTYP) THEN
         NOD = JMPB(NOD)
         IF(NOD.NE.0) GOTO 10
      ENDIF

      LSTJPB = NOD

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: LSTJPB - TABLE NODE (",I7,") IS OUT '//
     . 'OF BOUNDS, < LOWER BOUNDS (",I7,")")')
     . NODE,INODE(LUN)
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: LSTJPB - TABLE NODE (",I7,") IS OUT '//
     . 'OF BOUNDS, > UPPER BOUNDS (",I7,")")')
     . NODE,ISC(INODE(LUN))
      CALL BORT(BORT_STR)
      END
