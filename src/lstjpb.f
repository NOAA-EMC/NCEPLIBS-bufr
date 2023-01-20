C> @file
C> @brief Search backwards, beginning from a given
C> node within the jump/link table, until we find the most recent
C> node of type jbtyp.      
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced call to cray library routine "abort" with call to bort().
C> 1999-11-18 | J. Woollen | The number of bufr files which can be opened at one time increased from 10 to 32.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation; outputs more info.
C> 2009-03-31 | J. Woollen | Added additional documentation.
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C>
C> @author Woollen @date 1994-01-06
      
C> This function searches backwards, beginning from a given
C> node within the jump/link table, until it finds the most recent
C> node of type jbtyp. The internal jmpb array is used to jump
C> backwards within the jump/link table, and the function returns
C> the table index of the found node. If the input node itself is
C> of type jbtyp, then the function simply returns the index of that
C> same node. 
C>
C> @note See tabsub() for an
C> explanation of the various node types present within an internal
C> jump/link table. 
C>
C> @param[in] NODE - integer: jump/link table index of entry to begin
C> searching backwards from
C> @param[in] LUN - integer: i/o stream index into internal memory arrays
C> @param[in] JBTYP - character*(*): type of node for which to search
C>
C> @return - integer: index of first node of type jbtyp found by
C> jumping backwards from input node, 0 = no such node found.
C>
C> @author Woollen @date 1994-01-06
      FUNCTION LSTJPB(NODE,LUN,JBTYP)

      USE MODA_MSGCWD
      USE MODA_TABLES

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
     . 'OF BOUNDS, < LOWER BOUNDS (",I7,"); TAG IS ",A10)')
     . NODE,INODE(LUN),TAG(NODE)
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: LSTJPB - TABLE NODE (",I7,") IS OUT '//
     . 'OF BOUNDS, > UPPER BOUNDS (",I7,"); TAG IS ",A10)')
     . NODE,ISC(INODE(LUN)),TAG(NODE)
      CALL BORT(BORT_STR)
      END
