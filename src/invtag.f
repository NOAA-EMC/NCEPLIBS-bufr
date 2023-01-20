C> @file
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1999-11-18 | J. Woollen | The number of bufr files which can be opened at one time increased from 10 to 32 (necessary in order to process multiple bufr files under the mpi).
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | maxjl (maximum number of jump/link entries) increased from 15000 to 16000 (was in verification version); unified/portable for wrf; added documentation (including history); outputs more complete diagnostic info when unusual things happen.
C> 2009-03-31 | J. Woollen | Added documentation.
C> 2009-04-21 | J. Ator    | use errwrt().
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C>
C> @author Woollen @date 1994-01-06
      
C> This function looks for a specified mnemonic within the
C> portion of the current subset buffer bounded by the indices inv1
C> and inv2. It is similar to bufr archive library function invwin,
C> except that invwin searches based on the actual node within the
C> internal jump/link table, rather than on the mnemonic corresponding
C> to that node.
C>
C> @param[in] NODE - integer: jump/link table index of mnemonic to look for
C> @param[in] LUN - integer: i/o stream index into internal memory arrays
C> @param[in] INV1 - integer: starting index of the portion of the subset buffer in which to look
C> @param[in] INV2 - integer: ending index of the portion of the subset buffer in which to look
C>
C> @return - integer: location index of node within specified
C> portion of subset buffer, 0 = not found.
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

10    DO INVTAG=INV1,INV2
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
