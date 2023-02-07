C> @file
C> @brief Search for a specified node within a specified portion
C> of the current data subset.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1999-11-18 | J. Woollen | Increased number of open bufr files to 32 (necessary for mpi).
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation); outputs more info when unusual things happen.
C> 2009-03-31 | J. Woollen | Added documentation.
C> 2009-04-21 | J. Ator    | Use errwrt().
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C>
C> @author Woollen @date 1994-01-06

C> This function looks for a specified node within the portion
c> of the current subset buffer bounded by the indices inv1 and inv2.
C> It is similar to BUFR archive library function invtag(), except that
C> invtag() searches based on the mnemonic corresponding to the node.
C>
C> @param[in] NODE - integer: jump/link table index to look for
C> @param[in] LUN - integer: i/o stream index into internal memory arrays
C> @param[in] INV1 - integer: starting index of the portion of the subset buffer in which to look
C> @param[in] INV2 - integer: ending index of the portion of the subset buffer in which to look
C>
C> @return - integer: location index of node within specified portion of subset buffer
C> - 0 not found
C>
C> @author Woollen @date 1994-01-06
      FUNCTION INVWIN(NODE,LUN,INV1,INV2)

      USE MODA_USRINT

      COMMON /QUIET/  IPRT

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      INVWIN = 0
      IF(NODE.EQ.0) GOTO 200

C  SEARCH BETWEEN INV1 AND INV2
C  ----------------------------

10    DO INVWIN=INV1,INV2
      IF(INV(INVWIN,LUN).EQ.NODE) GOTO 100
      ENDDO

      INVWIN = 0

 200  IF(IPRT.GE.2) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: INVWIN - RETURNING WITH A VALUE OF 0')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
