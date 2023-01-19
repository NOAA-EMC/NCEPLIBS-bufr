C> @file
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced call to cray "abort" with call to routine bort().
C> 1999-11-18 | J. Woollen | The number of bufr files which can be opened at one time increased from 10 to 32 (necessary for mpi).
C> 2002-05-14 | J. Woollen | Removed old cray compiler directives.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation; outputs more diagnostic info.
C> 2009-03-31 | J. Woollen | Added additional documentation.
C> 2009-05-07 | J. Ator    | Use lstjpb instead of lstrpc.
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C>
C> @author Woollen @date 1994-01-06
      
C> Given a node index within the internal jump/link table, this
C> subroutine looks within the current subset buffer for a "window"
C> (see below remarks) which contains this node. If found, it returns
C> the starting and ending indices of this window within the current
C> subset buffer. For example, if the node is found within the subset
C> but is not part of a delayed replication sequence, then the returned
C> indices define the start and end of the entire subset buffer.
C> Otherwise, the returned indices define the start and end of the next
C> available delayed replication sequence iteration which contains the
C> node. If no further iterations of the sequence can be found, then
C> the starting index is returned with a value of zero.
C>
C> @note
C> This is one of a number of subroutines which operate on "windows"
C> (i.e. contiguous portions) of the internal subset buffer. The
C> subset buffer is an array of values arranged according to the
C> overall template definition for a subset. A window can be any
C> contiguous portion of the subset buffer up to and including the
C> entire subset buffer itself. For the purposes of these "window
C> operator" subroutines, a window essentially consists of all of the
C> elements within a particular delayed replication group, since such
C> groups effectively define the dimensions within a bufr subset for
C> the bufr archive library subroutines such as ufbint, ufbin3, etc.
C> which read/write individual data values. A bufr subset with no
C> delayed replication groups is considered to have only one
C> dimension, and therefore only one "window" which spans the entire
C> subset. On the other hand, each delayed replication sequence
C> within a bufr subset consists of some number of "windows", which
C> are a de-facto second dimension of the subset and where the number
C> of windows is the delayed descriptor replication factor (i.e. the
C> number of iterations) of the sequence. If nested delayed
C> replication is used, then there may be three or more dimensions
C> within the subset.
C>
C> @param[in] NODE - integer: jump/link table index of mnemonic to look for.
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C> @param[out] JWIN - integer: ending index of the previous window iteration which contained node.
C> @param[out] IWIN - integer: starting index of the current window iteration which
C> ontains node 0 = not found or no more iterations available.
C> @param[out] JWIN - integer: ending index of the current window iteration which contains node .
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE GETWIN(NODE,LUN,IWIN,JWIN)

      USE MODA_USRINT

      CHARACTER*128 BORT_STR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IRPC = LSTJPB(NODE,LUN,'RPC')

      IF(IRPC.EQ.0) THEN
         IWIN = INVWIN(NODE,LUN,JWIN,NVAL(LUN))
         IF(IWIN.EQ.0 .and. JWIN.GT.1) GOTO 100
         IWIN = 1
         JWIN = NVAL(LUN)
         GOTO 100
      ELSE
         IWIN = INVWIN(IRPC,LUN,JWIN,NVAL(LUN))
         IF(IWIN.EQ.0) THEN
            GOTO 100
         ELSEIF(VAL(IWIN,LUN).EQ.0.) THEN
            IWIN = 0
            GOTO 100
         ENDIF
      ENDIF

      JWIN = INVWIN(IRPC,LUN,IWIN+1,NVAL(LUN))
      IF(JWIN.EQ.0) GOTO 900

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: GETWIN - SEARCHED BETWEEN",I5," AND"'//
     . ',I5,", MISSING BRACKET")') IWIN+1,NVAL(LUN)
      CALL BORT(BORT_STR)
      END
