C> @file
C> @brief Define a customized maximum length for output BUFR messages.

C> This subroutine allows the user to define the maximum length of a
C> BUFR message that can be written to an output file by the BUFRLIB
C> software.
C>
C> This subroutine can be called from within an application program at
C> any time after the initial call to subroutine openbf(), and the
C> specified value MAXO will then be used for all future BUFR messages
C> written by the software to all output files for the remainder of
C> the program, unless another call is made to this same subroutine
C> to reset the value of MAXO again.  Otherwise, if this subroutine
C> is never called, a default maximum message length is used for all
C> output files, as set via an initial internal call to subroutine
C> bfrini().
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 2002-05-14
C>
C> @param[in] MAXO -   integer: New maximum length (in bytes) for
C>                     all BUFR messages written to all output files
C>                     - 0 = Set MAXO to the maximum value allowed
C>                           by the BUFRLIB software
C>
C> <b>Program history log:</b>
C> - 2002-05-14  J. Woollen -- Original author
C> - 2004-08-09  J. Ator    -- Maximum message length increased from
C>                           20,000 to 50,000 bytes
C> - 2006-04-14  J. Ator    -- Added MAXO=0 option and overflow check
C> - 2009-03-23  D. Keyser  -- No longer prints the record length change
C>                           diagnostic if the requested record length
C>                           passed in as MAXO is actually the same as
C>                           the previous value
C> - 2009-04-21  J. Ator    -- Use errwrt()
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C> - 2015-09-24  D. Stokes  -- Correct typos in docblock
C>
      SUBROUTINE MAXOUT(MAXO)

      USE MODA_BITBUF

      COMMON /MAXCMP/ MAXCMB,MAXROW,MAXCOL,NCMSGS,NCSUBS,NCBYTS
      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)
      COMMON /QUIET / IPRT

      CHARACTER*128   ERRSTR
      CHARACTER*56    DXSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF((MAXO.EQ.0).OR.(MAXO.GT.MXMSGL)) THEN
         NEWSIZ = MXMSGL
      ELSE
         NEWSIZ = MAXO
      ENDIF

      IF(IPRT.GE.0) THEN
         IF(MAXBYT.NE.NEWSIZ) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I7,A,I7)' )
     . 'BUFRLIB: MAXOUT - THE RECORD LENGTH OF ALL BUFR MESSAGES ',
     . 'CREATED FROM THIS POINT ON IS BEING CHANGED FROM ', MAXBYT,
     . ' TO ', NEWSIZ
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
      ENDIF

      MAXBYT = NEWSIZ
      MAXCMB = NEWSIZ
      MAXDX  = NEWSIZ

      RETURN
      END
