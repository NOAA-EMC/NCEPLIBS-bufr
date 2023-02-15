C> @file
C> @brief Define a customized maximum length for output BUFR messages.
C>
C> @authors J. Woollen, J. Ator @date 2002-05-14

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
C> @param[in] MAXO --  integer: New maximum length (in bytes) for
C>                     all BUFR messages written to all output files
C>                     - 0 = Set MAXO to the maximum value allowed
C>                           by the BUFRLIB software
C>
C> @authors J. Woollen, J. Ator @date 2002-05-14
      RECURSIVE SUBROUTINE MAXOUT(MAXO)

      USE MODV_MXMSGL
      USE MODV_IM8B

      USE MODA_BITBUF

      COMMON /MAXCMP/ MAXCMB,MAXROW,MAXCOL,NCMSGS,NCSUBS,NCBYTS
      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)
      COMMON /QUIET / IPRT

      CHARACTER*128   ERRSTR
      CHARACTER*56    DXSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     CHECK FOR I8 INTEGERS

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(MAXO,MY_MAXO,1)
         CALL MAXOUT(MY_MAXO)

         IM8B=.TRUE.
         RETURN
      ENDIF

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
