C> @file
C> @brief Close and write the current message to a BUFR file that was
C> previously opened for writing.
C>
C> @author J. Woollen, D. Keyser @date 1994-01-06

C> Close the BUFR message that is currently open for
C> writing within internal arrays associated with logical unit
C> ABS(LUNIN), then write the message to that logical unit.
C>
C> Logical unit ABS(LUNIN) should have already been opened for output
C> operations via a previous call to subroutine openbf().
C>
C> If LUNIN < 0, then any message containing zero data subsets will
C> not be written to logical unit ABS(LUNIN) for the remainder of the
C> life of the application program.  This includes suppressing the
C> writing of any "dummy" messages containing dump center and initiation
C> times that normally appear in the first 2 messages of NCEP dump files.
C>
C> @param[in] LUNIN - integer: Absolute value is Fortran logical unit
C> number for BUFR file
C>
C> @author J. Woollen, D. Keyser @date 1994-01-06

      RECURSIVE SUBROUTINE CLOSMG(LUNIN)

      USE MODA_MSGCWD
      USE MODA_MSGLIM
      USE MODA_BITBUF
      USE MODV_IM8B

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIN,MY_LUNIN,1)
         CALL CLOSMG(MY_LUNIN)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUS
C  ---------------------

      LUNIT = ABS(LUNIN)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(LUNIT.NE.LUNIN) MSGLIM(LUN) = 0
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.NE.0) THEN
         IF(NSUB(LUN).GT.0) THEN
            CALL MSGWRT(LUNIT,MBAY(1,LUN),MBYT(LUN))
         ELSE IF(NSUB(LUN).EQ.0.AND.NMSG(LUN).LT.MSGLIM(LUN)) THEN
            CALL MSGWRT(LUNIT,MBAY(1,LUN),MBYT(LUN))
         ELSE IF(NSUB(LUN).LT.0) THEN
            CALL WRCMPS(-LUNIT)
         ENDIF
      ENDIF
      CALL WTSTAT(LUNIT,LUN,IL,0)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: CLOSMG - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: CLOSMG - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
      END
