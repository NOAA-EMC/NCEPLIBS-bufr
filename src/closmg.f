C> @file
C> @brief Close and write the current message to a BUFR file that was
C> previously opened for writing.
C> <b>Program history log:</b>
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant.
C> 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 (necessary in order to process multiple BUFR files under the MPI).
C> 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes.
C> 2003-05-19 | J. Woollen | Corrected a bug which prevented the dump center and initiation time messages from being written out.
C> 2003-11-04 | J. Ator  | Added documentation.
C> 2003-11-04 | S. Bender | Added remarks and routine interdependencies.
C> 2003-11-04 | D. Keyser | Unified/portable for WRF; added history documentation; outputs more complete diagnostic info when routine terminates abnormally.
C> 2004-08-09 | J. Ator | Maximum message length increased from 20,000 to 50,000 bytes.
C> 2005-05-26 | D. Keyser | Add LUNIN < 0 option to suppress writing of all future zero-subset messsages to ABS(LUNIN).
C> 2014-12-10 | J. Ator | Use modules instead of COMMON blocks.
C> 2022-08-04 | J. Woollen | Added 8-byte wrapper.
C>
C> @author J. Woollen, D. Keyser @date 1994-01-06
      
C> This subroutine closes the BUFR message that is currently open for
C> writing within internal arrays associated with logical unit
C> ABS(LUNIN), and it then writes the message to that logical unit.
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
