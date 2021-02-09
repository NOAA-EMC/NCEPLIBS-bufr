C> @file
C> @brief Close and write the current message to a BUFR file that was
C> previously opened for writing.
      
C> This subroutine closes the BUFR message that is currently open for
C> writing within internal arrays associated with logical unit
C> ABS(LUNIN), and it then writes the message to that logical unit.
C>
C> @authors J. Woollen
C> @authors D. Keyser
C> @date 1994-01-06
C>
C> @param[in] LUNIN  - integer: Absolute value is Fortran logical unit
C>                     number for BUFR file
C>
C> <p>Logical unit ABS(LUNIN) should have already been opened for output
C> operations via a previous call to subroutine openbf().
C>
C> <p>If LUNIN < 0, then any message containing zero data subsets will
C> not be written to logical unit ABS(LUNIN) for the remainder of the
C> life of the application program.  This includes suppressing the
C> writing of any dummy messages containing dump center and initiation
C> times that normally appear in the first 2 messages of NCEP dump files.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine
C>                           "ABORT" with call to new internal BUFRLIB
C>                           routine "BORT"; modified to make Y2K
C>                           compliant
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                           opened at one time increased from 10 to 32
C>                           (necessary in order to process multiple
C>                           BUFR files under the MPI)
C> - 2000-09-19  J. Woollen -- Maximum message length increased from
C>                           10,000 to 20,000 bytes
C> - 2003-05-19  J. Woollen -- Corrected a bug which prevented the dump
C>                           center and initiatiion time messages from
C>                           being written out
C> - 2003-11-04  J. Ator    -- Added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                           documentation; outputs more complete
C>                           diagnostic info when routine terminates
C>                           abnormally
C> - 2004-08-09  J. Ator -- Maximum message length increased from
C>                           20,000 to 50,000 bytes
C> - 2005-05-26  D. Keyser -- Add LUNIN < 0 option to suppress writing
C>                          of all future zero-subset messsages to
C>                          ABS(LUNIN)
C> - 2014-12-10  J. Ator  -- Use modules instead of COMMON blocks
C>
C> <b>This routine calls:</b> bort()   msgwrt()   status()   wrcmps()
C>                            wtstat()
C>
C> <b>This routine is called by:</b> closbf()   makestab() openmb()
C>                               openmg() writsa()
C>                           <br>Also called by application programs.
C>
      SUBROUTINE CLOSMG(LUNIN)

      USE MODA_MSGCWD
      USE MODA_MSGLIM
      USE MODA_BITBUF

      INCLUDE 'bufrlib.inc'

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

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
