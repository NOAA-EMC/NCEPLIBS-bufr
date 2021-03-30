C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> This subroutine copies a bufr message, intact, from logical
C> unit lunin, opened for input via a previous call to bufr archive
C> library subroutine openbf, to logical unit lunot, opened for output
C> via a previous call to openbf. The message copied from logical
C> unit lunin will be the one most recently read using bufr archive
C> library subroutine readmg. The output file must have no currently
C> open messages. Also, both files must have been opened to the bufr
C> interface with identical bufr tables.
C>
C> PROGRAM HISTORY LOG:
C> - 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> - 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"
C> - 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> - 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           10,000 TO 20,000 BYTES
C> - 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> - 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C>                           INCREASED FROM 15000 TO 16000 (WAS IN
C>                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C>                           WRF; ADDED DOCUMENTATION (INCLUDING
C>                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
C>                           INFO WHEN ROUTINE TERMINATES ABNORMALLY
C> - 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> - 2005-11-29  J. ATOR    -- USE IUPBS01
C> - 2009-06-26  J. ATOR    -- USE IOK2CPY
C> - 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> @param[in] LUNIN Fortran logical unit number for input bufr file.
C> @param[in] LUNOT Fortran logical unit number for output bufr file.
C>
C> REMARKS:
C> - THIS ROUTINE CALLS:
C>      bort() iok2cpy() iupbs01() msgwrt() nemtba() status()
C> - THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
      SUBROUTINE COPYMG(LUNIN,LUNOT)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES

      CHARACTER*8  SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE FILE STATUSES
C  -----------------------

      CALL STATUS(LUNIN,LIN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      CALL STATUS(LUNOT,LOT,IL,IM)
      IF(IL.EQ.0) GOTO 903
      IF(IL.LT.0) GOTO 904
      IF(IM.NE.0) GOTO 905

C  MAKE SURE BOTH FILES HAVE THE SAME TABLES
C  -----------------------------------------

      SUBSET = TAG(INODE(LIN))
c  .... Given SUBSET, returns MTYP,MSBT,INOD
      CALL NEMTBA(LOT,SUBSET,MTYP,MSBT,INOD)
      IF(INODE(LIN).NE.INOD) THEN
        IF(IOK2CPY(LIN,LOT).NE.1) GOTO 906
      ENDIF

C  EVERYTHING OKAY, COPY A MESSAGE
C  -------------------------------

      MBYM = IUPBS01(MBAY(1,LIN),'LENM')
      CALL MSGWRT(LUNOT,MBAY(1,LIN),MBYM)

C  SET THE MESSAGE CONTROL WORDS FOR PARTITION ASSOCIATED WITH LUNOT
C  -----------------------------------------------------------------

      NMSG (LOT) = NMSG(LOT) + 1
      NSUB (LOT) = MSUB(LIN)
      MSUB (LOT) = MSUB(LIN)
      IDATE(LOT) = IDATE(LIN)
      INODE(LOT) = INOD

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: COPYMG - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: COPYMG - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: COPYMG - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
904   CALL BORT('BUFRLIB: COPYMG - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: COPYMG - ALL MESSAGES MUST BE CLOSED IN '//
     . 'OUTPUT BUFR FILE, A MESSAGE IS OPEN')
906   CALL BORT('BUFRLIB: COPYMG - INPUT AND OUTPUT BUFR FILES MUST '//
     . 'HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')
      END
