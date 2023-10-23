C> @file
C> @brief Store or restore parameters associated with a BUFR file.
C>
C> @author Woollen @date 2003-11-04

C> Depending on the value of ISR, either:
C> - store the current parameters associated with a BUFR file
C> connected to LUNIT (read/write pointers, etc.), set the file status
C> to read, then rewind the BUFR file and position it such that the
C> next BUFR message read will be the first message in the file
C> containing actual subsets with data; or
C> - restore the BUFR file connected to LUNIT to the parameters
C> it had prior to the previous call, and using the information that
C> was saved previously
C>
C> This allows information to be extracted from a particular subset in
C> a BUFR file which is in the midst of being read from or written to
C> by an application program.  Note that, for any given BUFR file, a call
C> to this subroutine with ISR = 0 must precede a call to this same
C> subroutine with ISR = 1.  An application program might first
C> call this subroutine with ISR = 0, then call either
C> subroutine rdmgsb() or ufbinx() to get info from a subset, then
C> call this routine again with ISR = 1 to restore the pointers in the
C> BUFR file to their original location.  For example, this subroutine is
C> called internally by subroutine ufbtab() whenever
C> the BUFR file it is acting upon is already open for input or output.
C>
C> @param[in] LUNIT - integer: fortran logical unit number for BUFR file.
C> @param[in] ISR - integer: switch:
C> - 0 store current parameters associated with BUFR file, set file status to read, and rewind
C> file such that next message read is first message containing subset data
C> - 1 restore BUFR file with parameters saved from the previous call to this routine with
C> ISR = 0
C>
C> @author Woollen @date 2003-11-04
      SUBROUTINE REWNBF(LUNIT,ISR)

      use bufrlib

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_BUFRSR

      CHARACTER*128 BORT_STR

      CHARACTER*8   SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  TRY TO TRAP BAD CALL PROBLEMS
C  -----------------------------

      IF(ISR.EQ.0) THEN
         CALL STATUS(LUNIT,LUN,IL,IM)
         IF(JSR(LUN).NE.0)  GOTO 900
         IF(IL.EQ.0) GOTO 901
      ELSEIF(ISR.EQ.1) THEN
         LUN = JUNN
         IF(JSR(JUNN).NE.1)  GOTO 902
      ELSE
         GOTO 903
      ENDIF

C  STORE FILE PARAMETERS AND SET FOR READING
C  -----------------------------------------

      IF(ISR.EQ.0) THEN
         JUNN = LUN
         JILL = IL
         JIMM = IM
         JBIT = IBIT
         JBYT = MBYT(LUN)
         JMSG = NMSG(LUN)
         JSUB = NSUB(LUN)
         KSUB = MSUB(LUN)
         JNOD = INODE(LUN)
         JDAT = IDATE(LUN)
         DO I=1,JBYT
         JBAY(I) = MBAY(I,LUN)
         ENDDO
         CALL WTSTAT(LUNIT,LUN,-1,0)
      ENDIF

C  REWIND THE FILE
C  ---------------

      CALL CEWIND_C(LUN)

C  RESTORE FILE PARAMETERS AND POSITION IT TO WHERE IT WAS SAVED
C  -------------------------------------------------------------

      IF(ISR.EQ.1) THEN
         LUN        = JUNN
         IL         = JILL
         IM         = JIMM
         IBIT       = JBIT
         MBYT(LUN)  = JBYT
         NMSG(LUN)  = JMSG
         NSUB(LUN)  = JSUB
         MSUB(LUN)  = KSUB
         INODE(LUN) = JNOD
         IDATE(LUN) = I4DY(JDAT)
         DO I=1,JBYT
           MBAY(I,LUN) = JBAY(I)
         ENDDO
         DO IMSG=1,JMSG
           CALL READMG(LUNIT,SUBSET,KDATE,IER)
           IF(IER.LT.0) GOTO 905
         ENDDO
         CALL WTSTAT(LUNIT,LUN,IL,IM)
      ENDIF

      JSR(LUN) = MOD(JSR(LUN)+1,2)

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: REWNBF - ATTEMPING TO SAVE '//
     . 'PARAMETERS FOR FILE FOR WHICH THEY HAVE ALREADY BEEN SAVED '//
     . '(AND NOT YET RESTORED) (UNIT",I3,")")') LUNIT
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: REWNBF - ATTEMPING TO SAVE '//
     . 'PARAMETERS FOR BUFR FILE WHICH IS NOT OPENED FOR EITHER INPUT'//
     . ' OR OUTPUT) (UNIT",I3,")")') LUNIT
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: REWNBF - ATTEMPING TO RESTORE '//
     . 'PARAMETERS TO BUFR FILE WHICH WERE NEVER SAVED (UNIT",I3,")")')
     . LUNIT
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: REWNBF - SAVE/RESTORE SWITCH (INPUT '//
     . 'ARGUMENT ISR) IS NOT ZERO OR ONE (HERE =",I4,") (UNIT",I3,")")')
     . ISR,LUNIT
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: REWNBF - HIT END OF FILE BEFORE '//
     . 'REPOSITIONING BUFR FILE IN UNIT",I3," TO ORIGINAL MESSAGE '//
     . 'NO.",I5)') LUNIT,JMSG
      CALL BORT(BORT_STR)
      END
