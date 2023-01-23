C> @file
C> @brief Store or restore parameters associated with BUFR file.
C> 
C> ### Program History Log
C> Date | Programmer | Comments 
C> -----|------------|----------
C> 2003-11-04 | J. Woollen | original author (was in verification version but may have been in the production version at one time and then removed)
C> 2003-11-04 | D. Keyser  | unified/portable for wrf; added documentation; outputs more complete diagnostic info when routine terminates abnormally
C> 2004-08-09 | J. Ator    | maximum message length increased from 20,000 to 50,000 bytes
C> 2009-03-23 | J. Ator    | modified to handle embedded bufr table (dictionary) messages
C> 2011-09-26 | J. Woollen | fixed bug to prevent skip of first data message after rewind
C> 2012-09-15 | J. Woollen | modified for c/i/o/bufr interface; replace fortran rewind with c cewind
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C>
C> @author Woollen @date 2003-11-04
      
C> This subroutine, depending on the value of isr, will
c> either:
C> 1. Store the current parameters associated with a bufr file
c> connected to lunit (read/write pointers, etc.), set the file status
c> to read, then rewind the bufr file and position it such that the
c> next bufr message read will be the first message in the file
c> containing actual subsets with data; or
C> 2. restore the bufr file connected to lunit to the parameters
c> it had prior to 1) above using the information saved in 1) above.
C>
C> This allows information to be extracted from a particular subset in
c> a bufr file which is in the midst of being read from or written to
c> by an application program. Note that for a particular bufr file 1)
c> above must precede 2) above. An application program might first
c> call this subroutine with isr = 0, then call either bufr archive
c> library subroutine rdmgsb or ufbinx to get info from a subset, then
c> call this routine again with isr = 1 to restore the pointers in the
c> bufr file to their original location. Also, bufr archive library
c> subroutine ufbtab will call this routine if the bufr file it is
c> acting upon is already open for input or output.
C>
C> @param[in] LUNIT - integer: fortran logical unit number for bufr file.
C> @param[in] ISR - integer: switch:.
C> - 0 store current parameters associated with BUFR file, set file status to read, rewind
C> file such that next message read is first message containing subset data
C> - 1 restore BUFR file with parameters saved from the previous call to this routine with
C> ISR=0
C>
C> @author Woollen @date 2003-11-04
      SUBROUTINE REWNBF(LUNIT,ISR)

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

      CALL CEWIND(LUN)

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
