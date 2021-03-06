C> @file
C> @author WOOLLEN @date 2003-11-04
      
C> THIS SUBROUTINE, DEPENDING ON THE VALUE OF ISR, WILL
C>   EITHER:
C>        1) STORE THE CURRENT PARAMETERS ASSOCIATED WITH A BUFR FILE
C>   CONNECTED TO LUNIT (READ/WRITE POINTERS, ETC.), SET THE FILE STATUS
C>   TO READ, THEN REWIND THE BUFR FILE AND POSITION IT SUCH THAT THE
C>   NEXT BUFR MESSAGE READ WILL BE THE FIRST MESSAGE IN THE FILE
C>   CONTAINING ACTUAL SUBSETS WITH DATA; OR
C>        2) RESTORE THE BUFR FILE CONNECTED TO LUNIT TO THE PARAMETERS
C>   IT HAD PRIOR TO 1) ABOVE USING THE INFORMATION SAVED IN 1) ABOVE.
C>
C>   THIS ALLOWS INFORMATION TO BE EXTRACTED FROM A PARTICULAR SUBSET IN
C>   A BUFR FILE WHICH IS IN THE MIDST OF BEING READ FROM OR WRITTEN TO
C>   BY AN APPLICATION PROGRAM.  NOTE THAT FOR A PARTICULAR BUFR FILE 1)
C>   ABOVE MUST PRECEDE 2) ABOVE.  AN APPLICATION PROGRAM MIGHT FIRST
C>   CALL THIS SUBROUTINE WITH ISR = 0, THEN CALL EITHER BUFR ARCHIVE
C>   LIBRARY SUBROUTINE RDMGSB OR UFBINX TO GET INFO FROM A SUBSET, THEN
C>   CALL THIS ROUTINE AGAIN WITH ISR = 1 TO RESTORE THE POINTERS IN THE
C>   BUFR FILE TO THEIR ORIGINAL LOCATION.  ALSO, BUFR ARCHIVE LIBRARY
C>   SUBROUTINE UFBTAB WILL CALL THIS ROUTINE IF THE BUFR FILE IT IS
C>   ACTING UPON IS ALREADY OPEN FOR INPUT OR OUTPUT.
C>
C> PROGRAM HISTORY LOG:
C> 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR (WAS IN VERIFICATION
C>                           VERSION BUT MAY HAVE BEEN IN THE PRODUCTION
C>                           VERSION AT ONE TIME AND THEN REMOVED)
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C>                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C>                           ABNORMALLY
C> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> 2009-03-23  J. ATOR    -- MODIFIED TO HANDLE EMBEDDED BUFR TABLE
C>                           (DICTIONARY) MESSAGES
C> 2011-09-26  J. WOOLLEN -- FIXED BUG TO PREVENT SKIP OF FIRST DATA
C>                           MESSAGE AFTER REWIND
C> 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE;
C>                           REPLACE FORTRAN REWIND WITH C CEWIND
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL REWNBF (LUNIT, ISR)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>     ISR      - INTEGER: SWITCH:
C>                       0 = store current parameters associated with
C>                           BUFR file, set file status to read, rewind
C>                           file such that next message read is first
C>                           message containing subset data
C>                       1 = restore BUFR file with parameters saved
C>                           from the previous call to this routine with
C>                           ISR=0
C>
C>   INPUT FILES:
C>     UNIT "LUNIT" - BUFR FILE
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     I4DY     READMG   STATUS
C>                               WTSTAT   CEWIND
C>    THIS ROUTINE IS CALLED BY: UFBINX   UFBTAB
C>                               Also called by application programs.
C>
      SUBROUTINE REWNBF(LUNIT,ISR)



      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_BUFRSR

      INCLUDE 'bufrlib.inc'

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
