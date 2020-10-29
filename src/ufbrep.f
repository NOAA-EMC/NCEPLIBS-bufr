C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR
C>   FROM THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
C>   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF
C>   ABS(LUNIO) (I.E., IF ABS(LUNIO) POINTS TO A BUFR FILE THAT IS OPEN
C>   FOR INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
C>   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET).
C>   THE DATA VALUES CORRESPOND TO MNEMONICS WHICH ARE EITHER:
C>       1) PART OF A REGULAR (I.E., NON-DELAYED) REPLICATION SEQUENCE
C>                OR
C>       2) REPLICATED BY BEING DIRECTLY LISTED MORE THAN ONCE WITHIN AN
C>          OVERALL SUBSET DEFINITION
C>
C>   THE DIFFERENCE IN THE WAY UFBREP WORKS AS COMPARED TO UFBINT IS IN
C>   THE WAY THE MNEMONIC STRING IS INTERPRETED TO DEFINE WHICH ELEMENTS
C>   ARE PROCESSED AND IN WHAT ORDER.  UFBREP INTERPRETS THE FIRST
C>   MNEMONIC IN THE STRING AS A "PIVOT".  THIS MEANS THE 2ND DIMENSION
C>   OF THE DATA RETURNED (AS INDICATED BY ARGUMENT I2) IS DEFINED BY
C>   OCCURRENCES OF THE PIVOT ELEMENT FOUND WITHIN THE OVERALL SUBSET
C>   DEFINITION.  FOR EXAMPLE, IF THE SUBSET DEFINITION CONTAINS THE
C>   FOLLOWING SEQUENCE OF MNEMONICS:
C>   {..,A,..,B,..,C,..,D,..,A,..,C,..,D,..,B,..
C>         A,..,B,..,D,..,C,..,A,..,C,..,B,..,D,..},
C>   THEN READING A SUBSET VIA UFBREP WITH STR = "A B C D" RETURNS THE
C>   FOLLOWING 4X4 MATRIX OF VALUES IN USR, USING A AS THE "PIVOT"
C>   MNEMONIC SINCE IT WAS THE FIRST MNEMONIC IN THE STRING:
C>       ( A1, B1, C1, D1,
C>         A2, B2, C2, D2,
C>         A3, B3, C3, D3,
C>         A4, B4, C4, D4 )
C>   NOTE THAT, WHEN USING UFBREP, THE ORDER OF THE NON-PIVOT MNEMONICS
C>   BETWEEN EACH PIVOT IS IMMATERIAL, I.E., IN THE ABOVE EXAMPLE, UFBREP
C>   FINDS ALL OF THE OCCURRENCES OF MNEMONICS B, C AND D BETWEEN EACH
C>   PIVOT BECAUSE IT SEARCHES INDEPENDENTLY FOR EACH ONE BETWEEN
C>   SUCCESSIVE PIVOTS.
C>
C>   IN CONTRAST, NOTE THERE IS ALSO A SEPARATE SUBROUTINE UFBSTP WHICH
C>   IS SIMILAR TO UFBREP, EXCEPT THAT UFBSTP ALWAYS STEPS FORWARD WHEN
C>   SEARCHING FOR EACH SUCCESSIVE NON-PIVOT MNEMONIC, RATHER THAN
C>   SEARCHING INDEPENDENTLY FOR EACH ONE BETWEEN SUCCESSIVE PIVOTS.
C>   SO IN THE ABOVE EXAMPLE WITH STR="A B C D" AND STARTING FROM EACH
C>   SUCCESSIVE PIVOT MNEMONIC A, UFBSTP WOULD SEARCH FORWARD FOR THE
C>   NEXT OCCURRENCE OF MNEMONIC B, THEN IF FOUND SEARCH FORWARD FROM
C>   THERE FOR THE NEXT OCCURRENCE OF C, THEN IF FOUND SEARCH FORWARD
C>   FROM THERE FOR THE NEXT OCCURRENCE OF D, ETC. UP UNTIL REACHING
C>   THE NEXT OCCURRENCE OF THE PIVOT MNEMONIC A (OR THE END OF THE DATA
C>   SUBSET), WITHOUT EVER DOING ANY BACKTRACKING.  SO IN THE ABOVE
C>   EXAMPLE UFBSTP WOULD RETURN THE FOLLOWING 4x4 MATRIX OF VALUES IN
C>   ARRAY USR, WHERE XX DENOTES A "MISSING" VALUE:
C>       ( A1, B1, C1, D1,
C>         A2, B2, XX, XX,
C>         A3, B3, C3, XX,
C>         A4, B4, XX, XX )
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2003-05-19  J. WOOLLEN -- DISABLED THE PARSING SWITCH WHICH CONTROLS
C>                           CHECKING FOR IN THE SAME REPLICATION GROUP,
C>                           UFBREP DOES NOT NEED THIS CHECK, AND IT
C>                           INTERFERES WITH WHAT UFBREP CAN DO
C>                           OTHERWISE
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C>                           INCREASED FROM 15000 TO 16000 (WAS IN
C>                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C>                           WRF; ADDED DOCUMENTATION (INCLUDING
C>                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
C>                           INFO WHEN ROUTINE TERMINATES ABNORMALLY OR
C>                           UNUSUAL THINGS HAPPEN; CHANGED CALL FROM
C>                           BORT TO BORT2 IN SOME CASES
C> 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST1 AND IFIRST2 FLAGS
C> 2009-03-31  J. WOOLLEN -- ADD DOCUMENTATION
C> 2009-04-21  J. ATOR    -- USE ERRWRT
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL UFBREP (LUNIO, USR, I1, I2, IRET, STR)
C>   INPUT ARGUMENT LIST:
C>     LUNIO    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
C>                FOR BUFR FILE
C>                  - IF BUFR FILE OPEN FOR OUTPUT AND LUNIO IS LESS
C>                    THAN ZERO, UFBREP TREATS THE BUFR FILE AS THOUGH
C>                    IT WERE OPEN FOR INPUT
C>     USR      - ONLY IF BUFR FILE OPEN FOR OUTPUT:
C>                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
C>                   WRITTEN TO DATA SUBSET
C>     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR (MUST BE AT
C>                LEAST AS LARGE AS THE NUMBER OF BLANK-SEPARATED
C>                MNEMONICS IN STR)
C>     I2       - INTEGER:
C>                  - IF BUFR FILE OPEN FOR INPUT:  LENGTH OF SECOND
C>                    DIMENSION OF USR
C>                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
C>                    OF DATA VALUES TO BE WRITTEN TO DATA SUBSET
C>     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B
C>                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST
C>                DIMENSION OF USR
C>                  - IF BUFR FILE OPEN FOR INPUT: THERE ARE THREE
C>                     "GENERIC" MNEMONICS NOT RELATED TO TABLE B,
C>                     THESE RETURN THE FOLLOWING INFORMATION IN
C>                     CORRESPONDING USR LOCATION:
C>                     'NUL'  WHICH ALWAYS RETURNS BMISS ("MISSING")
C>                     'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR
C>                            MESSAGE (RECORD) NUMBER IN WHICH THIS
C>                            SUBSET RESIDES
C>                     'ISUB' WHICH ALWAYS RETURNS THE CURRENT SUBSET
C>                            NUMBER OF THIS SUBSET WITHIN THE BUFR
C>                            MESSAGE (RECORD) NUMBER 'IREC'
C>
C>   OUTPUT ARGUMENT LIST:
C>     USR      - ONLY IF BUFR FILE OPEN FOR INPUT:
C>                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
C>                   READ FROM DATA SUBSET
C>     IRET     - INTEGER:
C>                  - IF BUFR FILE OPEN FOR INPUT: NUMBER OF "LEVELS" OF
C>                    DATA VALUES READ FROM DATA SUBSET (MUST BE NO
C>                    LARGER THAN I2)
C>                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
C>                    OF DATA VALUES WRITTEN TO DATA SUBSET (SHOULD BE
C>                    SAME AS I2)
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     BORT2    ERRWRT   STATUS
C>                               STRING   UFBRP
C>    THIS ROUTINE IS CALLED BY: None
C>                               Normally called only by application
C>                               programs.
C>
      SUBROUTINE UFBREP(LUNIO,USR,I1,I2,IRET,STR)



      USE MODA_USRINT
      USE MODA_MSGCWD

      INCLUDE 'bufrlib.inc'

      COMMON /ACMODE/ IAC
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2,ERRSTR
      REAL*8        USR(I1,I2)

      DATA IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IRET = 0

C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------

      LUNIT = ABS(LUNIO)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IM.EQ.0) GOTO 901
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 902

      IO = MIN(MAX(0,IL),1)
      IF(LUNIO.NE.LUNIT) IO = 0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBREP - 3rd ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ELSEIF(I2.LE.0) THEN
         IF(IPRT.EQ.-1)  IFIRST1 = 1
         IF(IO.EQ.0 .OR. IFIRST1.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBREP - 4th ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
            IF(IPRT.EQ.0 .AND. IO.EQ.1) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING ' //
     .   'message is printed, there may be more.  To output all ' //
     .   'such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add ' //
     .   '"CALL OPENBF(0,''QUIET'',1)" prior to the first call ' //
     .   'to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
            ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            IFIRST1 = 1
         ENDIF
         GOTO 100
      ENDIF

C  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
C  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

C  PARSE OR RECALL THE INPUT STRING - READ/WRITE VALUES
C  ----------------------------------------------------

      IA2 = IAC
      IAC = 1
      CALL STRING(STR,LUN,I1,IO)

C  CALL THE MNEMONIC READER/WRITER
C  -------------------------------

      CALL UFBRP(LUN,USR,I1,I2,IO,IRET)
      IAC = IA2

      IF(IO.EQ.1 .AND. IRET.LT.I2) GOTO 903

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBREP - NO SPECIFIED VALUES READ IN, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            ENDIF
         ELSE
            IF(IPRT.EQ.-1)  IFIRST2 = 1
            IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBREP - NO SPECIFIED VALUES WRITTEN OUT, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('MAY NOT BE IN THE BUFR TABLE(?)')
               IF(IPRT.EQ.0) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING ' //
     .   'message is printed, there may be more.  To output all ' //
     .   'such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add ' //
     .   '"CALL OPENBF(0,''QUIET'',1)" prior to the first call ' //
     .   'to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
               ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
               IFIRST2 = 1
            ENDIF
         ENDIF
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBREP - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   CALL BORT('BUFRLIB: UFBREP - A MESSAGE MUST BE OPEN IN BUFR '//
     . 'FILE, NONE ARE')
902   CALL BORT('BUFRLIB: UFBREP - LOCATION OF INTERNAL TABLE FOR '//
     . 'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL '//
     . 'SUBSET ARRAY')
903   WRITE(BORT_STR1,'("BUFRLIB: UFBREP - MNEMONIC STRING READ IN IS'//
     . ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '//
     . 'WRITTEN (",I3,") LESS THAN THE NUMBER REQUESTED (",I3,") - '//
     . 'INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
