C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE STORES THE SUBSET TEMPLATE INTO INTERNAL
C>   SUBSET ARRAYS IN MODULES USRINT AND USRBIT.  THIS IS IN
C>   PREPARATION FOR THE ACTUAL UNPACKING OF THE SUBSET IN BUFR ARCHIVE
C>   LIBRARY SUBROUTINE RDTREE.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"
C> 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-
C>                           LINING CODE WITH FPP DIRECTIVES
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           10,000 TO 20,000 BYTES
C> 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C>                           INCREASED FROM 15000 TO 16000 (WAS IN
C>                           VERIFICATION VERSION); MAXRCR (MAXIMUM
C>                           NUMBER OF RECURSION LEVELS) INCREASED FROM
C>                           50 TO 100  (WAS IN VERIFICATION VERSION);
C>                           UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY; COMMENTED OUT
C>                           HARDWIRE OF VTMP TO "BMISS" (10E10) WHEN IT
C>                           IS > 10E9 (CAUSED PROBLEMS ON SOME FOREIGN
C>                           MACHINES)
C> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C> 2016-11-09  J. ATOR    -- ADDED IRET ARGUMENT AND CHECK FOR POSSIBLY
C>                           CORRUPT SUBSETS
C>
C> USAGE:    CALL RCSTPL (LUN,IRET)
C>   INPUT ARGUMENT LIST:
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>
C>   OUTPUT ARGUMENT LIST:
C>     IRET     - INTEGER: RETURN CODE:
C>                       0 = NORMAL RETURN
C>                      -1 = AN ERROR OCCURRED, POSSIBLY DUE TO A
C>                           CORRUPT SUBSET IN THE INPUT MESSAGE
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     IGETRFEL STRBTM   UPBB
C>    THIS ROUTINE IS CALLED BY: RDTREE
C>                               Normally not called by any application
C>                               programs.
C>
      SUBROUTINE RCSTPL(LUN,IRET)



      USE MODA_USRINT
      USE MODA_USRBIT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES
      USE MODA_USRTMP

      INCLUDE 'bufrlib.inc'

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR
      DIMENSION     NBMP(2,MAXRCR),NEWN(2,MAXRCR)
      DIMENSION     KNX(MAXRCR)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  SET THE INITIAL VALUES FOR THE TEMPLATE
C  ---------------------------------------

c  .... Positional index of Table A mnem.
      INV(1,LUN) = INODE(LUN)
      VAL(1,LUN) = 0
      NBMP(1,1) = 1
      NBMP(2,1) = 1
      NODI = INODE(LUN)
      NODE = INODE(LUN)
      MBMP = 1
      KNVN = 1
      NR   = 0

      DO I=1,MAXRCR
      KNX(I) = 0
      ENDDO

C  SET UP THE PARAMETERS FOR A LEVEL OF RECURSION
C  ----------------------------------------------

10    CONTINUE

      NR = NR+1
      IF(NR.GT.MAXRCR) GOTO 900
      NBMP(1,NR) = 1
      NBMP(2,NR) = MBMP

      N1 = ISEQ(NODE,1)
      N2 = ISEQ(NODE,2)
      IF(N1.EQ.0         ) GOTO 901
      IF(N2-N1+1.GT.MAXJL) THEN
        IF(IPRT.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: RCSTPL - MAXJL OVERFLOW; SUBSET SKIPPED')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
        ENDIF
        IRET = -1
        RETURN
      ENDIF
      NEWN(1,NR) = 1
      NEWN(2,NR) = N2-N1+1

      DO N=1,NEWN(2,NR)
      NN = JSEQ(N+N1-1)
      IUTMP(N,NR) = NN
      VUTMP(N,NR) = VALI(NN)
      ENDDO

C  STORE NODES AT SOME RECURSION LEVEL
C  -----------------------------------

20    DO I=NBMP(1,NR),NBMP(2,NR)
      IF(KNX(NR).EQ.0000) KNX(NR) = KNVN
      IF(I.GT.NBMP(1,NR)) NEWN(1,NR) = 1
      DO J=NEWN(1,NR),NEWN(2,NR)
      IF(KNVN+1.GT.MAXSS) THEN
        IF(IPRT.GE.0) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: RCSTPL - MAXSS OVERFLOW; SUBSET SKIPPED')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
        ENDIF
        IRET = -1
        RETURN
      ENDIF
      KNVN = KNVN+1
      NODE = IUTMP(J,NR)
c  .... INV is positional index in internal jump/link table for packed
c       subset element KNVN in MBAY
      INV(KNVN,LUN) = NODE
c  .... MBIT is the bit in MBAY pointing to where the packed subset
c       element KNVN begins
      MBIT(KNVN) = MBIT(KNVN-1)+NBIT(KNVN-1)
c  .... NBIT is the number of bits in MBAY occupied by packed subset
c       element KNVN
      NRFELM(KNVN,LUN) = IGETRFEL(KNVN,LUN)
      NBIT(KNVN) = IBT(NODE)
      IF(TAG(NODE)(1:5).EQ.'DPRI ') THEN
c  .... This is a bitmap entry, so get and store the corresponding value
        CALL UPBB(IDPRI,NBIT(KNVN),MBIT(KNVN),MBAY(1,LUN))
        IF(IDPRI.EQ.0) THEN
          VAL(KNVN,LUN) = 0.0
        ELSE
          VAL(KNVN,LUN) = BMISS
        ENDIF
        CALL STRBTM(KNVN,LUN)
      ENDIF
c  .... Actual unpacked subset values (VAL) are initialized here
c       (numbers as BMISS)
      VAL(KNVN,LUN) = VUTMP(J,NR)
      IF(ITP(NODE).EQ.1) THEN
         CALL UPBB(MBMP,NBIT(KNVN),MBIT(KNVN),MBAY(1,LUN))
         NEWN(1,NR) = J+1
         NBMP(1,NR) = I
         GOTO 10
      ENDIF
      ENDDO
      NEW = KNVN-KNX(NR)
      VAL(KNX(NR)+1,LUN) = VAL(KNX(NR)+1,LUN) + NEW
      KNX(NR) = 0
      ENDDO

C  CONTINUE AT ONE RECURSION LEVEL BACK
C  ------------------------------------

      IF(NR-1.NE.0) THEN
         NR = NR-1
         GOTO 20
      ENDIF

C  FINALLY STORE THE LENGTH OF (NUMBER OF ELEMENTS IN) SUBSET TEMPLATE
C  -------------------------------------------------------------------

      NVAL(LUN) = KNVN

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: RCSTPL - THE NUMBER OF RECURSION '//
     . 'LEVELS EXCEEDS THE LIMIT (",I3,")")') MAXRCR
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: RCSTPL - UNSET EXPANSION SEGMENT ",A)')
     . TAG(NODI)
      CALL BORT(BORT_STR)
      END
