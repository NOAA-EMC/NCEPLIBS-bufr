C> @file
C> @brief Unpack and return the values for one-
C> dimensional descriptors in the input string without advancing the
C> subset pointer.
C>
C> ### Program History Log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 1994-01-06 | J. Woollen | original author
C> 1998-07-08 | J. Woollen | replaced call to cray library routine "abort" with call to new internal bufrlib routine "bort"; improved machine portability
C> 1998-10-27 | J. Woollen | modified to correct problems caused by in- lining code with fpp directives
C> 1999-11-18 | J. Woollen | the number of bufr files which can be opened at one time increased from 10 to 32 
C> 2000-09-19 | J. Woollen | maximum message length increased from 10,000 to 20,000 bytes
C> 2002-05-14 | J. Woollen | removed old cray compiler directives
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation; outputs more info
C> 2004-08-09 | J. Ator    | maximum message length increased from 20,000 to 50,000 bytes
C> 2012-03-02 | J. Ator    | use function ups
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C> 2022-05-06 | j. Woollen | replace upbb with upb8 for 8byte integers
C> 2022-10-04 | J. Ator    | added 8-byte wrapper
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine unpacks and returns the values for one-
C> dimensional descriptors in the input string without advancing the
C> subset pointer.
C>
C> @param[in] LUNIT - integer: fortran logical unit number for bufr file.
C> @param[out] TAB - real*8: (i1) starting address of data values read from data subset.
C> @param[in] I1 - integer: length of tab.
C> @param[out] IRET - integer: return code:.
C> - 0 normal return
C> - -1 there are no more subsets in the BUFR message
C> @param[in] STR - character*(*): string of blank-separated table B
C> mnemonics in one-to-one correspondence with the words
C> in the array tab. There are three "generic" mnemonics not related
C> to table B, these return the following
C> information in corresponding tab location:
C> - 'NUL'  which always returns bmiss ("MISSING")
C> - 'IREC' which always returns the current bufr message (record) number
C>    in which this subset resides.
C> - 'ISUB' which always returns the current subset number of this subset within the bufr
C>   message (record) number 'IREC'.
C>
C> @author Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBGET(LUNIT,TAB,I1,IRET,STR)

      USE MODV_BMISS
      USE MODV_IM8B

      USE MODA_USRINT
      USE MODA_USRBIT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)

      CHARACTER*(*) STR
      CHARACTER*8   CVAL
      EQUIVALENCE   (CVAL,RVAL)
      INTEGER*8     IVAL
      REAL*8        RVAL,TAB(I1),UPS

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(I1,MY_I1,1)
         CALL UFBGET(MY_LUNIT,TAB,MY_I1,IRET,STR)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = 0

      DO I=1,I1
      TAB(I) = BMISS
      ENDDO

C  MAKE SURE A FILE/MESSAGE IS OPEN FOR INPUT
C  ------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE
C  ---------------------------------------------

      IF(NSUB(LUN).EQ.MSUB(LUN)) THEN
         IRET = -1
         GOTO 100
      ENDIF

C  PARSE THE STRING
C  ----------------

      CALL STRING(STR,LUN,I1,0)

C  EXPAND THE TEMPLATE FOR THIS SUBSET AS LITTLE AS POSSIBLE
C  ---------------------------------------------------------

      N = 1
      NBIT(N) = 0
      MBIT(N) = MBYT(LUN)*8 + 16
      CALL USRTPL(LUN,N,N)

10    DO N=N+1,NVAL(LUN)
      NODE = INV(N,LUN)
      NBIT(N) = IBT(NODE)
      MBIT(N) = MBIT(N-1)+NBIT(N-1)
      IF(NODE.EQ.NODS(NNOD)) THEN
         NVAL(LUN) = N
         GOTO 20
      ELSEIF(ITP(NODE).EQ.1) THEN
         CALL UPB8(IVAL,NBIT(N),MBIT(N),MBAY(1,LUN))
         NBMP=IVAL; CALL USRTPL(LUN,N,NBMP)
         GOTO 10
      ENDIF
      ENDDO
20    CONTINUE

C  UNPACK ONLY THE NODES FOUND IN THE STRING
C  -----------------------------------------

      DO I=1,NNOD
      NODE = NODS(I)
      INVN = INVWIN(NODE,LUN,1,NVAL(LUN))
      IF(INVN.GT.0) THEN
         CALL UPB8(IVAL,NBIT(INVN),MBIT(INVN),MBAY(1,LUN))
         IF(ITP(NODE).EQ.1) THEN
            TAB(I) = IVAL
         ELSEIF(ITP(NODE).EQ.2) THEN
            IF(IVAL.LT.2_8**(IBT(NODE))-1) TAB(I) = UPS(IVAL,NODE)
         ELSEIF(ITP(NODE).EQ.3) THEN
            CVAL = ' '
            KBIT = MBIT(INVN)
            CALL UPC(CVAL,NBIT(INVN)/8,MBAY(1,LUN),KBIT,.TRUE.)
            TAB(I) = RVAL
         ENDIF
      ELSE
         TAB(I) = BMISS
      ENDIF
      ENDDO

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBGET - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: UFBGET - INPUT BUFR FILE IS OPEN FOR OUTPUT'//
     . ', IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: UFBGET - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
      END
