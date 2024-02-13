C> @file
C> @brief Read one or more data values from a data subset.
C>
C> @author Woollen @date 1994-01-06

C> Unpacks and return the values for one-dimensional
C> descriptors in the input string without advancing the subset pointer.
C>
C> @param[in] LUNIT - integer: fortran logical unit number for BUFR file.
C> @param[out] TAB - real*8(*): data values.
C> @param[in] I1 - integer: size of TAB as allocated within the calling program.
C> @param[out] IRET - integer: return code:
C> - 0 normal return.
C> - -1 there are no more subsets in the BUFR message.
C> @param[in] STR - character*(*): string of blank-separated Table B
C> mnemonics in one-to-one correspondence with the number of data values
C> that will be read from the data subset into TAB.
C>
C> @author Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBGET(LUNIT,TAB,I1,IRET,STR)

      use modv_vars, only: im8b, bmiss

      use moda_usrint
      use moda_usrbit
      use moda_msgcwd
      use moda_bitbuf
      use moda_tables

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
         NBMP=INT(IVAL)
         CALL USRTPL(LUN,N,NBMP)
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
