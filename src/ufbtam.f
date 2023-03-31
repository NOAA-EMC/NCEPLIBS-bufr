C> @file
C> @brief Read one or more data values from every data subset in
C> internal arrays
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine reads through every data subset in internal arrays
C> and returns one or more specified data values from each subset.
C>
C> This provides a useful way to scan the ranges of one or more
C> specified data values across all of the data subsets in the
C> internal arrays.  It is similar to subroutine ufbtab(), except
C> that ufbtab() works on data subsets in a BUFR file.
C>
C> It is the user's responsibility to ensure that TAB is dimensioned
C> sufficiently large enough to accommodate the number of data values
C> that are to be read from the internal arrays.  Specifically, each row of
C> TAB will contain the data values read from a different data subset,
C> so the value I2 must be at least as large as the total number of data
C> subsets in the internal arrays.
C>
C> The internal arrays must have already been populated via a previous
C> call to subroutine ufbmem().
C>
C> There are a few additional special mnemonics that can be
C> included within STR when calling this subroutine, and which in turn
C> will result in special information being returned within the
C> corresponding location in TAB:
C> - IREC - returns the number of the BUFR message within the
C>   internal arrays (counting from the beginning of the
C>   internal arrays) in which the current data subset resides.
C> - ISUB - returns the number of the current data subset within
C>   the BUFR message pointed to by IREC, counting from
C>   the beginning of the message.
C> - ITBL - returns the number of the DX BUFR table that is
C>   in scope for the current data subset.
C>
C> This subroutine will not work on compressed data subsets.
C>
C> @param[out] TAB    -- real*8(*,*): Data values.
C> @param[in] I1 -- integer: First dimension of TAB as allocated
C>                  within the calling program.
C> @param[in] I2 -- integer: Second dimension of TAB as allocated
C>                  within the calling program.
C> @param[out] IRET -- integer: Number of data subsets in internal arrays.
C> @param[in] STR -- character*(*): String of blank-separated
C>                   Table B mnemonics, in one-to-one correspondence
C>                   with the number of data values that will be read
C>                   from each data subset within the first dimension of
C>                   TAB (see [DX BUFR Tables](@ref dfbftab) for further
C>                   information about Table B mnemonics).
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBTAM(TAB,I1,I2,IRET,STR)

      USE MODV_BMISS
      USE MODV_IM8B

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_MSGMEM
      USE MODA_TABLES

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),VALS(10),KONS(10)
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*10  TGS(100)
      CHARACTER*8   SUBSET,CVAL
      EQUIVALENCE   (CVAL,RVAL)
      integer*8     mps,ival
      REAL*8        TAB(I1,I2),RVAL,UPS

      DATA MAXTG /100/

C-----------------------------------------------------------------------
      MPS(NODE) = 2_8**(IBT(NODE))-1
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(I1,MY_I1,1)
         CALL X84(I2,MY_I2,1)
         CALL UFBTAM(TAB,MY_I1,MY_I2,IRET,STR)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = 0

      IF(MSGP(0).EQ.0) GOTO 100

      DO J=1,I2
      DO I=1,I1
      TAB(I,J) = BMISS
      ENDDO
      ENDDO

C  CHECK FOR SPECIAL TAGS IN STRING
C  --------------------------------

      CALL PARSTR(STR,TGS,MAXTG,NTG,' ',.TRUE.)
      IREC = 0
      ISUB = 0
      ITBL = 0
      DO I=1,NTG
      IF(TGS(I).EQ.'IREC') IREC = I
      IF(TGS(I).EQ.'ISUB') ISUB = I
      IF(TGS(I).EQ.'ITBL') ITBL = I
      ENDDO

C  READ A MESSAGE AND PARSE A STRING
C  ---------------------------------

      CALL STATUS(MUNIT,LUN,IL,IM)

      DO IMSG=1,MSGP(0)
      CALL RDMEMM(IMSG,SUBSET,JDATE,MRET)
      IF(MRET.LT.0) GOTO 900

      CALL STRING(STR,LUN,I1,0)
      IF(IREC.GT.0) NODS(IREC) = 0
      IF(ISUB.GT.0) NODS(ISUB) = 0
      IF(ITBL.GT.0) NODS(ITBL) = 0

C  PROCESS ALL THE SUBSETS IN THE MEMORY MESSAGE
C  ---------------------------------------------

      DO WHILE (NSUB(LUN).LT.MSUB(LUN))
         IF(IRET+1.GT.I2) GOTO 99
         IRET = IRET+1

         DO I=1,NNOD
         NODS(I) = ABS(NODS(I))
         ENDDO

         CALL USRTPL(LUN,1,1)
         MBIT = MBYT(LUN)*8+16
         NBIT = 0
         N = 1

20       IF(N+1.LE.NVAL(LUN)) THEN
            N = N+1
            NODE = INV(N,LUN)
            MBIT = MBIT+NBIT
            NBIT = IBT(NODE)
            IF(ITP(NODE).EQ.1) THEN
               CALL UPB8(IVAL,NBIT,MBIT,MBAY(1,LUN))
               NBMP=INT(IVAL); CALL USRTPL(LUN,N,NBMP)
            ENDIF
            DO I=1,NNOD
            IF(NODS(I).EQ.NODE) THEN
               IF(ITP(NODE).EQ.1) THEN
                  CALL UPB8(IVAL,NBIT,MBIT,MBAY(1,LUN))
                  TAB(I,IRET) = IVAL
               ELSEIF(ITP(NODE).EQ.2) THEN
                  CALL UPB8(IVAL,NBIT,MBIT,MBAY(1,LUN))
                  IF(IVAL.LT.MPS(NODE)) TAB(I,IRET) = UPS(IVAL,NODE)
               ELSEIF(ITP(NODE).EQ.3) THEN
                  CVAL = ' '
                  KBIT = MBIT
                  CALL UPC(CVAL,NBIT/8,MBAY(1,LUN),KBIT,.TRUE.)
                  TAB(I,IRET) = RVAL
               ENDIF
               NODS(I) = -NODS(I)
               GOTO 20
            ENDIF
            ENDDO
            DO I=1,NNOD
            IF(NODS(I).GT.0) GOTO 20
            ENDDO
         ENDIF

C  UPDATE THE SUBSET POINTERS BEFORE NEXT READ
C  -------------------------------------------

         IBIT = MBYT(LUN)*8
         CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
         MBYT(LUN) = MBYT(LUN) + NBYT
         NSUB(LUN) = NSUB(LUN) + 1
         IF(IREC.GT.0) TAB(IREC,IRET) = NMSG(LUN)
         IF(ISUB.GT.0) TAB(ISUB,IRET) = NSUB(LUN)
         IF(ITBL.GT.0) TAB(ITBL,IRET) = LDXTS
      ENDDO

      ENDDO

      GOTO 200

C  EMERGENCY ROOM TREATMENT FOR ARRAY OVERFLOW
C  -------------------------------------------

99    CALL RDMEMM(0,SUBSET,JDATE,MRET)
      NREP = 0
      DO IMSG=1,MSGP(0)
      CALL RDMEMM(IMSG,SUBSET,JDATE,MRET)
      IF(MRET.LT.0) GOTO 900
      NREP = NREP+NMSUB(MUNIT)
      ENDDO
      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I8,A,A)' )
     . 'BUFRLIB: UFBTAM - THE NO. OF DATA SUBSETS IN MEMORY ',
     . 'IS .GT. LIMIT OF ', I2, ' IN THE 3RD ARG. (INPUT) - ',
     . 'INCOMPLETE READ'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBTAM STORED ', IRET, ' REPORTS OUT OF ', NREP, '<<<'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  RESET THE MEMORY FILE
C  ---------------------

200   CALL RDMEMM(0,SUBSET,JDATE,MRET)

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: UFBTAM - HIT END-OF-FILE READING '//
     . 'MESSAGE NUMBER",I5," IN INTERNAL MEMORY")') IMSG
      CALL BORT(BORT_STR)
      END
