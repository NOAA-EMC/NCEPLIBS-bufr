C> @file
C> @brief Initate the process to parse out mnemonics from a character string.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced call to cray "abort" with bort(); improved machine portability.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | Unified/portable for wrf; documentation; more diagnostic info; changed to bort2(); change parutg().
C> 2007-01-19 | J. Ator    | Replaced call to parseq with call to parstr.
C> 2009-05-07 | J. Ator    | Use lstjpb instead of lstrpc.
C>
C> @author Woollen @date 1994-01-06

C> This subroutine initates the process to parse out mnemonics
C> (nodes) from a user-specified character string, and separates them
C> into store and condition nodes. Information about the string
C> "pieces" (i.e., the mnemonics) is stored in arrays in common block
C> /usrstr/. Condition nodes are sorted in the order expected in the
C> internal jump/link tables and several checks are performed on the
C> nodes.
C>
C> @param[in] STR - character*(*): string of blank-separated mnemonics.
C> @param[in] LUN - integer: I/O stream index into internal memory arrays.
C> @param[in] I1 - integer: a number greater than or equal to the number of
C> blank-separated mnemonics in STR.
C> @param[in] IO - integer: status indicator for BUFR file associated with LUN:
C> - 0 input file
C> - 1 output file
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE PARUSR(STR,LUN,I1,IO)

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
      COMMON /ACMODE/ IAC

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*80  UST
      CHARACTER*20  UTG(30)
      LOGICAL       BUMP

      DATA MAXUSR /30/
      DATA MAXNOD /20/
      DATA MAXCON /10/

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      UST  = STR
      IF(LEN(STR).GT.80) GOTO 900

      NCON = 0
      NNOD = 0

C  PARSE OUT STRING PIECES(S) (UTG's or MNEMONICS)
C  -----------------------------------------------

      CALL PARSTR(UST,UTG,MAXUSR,NTOT,' ',.TRUE.)

      DO N=1,NTOT

C  DETERMINE IF THIS UTG IS A CONDITION NODE OR A STORE NODE
C  ---------------------------------------------------------

      CALL PARUTG(LUN,IO,UTG(N),NOD,KON,VAL)
      IF(KON.NE.0) THEN
c  .... it is a condition node
         NCON = NCON+1
         IF(NCON.GT.MAXCON) GOTO 901
         NODC(NCON) = NOD
         KONS(NCON) = KON
         IVLS(NCON) = NINT(VAL)
      ELSE
c  .... it is a store node
         NNOD = NNOD+1
         IF(NNOD.GT.MAXNOD) GOTO 902
         NODS(NNOD) = NOD
      ENDIF
      ENDDO

C  SORT CONDITION NODES IN JUMP/LINK TABLE ORDER
C  ---------------------------------------------

      DO I=1,NCON
      DO J=I+1,NCON
      IF(NODC(I).GT.NODC(J)) THEN
         NOD     = NODC(I)
         NODC(I) = NODC(J)
         NODC(J) = NOD

         KON     = KONS(I)
         KONS(I) = KONS(J)
         KONS(J) = KON

         VAL     = IVLS(I)
         IVLS(I) = IVLS(J)
         IVLS(J) = VAL
      ENDIF
      ENDDO
      ENDDO

C  CHECK ON SPECIAL RULES FOR CONDITIONAL NODES THAT ARE BUMP NODES
C  ----------------------------------------------------------------

      BUMP = .FALSE.

      DO N=1,NCON
      IF(KONS(N).EQ.5) THEN
         IF(IO.EQ.0)   GOTO 903
         IF(N.NE.NCON) GOTO 904
         BUMP = .TRUE.
      ENDIF
      ENDDO

C  CHECK STORE NODE COUNT AND ALIGNMENT
C  ------------------------------------

      IF(.NOT.BUMP .AND. NNOD.EQ.0) GOTO 905
      IF(NNOD.GT.I1)                GOTO 906

      IRPC = -1
      DO I=1,NNOD
      IF(NODS(I).GT.0) THEN
         IF(IRPC.LT.0) IRPC = LSTJPB(NODS(I),LUN,'RPC')
         IF(IRPC.NE.LSTJPB(NODS(I),LUN,'RPC').AND.IAC.EQ.0) GOTO 907
      ENDIF
      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: PARUSR - INPUT STRING (",A,") HAS ")')
     . STR
      WRITE(BORT_STR2,'(18X,"LENGTH (",I4,"), > LIMIT OF 80 CHAR.")')
     . LEN(STR)
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: PARUSR - THE NUMBER OF CONDITION '//
     . 'NODES IN INPUT STRING")')
      WRITE(BORT_STR2,'(18X,A,") EXCEEDS THE MAXIMUM (",I3,")")')
     . STR,MAXCON
      CALL BORT2(BORT_STR1,BORT_STR2)
902   WRITE(BORT_STR1,'("BUFRLIB: PARUSR - THE NUMBER OF STORE NODES '//
     . 'IN INPUT STRING")')
       WRITE(BORT_STR2,'(18X,A,") EXCEEDS THE MAXIMUM (",I3,")")')
     . STR,MAXNOD
      CALL BORT2(BORT_STR1,BORT_STR2)
903   WRITE(BORT_STR1,'("BUFRLIB: PARUSR - BUMP NODE (^ IN INPUT '//
     . 'STRING ",A)') STR
      WRITE(BORT_STR2,'(18X,"IS SPECIFIED FOR A BUFR FILE OPEN FOR '//
     . 'INPUT, THE BUFR FILE MUST BE OPEN FOR OUTPUT")')
      CALL BORT2(BORT_STR1,BORT_STR2)
904   WRITE(BORT_STR1,'("BUFRLIB: PARUSR - INPUT STRING (",A,") '//
     . 'CONTAINS")') STR
      WRITE(BORT_STR2,'(18X,"CONDITIONAL NODES IN ADDITION TO BUMP '//
     . 'NODE - THE BUMP MUST BE ON THE INNER NODE")')
      CALL BORT2(BORT_STR1,BORT_STR2)
905   WRITE(BORT_STR1,'("BUFRLIB: PARUSR - INPUT STRING (",A,") HAS")')
     . STR
      WRITE(BORT_STR2,'(18X,"NO STORE NODES")')
      CALL BORT2(BORT_STR1,BORT_STR2)
906   WRITE(BORT_STR1,'("BUFRLIB: PARUSR - INPUT STRING (",A,")")') STR
      WRITE(BORT_STR2,'(18X,"HAS",I5," STORE NODES (MNEMONICS) - THE '//
     . 'LIMIT {THIRD (INPUT) ARGUMENT} IS",I5)') NNOD,I1
      CALL BORT2(BORT_STR1,BORT_STR2)
907   WRITE(BORT_STR1,'("BUFRLIB: PARUSR - INPUT STRING (",A,") '//
     . 'CONTAINS")') STR
      WRITE(BORT_STR2,'(18X,"STORE NODES (MNEMONICS) THAT ARE IN MORE'//
     . ' THAN ONE REPLICATION GROUP")')
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
