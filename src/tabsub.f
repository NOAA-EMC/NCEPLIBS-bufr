C> @file
C> @brief Store an entry for a Table A mnemonic within the internal jump/link table.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1994-01-06 | J. Woollen | original author
C> 1998-07-08 | J. Woollen | replaced call to cray library routine "abort" with call to new internal bufrlib routine "bort"
C> 2000-09-19 | J. Woollen | added capability to encode and decode data using the operator descriptors (bufr table c) for changing width and changing scale
C> 2003-11-04 | J. Ator    | added documentation
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation; outputs more diagnostic info.
C> 2005-11-29 | J. Ator    | added support for 207 and 208 operators
C> 2012-03-02 | J. Ator    | added support for 203 operator
C> 2012-04-19 | J. Ator    | fixed bug for cases where a table c operator immediately follows a table d sequence
C> 2014-12-10 | J. Ator    | use modules instead of common blocks
C> 2016-05-24 | J. Ator    | store table c operators in module bitmaps
C> 2017-04-03 | J. Ator    | add a dimension to all tco arrays so that each subset definition in the jump/link table has its own set of table c operators
C>
C> @author Woollen @date 1994-01-06

C> This subroutine builds the entire jump/link tree
C> (including recursively resolving all "child" mnemonics) for a Table A
C> mnemonic within the internal jump/link table.
C>
C> @param[in] LUN - integer: I/O stream index into internal memory arrays.
C> @param[in] NEMO - character*8: Table A mnemonic.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE TABSUB(LUN,NEMO)

      USE MODV_MXTAMC
      USE MODV_MXTCO

      USE MODA_TABLES
      USE MODA_NMIKRP
      USE MODA_NRV203
      USE MODA_BITMAPS

      COMMON /TABCCC/ ICDW,ICSC,ICRV,INCW

      CHARACTER*128 BORT_STR
      CHARACTER*8   NEMO,NEMS
      CHARACTER*1   TAB
      DIMENSION     DROP(10),JMP0(10),NODL(10),NTAG(10,2)
      LOGICAL       DROP,LTAMC

      DATA MAXLIM /10/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE MNEMONIC
C  ------------------

C     Note that Table A mnemonics, in addition to being stored within
C     internal BUFR Table A array TABA(*,LUN), are also stored as
C     Table D mnemonics within internal BUFR Table D array TABD(*,LUN).
C     Thus, the following test is valid.

      CALL NEMTAB(LUN,NEMO,IDN,TAB,ITAB)
      IF(TAB.NE.'D') GOTO 900

C  STORE A SUBSET NODE AND JUMP/LINK THE TREE
C  ------------------------------------------

      CALL INCTAB(NEMO,'SUB',NODE)
      JUMP(NODE) = NODE+1
      JMPB(NODE) = 0
      LINK(NODE) = 0
      IBT (NODE) = 0
      IRF (NODE) = 0
      ISC (NODE) = 0

      CALL NEMTBD(LUN,ITAB,NSEQ,NEM(1,1),IRP(1,1),KRP(1,1))
      NTAG(1,1) = 1
      NTAG(1,2) = NSEQ
      JMP0(1)   = NODE
      NODL(1)   = NODE
      LIMB      = 1

      ICDW = 0
      ICSC = 0
      ICRV = 1
      INCW = 0

      IBTNRV = 0
      IPFNRV = 0

      IF(NTAMC+1.GT.MXTAMC) GOTO 913
      INODTAMC(NTAMC+1) = NODE
      NTCO(NTAMC+1) = 0
      LTAMC = .FALSE.

C  THIS LOOP RESOLVES ENTITIES IN A SUBSET BY EMULATING RECURSION
C  --------------------------------------------------------------

1     DO N=NTAG(LIMB,1),NTAG(LIMB,2)

      NTAG(LIMB,1) = N+1
      DROP(LIMB)   = N.EQ.NTAG(LIMB,2)

      CALL NEMTAB(LUN,NEM(N,LIMB),IDN,TAB,ITAB)
      NEMS = NEM(N,LIMB)

C  SPECIAL TREATMENT FOR CERTAIN OPERATOR DESCRIPTORS (TAB=C)
C  ----------------------------------------------------------

      IF(TAB.EQ.'C') THEN
         READ(NEMS,'(3X,I3)') IYYY
         IF(ITAB.EQ.1) THEN
            IF(IYYY.NE.0) THEN
              IF(ICDW.NE.0) GOTO 907
              ICDW = IYYY-128
            ELSE
              ICDW = 0
            ENDIF
         ELSEIF(ITAB.EQ.2) THEN
            IF(IYYY.NE.0) THEN
              IF(ICSC.NE.0) GOTO 908
              ICSC = IYYY-128
            ELSE
              ICSC = 0
            ENDIF
         ELSEIF(ITAB.EQ.3) THEN
            IF(IYYY.EQ.0) THEN

C             Stop applying new reference values to subset nodes.
C             Instead, revert to the use of standard Table B values.

              IF(IPFNRV.EQ.0) GOTO 911
              DO JJ=IPFNRV,NNRV
                IENRV(JJ) = NTAB
              ENDDO
              IPFNRV = 0
            ELSEIF(IYYY.EQ.255) THEN

C             End the definition of new reference values.

              IBTNRV = 0
            ELSE

C             Begin the definition of new reference values.

              IF(IBTNRV.NE.0) GOTO 909
              IBTNRV = IYYY
            ENDIF
         ELSEIF(ITAB.EQ.7) THEN
            IF(IYYY.GT.0) THEN
              IF(ICDW.NE.0) GOTO 907
              IF(ICSC.NE.0) GOTO 908
              ICDW = ((10*IYYY)+2)/3
              ICSC = IYYY
              ICRV = 10**IYYY
            ELSE
              ICSC = 0
              ICDW = 0
              ICRV = 1
            ENDIF
         ELSEIF(ITAB.EQ.8) THEN
            INCW = IYYY
         ELSEIF((ITAB.GE.21).AND.(IOKOPER(NEMS).EQ.1)) THEN

C           Save the location of this operator within the
C           jump/link table, for possible later use.

            IF(.NOT.LTAMC) THEN
              LTAMC = .TRUE.
              NTAMC = NTAMC+1
            END IF
            IF(NTCO(NTAMC)+1.GT.MXTCO) GOTO 912
            NTCO(NTAMC) = NTCO(NTAMC)+1
            CTCO(NTAMC,NTCO(NTAMC)) = NEMS(1:6)
            INODTCO(NTAMC,NTCO(NTAMC)) = NTAB
         ENDIF
      ELSE
         NODL(LIMB) = NTAB+1
         IREP = IRP(N,LIMB)
         IKNT = KRP(N,LIMB)
         JUM0 = JMP0(LIMB)
         CALL TABENT(LUN,NEMS,TAB,ITAB,IREP,IKNT,JUM0)
      ENDIF

      IF(TAB.EQ.'D') THEN

C        Note here how a new tree "LIMB" is created (and is then
C        immediately recursively resolved) whenever a Table D mnemonic
C        contains another Table D mnemonic as one of its children.

         LIMB = LIMB+1
         IF(LIMB.GT.MAXLIM) GOTO 901
         CALL NEMTBD(LUN,ITAB,NSEQ,NEM(1,LIMB),IRP(1,LIMB),KRP(1,LIMB))
         NTAG(LIMB,1) = 1
         NTAG(LIMB,2) = NSEQ
         JMP0(LIMB)   = NTAB
         GOTO 1
      ELSEIF(DROP(LIMB)) THEN
2        LINK(NODL(LIMB)) = 0
         LIMB = LIMB-1
         IF(LIMB.EQ.0 ) THEN
            IF(ICRV.NE.1) GOTO 904
            IF(ICDW.NE.0) GOTO 902
            IF(ICSC.NE.0) GOTO 903
            IF(INCW.NE.0) GOTO 905
            IF(IBTNRV.NE.0) GOTO 910
            IF(IPFNRV.NE.0) THEN

C             One or more new reference values were defined for this
C             subset, but there was no subsequent 2-03-000 operator,
C             so set all IENRV(*) values for this subset to point to
C             the last element of the subset within the jump/link table.
C             Note that, if there had been a subsequent 2-03-000
C             operator, then these IENRV(*) values would have already
C             been properly set above.

              DO JJ=IPFNRV,NNRV
                IENRV(JJ) = NTAB
              ENDDO
            ENDIF
            GOTO 100
         ENDIF
         IF(DROP(LIMB)) GOTO 2
         LINK(NODL(LIMB)) = NTAB+1
         GOTO 1
      ELSEIF(TAB.NE.'C') THEN
         LINK(NODL(LIMB)) = NTAB+1
      ENDIF

      ENDDO

      GOTO 906

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: TABSUB - SUBSET NODE NOT IN TABLE D '//
     . '(TAB=",A,") FOR INPUT MNEMONIC ",A)') TAB,NEMO
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: TABSUB - THERE ARE TOO MANY NESTED '//
     . 'TABLE D SEQUENCES (TREES) WITHIN INPUT MNEMONIC ",A," - THE '//
     . 'LIMIT IS",I4)') NEMO,MAXLIM
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-01-YYY OPERATOR WAS '//
     . 'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-02-YYY OPERATOR WAS '//
     . 'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-07-YYY OPERATOR WAS '//
     . 'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-08-YYY OPERATOR WAS '//
     . 'NOT CANCELLED IN THE TREE BUILT FROM INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
906   WRITE(BORT_STR,'("BUFRLIB: TABSUB - ENTITIES WERE NOT '//
     . 'SUCCESSFULLY RESOLVED (BY EMULATING RESURSION) FOR SUBSET '//
     . 'DEFINED BY TBL A MNEM. ",A)') NEMO
      CALL BORT(BORT_STR)
907   WRITE(BORT_STR,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '//
     . 'CHANGE DATA WIDTH OPERATIONS IN THE TREE BUILT FROM INPUT ' //
     . 'MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
908   WRITE(BORT_STR,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '//
     . 'CHANGE DATA SCALE OPERATIONS IN THE TREE BUILT FROM INPUT ' //
     . 'MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
909   WRITE(BORT_STR,'("BUFRLIB: TABSUB - THERE ARE TWO SIMULTANEOUS '//
     . 'CHANGE REF VALUE OPERATIONS IN THE TREE BUILT FROM INPUT ' //
     . 'MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
910   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-03-YYY OPERATOR WAS '//
     . 'APPLIED WITHOUT ANY SUBSEQUENT 2-03-255 OPERATOR FOR '//
     . 'INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
911   WRITE(BORT_STR,'("BUFRLIB: TABSUB - A 2-03-000 OPERATOR WAS '//
     . 'ENCOUNTERED WITHOUT ANY PRIOR 2-03-YYY OPERATOR FOR '//
     . 'INPUT MNEMONIC ",A)') NEMO
      CALL BORT(BORT_STR)
912   CALL BORT('BUFRLIB: TABSUB - MXTCO OVERFLOW')
913   CALL BORT('BUFRLIB: TABSUB - MXTAMC OVERFLOW')
      END
