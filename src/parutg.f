C> @file
C> @brief Parse a mnemonic from a character string.
C>
C> @author Woollen @date 1994-01-06

C> Parse a user-specified tag (mnemonic)
C> UTG that represents a value either being decoded from a BUFR file
C> (if it is being read) or encoded into a BUFR file (if it is being
C> written). This subroutine first checks to see if the tag contains
C> a condition character ('=', '!', '<', '>', '^' or '#'). If it does
C> not, nothing happens at this point. If it does, then the type of
C> condition character is noted and the tag is stripped of all
C> characters at and beyond the condition character. In either event,
C> the resultant tag is checked against those in the internal jump/
C> link subset table (in module tables). If found, the node
C> associated with the tag is returned (and it is either a "condition"
C> node or a "store" node depending of the presence or absence of a
C> condition character in UTG). Otherwise the node is returned as
C> zero. If the tag represents a condition node, then the condition
C> value (numeric characters beyond the condition character in the
C> user-specified tag input here) is returned.
C>
C> As an example of condition character usage, consider the following
C> example of a call to ufbint():
C>
C> @code
C>      REAL*8 USR(4,50)
C>             ....
C>             ....
C>      CALL UFBINT(LUNIN,USR,4,50,IRET,'PRLC<50000 TMDB WDIR WSPD')
C> @endcode
C>
C> Assuming that LUNIN points to a BUFR file open for input (reading),
C> then the USR array now contains IRET levels of data (up to a maximum
C> of 50) where the value of PRLC is/was less than 50000, along with
C> the corresponding values for TMDB, WDIR and WSPD at those levels.
C>
C> As another example, consider the following example of a call to
C> readlc() for a long character string:
C>
C> @code
C>      CHARACTER*200 LCHR
C>             ....
C>             ....
C>      CALL READLC(LUNIN,LCHR,'NUMID#3')
C> @endcode
C>
C> Assuming that LUNIN points to a BUFR file open for input (reading),
C> then the LCHR string now contains the value corresponding to the
C> third occurrence of NUMID within the current subset.
C>
C> Valid condition codes include:
C> - '<' - less than
C> - '>' - greater than
C> - '=' - equal to
C> - '!' - not equal to
C> - '#' - ordinal identifier for a particular occurrence of a long character string
C>
C> @param[in] LUN - integer: File ID.
C> @param[in] IO - integer: status indicator for BUFR file associated with LUN:
C> - 0 input file
C> - 1 output file
C> @param[in] UTG character*(*): user-supplied tag representing a value to be
C> encoded/decoded to/from BUFR file.
C> @param[out] NOD - integer: positional index in internal jump/link subset
C> table for TAG.
C> - 0 TAG not found in table
C> @param[out] KON - integer: indicator for type of condition character found in UTG:
C> - 0 no condition character found (NOD is a store node)
C> - 1 character '=' found (NOD is a condition node)
C> - 2 character '!' found (NOD is a condition node)
C> - 3 character '<' found (NOD is a condition node)
C> - 4 character '>' found (NOD is a condition node)
C> - 5 character '^' found (NOD is a condition node; specifically, a "bump" node)
C> - 6 character '#' found (NOD is a condition node)
C> @param[out] VAL - real: condition value associated with condition character found in utg:
C> - 0 = UTG does not have a condition character
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE PARUTG(LUN,IO,UTG,NOD,KON,VAL)

      USE MODA_MSGCWD
      USE MODA_TABLES

      COMMON /UTGPRM/ PICKY

      CHARACTER*(*) UTG
      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*20  ATAG
      CHARACTER*3   ATYP,BTYP
      CHARACTER*1   COND(6)
      DIMENSION     BTYP(8),IOK(8)
      LOGICAL       PICKY

      DATA NCHK   / 8/
      DATA BTYP   /'SUB','SEQ','REP','RPC','RPS','DRB','DRP','DRS'/
      DATA IOK    /  -1 ,  -1 ,  -1 ,  -1 ,  -1 ,   0 ,   0 ,   0 /

C----------------------------------------------------------------------
C     For now, set PICKY (see below) to always be .FALSE.
      PICKY = .FALSE.
      COND(1) = '='
      COND(2) = '!'
      COND(3) = '<'
      COND(4) = '>'
      COND(5) = '^'
      COND(6) = '#'
      NCOND   = 6
C----------------------------------------------------------------------

      ATAG  = ' '
      ATYP  = ' '
      KON   = 0
      NOD   = 0
      VAL   = 0
      LTG   = MIN(20,LEN(UTG))

C  PARSE UTG, SAVING INTO ATAG ONLY CHARACTERS PRIOR TO CONDITION CHAR.
C  --------------------------------------------------------------------

C     But first, take care of the special case where UTG denotes the
C     short (i.e. 1-bit) delayed replication of a Table D mnemonic.
C     This will prevent confusion later on since '<' and '>' are each
C     also valid as condition characters.

      IF((UTG(1:1).EQ.'<').AND.(INDEX(UTG(3:),'>').NE.0)) THEN
         ATAG = UTG
         GO TO 1
      ENDIF

      DO I=1,LTG
      IF(UTG(I:I).EQ.' ') GOTO 1
      DO J=1,NCOND
      IF(UTG(I:I).EQ.COND(J)) THEN
         KON = J
         ICV = I+1
         GOTO 1
      ENDIF
      ENDDO
      ATAG(I:I) = UTG(I:I)
      ENDDO

C  FIND THE NODE ASSOCIATED WITH ATAG IN THE SUBSET TABLE
C  ------------------------------------------------------

1     INOD = INODE(LUN)
      DO NOD=INOD,ISC(INOD)
      IF(ATAG.EQ.TAG(NOD)) GOTO 2
      ENDDO

C  ATAG NOT FOUND IN SUBSET TABLE
C  ------------------------------

C     So what do we want to do?  We could be "picky" and abort right
C     here, or we could allow for the possibility that, e.g. a user
C     application has been streamlined to always call UFBINT with the
C     same STR, even though some of the mnemonics contained within that
C     STR may not exist within the sequence definition of every
C     possible type/subtype that is being written by the application.
C     In such cases, by not being "picky", we could just allow BUFRLIB
C     to subsequently (and quietly, if IPRT happened to be set to -1
C     in COMMON /QUIET/!) not actually store the value corresponding
C     to such mnemonics, rather than loudly complaining and aborting.

      IF(KON.EQ.0 .AND. (IO.EQ.0.OR.ATAG.EQ.'NUL'.OR..NOT.PICKY)) THEN
C        i.e. (if this tag does not contain any condition characters)
C                 .AND.
C             ((either the file is open for input) .OR.
C              (the tag consists of 'NUL') .OR.
C              (we aren't being "picky"))
         NOD = 0
         GOTO 100
      ELSE
C        abort...
         GOTO 900
      ENDIF

C  ATAG IS FOUND IN SUBSET TABLE, MAKE SURE IT HAS A VALID NODE TYPE
C  -----------------------------------------------------------------

2     IF(KON.EQ.5) THEN
c  .... Cond. char "^" must be assoc. with a delayed replication
c       sequence (this is a "bump" node) (Note: This is obsolete but
c       remains for "old" programs using the BUFR ARCHIVE LIBRARY)
         IF(TYP(NOD-1).NE.'DRP' .AND. TYP(NOD-1).NE.'DRS') GOTO 901
      ELSEIF(KON.NE.6) THEN
C        Allow reading (but not writing) of delayed replication factors.
         ATYP = TYP(NOD)
         DO I=1,NCHK
           IF(ATYP.EQ.BTYP(I) .AND. IO.GT.IOK(I)) GOTO 902
         ENDDO
      ENDIF

C  IF CONDITION NODE, GET CONDITION VALUE WHICH IS A NUMBER FOLLOWING IT
C  ---------------------------------------------------------------------

      IF(KON.NE.0) THEN
         CALL STRNUM(UTG(ICV:LTG),NUM,IER)
         IF(IER.LT.0) GOTO 903
         VAL = NUM
      ENDIF

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: PARUTG - TRYING TO WRITE A MNEMONIC'//
     . ' (",A,") WHICH DOES NOT EXIST IN SUBSET TABLE")') ATAG
      WRITE(BORT_STR2,'(18X,"(UPON INPUT, IT CONTAINED THE CONDITION '//
     . 'CHARACTER ",A,")")') UTG(ICV-1:ICV-1)
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: PARUTG - BUMP NODE (MNEMONIC ",A,")'//
     . ' MUST REFER TO A DELAYED REPLICATION SEQUENCE, HERE TYPE IS "'//
     . ',A)') ATAG,TYP(NOD-1)
      CALL BORT(BORT_STR1)
902   WRITE(BORT_STR1,'("BUFRLIB: PARUTG - ILLEGAL NODE TYPE: ",A," '//
     . 'FOR MNEMONIC ",A)') ATYP,ATAG
      CALL BORT(BORT_STR1)
903   WRITE(BORT_STR1,'("BUFRLIB: PARUTG - CONDITION VALUE IN '//
     . 'MNEMONIC ",A," ILLEGAL BECAUSE ALL OTHER CHARACTERS IN '//
     . 'MNEMONIC MUST BE NUMERIC")') UTG
      CALL BORT(BORT_STR1)
      END
