C> @file
C> @brief Search a specified window for a conditional node.
C>
C> @author Woollen @date 1994-01-06

C> This function searches a "window" (see below remarks) for an
C> element identified in the user string as a conditional node.
C> A conditional node is an element which must meet a condition in order to be
C> read from or written to a data subset.
C> If a conditional element is found and it conforms to the
C> condition, then the index of the element within the window is returned;
C> otherwise a value of zero is returned.
C>
C> @note: See getwin() for an explanation of "windows" within the context
C> of a bufr data subset.
C>
C> @param[in] NC - integer: condition code:
C> - 1 '=' (equal)
C> - 2 '!' (not equal)
C> - 3 '<' (less than)
C> - 4 '>' (greater than)
C> @param[in] LUN - integer: i/o stream index into internal memory arrays
C> @param[in] INV1 - integer: first index of window to search
C> @param[in] INV2 - integer: last index of window to search
C>
C> @return integer: index within window of conditional node
C> conforming to specified condition.
C> - 0 none found.
C>
C> @author Woollen @date 1994-01-06
      FUNCTION INVCON(NC,LUN,INV1,INV2)

      USE MODA_USRINT

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
      COMMON /QUIET / IPRT

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  CHECK THE INVENTORY INTERVAL
C  ----------------------------

      IF(INV1.LE.0 .OR. INV1.GT.NVAL(LUN)) GOTO 99
      IF(INV2.LE.0 .OR. INV2.GT.NVAL(LUN)) GOTO 99

C  FIND AN OCCURANCE OF NODE IN THE WINDOW MEETING THIS CONDITION
C  --------------------------------------------------------------

      DO INVCON=INV1,INV2
      IF(INV(INVCON,LUN).EQ.NODC(NC)) THEN
         IF(KONS(NC).EQ.1 .AND. VAL(INVCON,LUN).EQ.IVLS(NC)) GOTO 100
         IF(KONS(NC).EQ.2 .AND. VAL(INVCON,LUN).NE.IVLS(NC)) GOTO 100
         IF(KONS(NC).EQ.3 .AND. VAL(INVCON,LUN).LT.IVLS(NC)) GOTO 100
         IF(KONS(NC).EQ.4 .AND. VAL(INVCON,LUN).GT.IVLS(NC)) GOTO 100
      ENDIF
      ENDDO

99    INVCON = 0
      IF(IPRT.GE.2) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT('BUFRLIB: INVCON - RETURNING WITH A VALUE OF 0')
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
