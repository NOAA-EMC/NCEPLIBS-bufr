C> @file
C> @brief Search consecutive subset buffer segments
C> for an element identified in the user string as a conditional node.
C> @author Woollen @date 1994-01-06

C> Search consecutive subset buffer segments for an
C> element identified in the user string as a conditional node. A conditional
C> node is an element which must meet a condition in order to be read
C> from or written to a data subset. If a conditional element is
C> found and it conforms to the condition, then return the internal subset
C> buffer indices of the "window" (see below remarks).
C>
C> The four conditions which can be exercised are:
C> - '<' - less than
C> - '>' - greater than
C> - '=' - equal
C> - '!' - not equal
C>
C> Each condition in a string is applied to one element, and all
C> conditions are 'and'ed to evaluate an outcome. For example, if the
C> condition string is: "POB<500 TOB>30 TQM<4" then the only levels of
C> data read or written are those with pressure lt 500 mb, temperature
C> gt 30 deg, and temperature quality mark < 4.
C>
C> See getwin() for an explanation of "windows" within the context of a
C> BUFR data subset.
C>
C> Function conwin() works with function invcon() to identify subset
C> buffer segments which conform to the set of conditions.
C>
C> @param[in] LUN - integer: file ID
C> @param[out] INC1 - integer: subset buffer start index
C> @param[inout] INC2 - integer: subset buffer ending index
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE CONWIN(LUN,INC1,INC2)

      USE MODA_USRINT

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  SPECIAL CASE
C  ------------

      IF(NCON.EQ.0) THEN
c  .... There are no condition nodes in the string
         INC1 = 1
         INC2 = NVAL(LUN)
         GOTO 100
      ENDIF

C  EVALUATE CONDITIONS TO SEE IF ANY MORE CASES
C  --------------------------------------------

15    CALL GETWIN(NODC(1),LUN,INC1,INC2)
      IF(INC1.GT.0) THEN
         DO NC=1,NCON
           ICON = INVCON(NC,LUN,INC1,INC2)
           IF(ICON.EQ.0) GOTO 15
         ENDDO
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
