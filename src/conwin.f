C> @file
C> @brief Search consecutive subset buffer segments
C> for an element identified in the user string as a conditional node.
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Improved machine portability.
C> 1999-11-18 | J. Woollen | Increase number of open BUFR FILES.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | MAXJL increased from 15000 to 16000; unified/portable for WRF.
C> 2010-04-27 | J. Woollen | correct logical flaw and add documentation.
C> 2014-12-10 | J. Ator    | use modules instead of common blocks.
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine searches consecutive subset buffer segments
C> for an element identified in the user string as a conditional node
C> (i.e. an element which must meet a condition in order to be read
C> from or written to a data subset). If a conditional element is
C> found and it conforms to the condition, then the internal subset
C> buffer indices of the "window" (see below remarks) are returned to
C> the caller for processing.
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
C> @param[in] LUN integer I/O stream index into internal memory arrays.
C> @param[inout] INC1 integer subset buffer start index 
C> @param[inout] INC2 integer subset buffer ending index
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
