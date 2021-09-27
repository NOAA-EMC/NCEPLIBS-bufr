C> @file
C> @brief Check whether the characters in a string are all numeric

C> This logical function checks whether the characters in a string
C> are all numeric.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in]  STR - character*(*): String
C> @returns   digit - logical: .TRUE. if all characters in STR are
C>                    numeric; .FALSE. otherwise
C>
C> <b>Program History Log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 2007-01-19  J. Ator    -- Simplified logic
C> - 2009-03-23  J. Ator    -- Fixed minor bug caused by typo
C>
      LOGICAL FUNCTION DIGIT(STR)

      CHARACTER*(*) STR
      DIGIT = .FALSE.
      DO I=1,LEN(STR)
        IF( LLT(STR(I:I),'0') .OR. LGT(STR(I:I),'9') ) GOTO 100
      ENDDO
      DIGIT = .TRUE.

C  EXIT
C  ----

100   RETURN
      END
