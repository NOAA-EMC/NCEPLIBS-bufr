C> @file
C> @brief Check whether the characters in a string are all numeric
C>
C> @author J. Woollen @date 1994-01-06

C> This logical function checks whether the characters in a string
C> are all numeric.
C>
C> @param[in]  STR -- character*(*): String
C> @returns   digit -- logical: .TRUE. if all characters in STR are
C>                     numeric; .FALSE. otherwise
C>
C> @author J. Woollen @date 1994-01-06
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
