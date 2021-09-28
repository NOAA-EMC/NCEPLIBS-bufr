C> @file
C> @brief Right-justify a character string

C> This subroutine right-justifies a character string by removing
C> all trailing blanks.  The string is modified in place.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in,out] STR - character*(*): String
C>
C> <b>Program History Log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 2021-09-28  J. Ator -- Changed from function to subroutine
C>
      SUBROUTINE RJUST(STR)

      CHARACTER*(*) STR
      IF(STR.EQ.' ') GOTO 100
      LSTR = LEN(STR)
      DO WHILE(STR(LSTR:LSTR).EQ.' ')
         DO I=LSTR,2,-1
         STR(I:I) = STR(I-1:I-1)
         ENDDO
         STR(1:1) = ' '
      ENDDO

C  EXIT
C  ----

100   RETURN
      END
