C> @file
C> @brief Left-justify a character string

C> This subroutine left-justifies a character string by removing
C> all leading blanks.  The string is modified in place.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in,out] STR - character*(*): String
C> @param[out]    IRET - integer: return code
C>                       - 0 = normal return
C>                       - -1 = input string contained only blank
C>                              characters
C>                
C> <b>Program History Log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine
C>                             ABORT with call to new internal routine
C>                             bort()
C> - 2002-05-14  J. Woollen -- Changed from an entry point to increase
C>                             portability to other platforms
C> - 2003-11-04  J. Ator    -- Added documentation
C> - 2007-01-19  J. Ator    -- Restructured and added IRET argument
C>
      SUBROUTINE JSTCHR(STR,IRET)

      CHARACTER*(*) STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(STR.EQ.' ') THEN
         IRET = -1
      ELSE
         IRET = 0
         LSTR = LEN(STR)
         DO WHILE(STR(1:1).EQ.' ')
            STR = STR(2:LSTR)
         ENDDO
      ENDIF

      RETURN
      END
