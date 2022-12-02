C> @file
C> @brief Get the maximum length of a BUFR message that can be
C> written to an output file.

C> This function returns the maximum length of a BUFR message that
C> can be written to any output file by the BUFRLIB software.
C>
C> @author J. Ator
C> @date 2016-06-27
C>
C> @returns igetmxby -- integer: Maximum length of a BUFR message
C>                      that can be written to an output file by
C>                      the BUFRLIB software
C>
C> <p>This maximum length value can be changed at any time via a
C> separate call to subroutine maxout().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2016-06-27 | J. Ator | Original author |
C> | 2022-10-04 | J. Ator | Added 8-byte wrapper |

      RECURSIVE FUNCTION IGETMXBY() RESULT(IRET)

      USE MODV_IM8B

      USE MODA_BITBUF

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------

C     CHECK FOR I8 INTEGERS.

      IF(IM8B) THEN
          IM8B=.FALSE.

          IRET = IGETMXBY()

          IM8B=.TRUE.
          RETURN
      ENDIF

      CALL OPENBF(0,'FIRST',0)

      IRET = MAXBYT

      RETURN
      END
