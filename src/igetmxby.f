C> @file
C> @brief Get the maximum length of a BUFR message that can be
C> written to an output file.
C>
C> @author J. Ator @date 2016-06-27

C> Return the maximum length of a BUFR message that can be written
C> to an output file by the NCEPLIBS-bufr software.
C>
C> @returns igetmxby - integer: Maximum length of a BUFR message
C> that can be written to an output file by the NCEPLIBS-bufr software.
C>
C> This maximum length value can be changed at any time via a
C> separate call to subroutine maxout().
C>
C> @author J. Ator @date 2016-06-27
      RECURSIVE FUNCTION IGETMXBY() RESULT(IRET)

      use modv_vars, only: im8b

      use moda_bitbuf

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------

C     CHECK FOR I8 INTEGERS.

      IF(IM8B) THEN
          IM8B=.FALSE.

          IRET = IGETMXBY()

          IM8B=.TRUE.
          RETURN
      ENDIF

      IRET = MAXBYT

      RETURN
      END
