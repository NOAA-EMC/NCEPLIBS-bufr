C> @file
C> @brief Determine the array size needed to store a BUFR message.

C> Given a character string containing Section 0 from a BUFR
C> message, this function determines the array size (in integers)
C> needed to store the entire BUFR message.
C>
C> <p>This function is similar to function nmwrd(), except that it
C> takes a character string as input rather than an integer array.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in]  SEC0  -- character*8: Section 0 from a BUFR message
C> @returns    lmsg  -- integer: Array size (in integers) needed to
C>                      store entire BUFR message
C>
C> @remarks
C> - In some cases, the value returned may be slightly larger than
C> the minimum number of integers needed to store the entire BUFR
C> message.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 2003-11-04 | D. Keyser | Unified/portable for WRF; added history documentation |
C> | 2004-08-18 | J. Ator | Improved documentation |
C> | 2005-11-29 | J. Ator | Use nmwrd() |
C> | 2022-10-04 | J. Ator | Added 8-byte wrapper |

      RECURSIVE FUNCTION LMSG(SEC0) RESULT(IRET)

      USE MODV_IM8B

      CHARACTER*8 SEC0,CSEC0
      DIMENSION   MSEC0(2)

      EQUIVALENCE(MSEC0,CSEC0)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF (IM8B) THEN
        IM8B = .FALSE.

        IRET = LMSG(SEC0)

        IM8B = .TRUE.
        RETURN
      END IF

      CSEC0 = SEC0
      IRET = NMWRD(MSEC0)

C  EXIT
C  ----

      RETURN
      END
