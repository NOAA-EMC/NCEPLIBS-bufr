C> @file
C> @brief Get the number of data subsets from a BUFR message.

C> This function returns the total number of data subsets available
C> within the BUFR message that was most recently opened for reading
C> via a call to one of the other
C> [message-reading subroutines](@ref hierarchy)
C> for a specified Fortran logical unit.
C>
C> <p>The data subsets themselves do not need to have already been
C> read via previous calls to any of the
C> [subset-reading subroutines](@ref hierarchy).
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                     BUFR file
C> @returns nmsub   -- integer: Number of data subsets
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

      RECURSIVE FUNCTION NMSUB(LUNIT) RESULT(IRET)

      USE MODV_IM8B

      USE MODA_MSGCWD

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         IRET=NMSUB(MY_LUNIT)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = 0

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      IRET = MSUB(LUN)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: NMSUB - INPUT BUFR FILE IS CLOSED, IT MUST '//
     . 'BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: NMSUB - INPUT BUFR FILE IS OPEN FOR OUTPUT,'//
     . ' IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: NMSUB - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
      END
