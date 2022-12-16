C> @file
C> @brief Convert a date-time with a 2-digit year to a date-time with
C> a 4-digit year

C> This function converts a date-time with a 2-digit year (YYMMDDHH)
C> to a date-time with a 4-digit year (YYYYMMDDHH) using a windowing
C> technique.
C>
C> All 2-digit years greater than 40 are assumed to have a 4-digit
C> year beginning with 19 (i.e. 1941-1999), and all 2-digit years less
C> than or equal to 40 are assumed to have a 4-digit year beginning
C> with 20 (i.e. 2000-2040).  If the input date-time already contains
C> a 4-digit year, then the function simply returns that value.
C>
C> @author J. Woollen
C> @date 1998-07-08
C>
C> @param[in] IDATE -- integer: Date-time in format of either YYMMDDHH
C>                     (2-digit year) or YYYYMMDDHH (4-digit year)
C> @returns i4dy -- integer: Date-time in format of YYYYMMDDHH (4-digit
C>                  year)
C>
C> <b>Program History Log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1998-07-08 | J. Woollen | Original author |
C> | 1998-12-14 | J. Woollen | Modified to use 20 as the 2-digit year for windowing to a 4-digit year (00-20 ==> add 2000; 21-99 ==> add 1900) |
C> | 2003-11-04 | D. Keyser | Modified date calculations to stop using floating point arithmetic since this can lead to round off error and an improper result on some machines |
C> | 2018-06-29 | J. Ator | Changed 2-digit->4-digit year window range to (00-40 ==> add 2000; 41-99 ==> add 1900) |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

      RECURSIVE FUNCTION I4DY(IDATE) RESULT(IRET)

      USE MODV_IM8B

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(IDATE,MY_IDATE,1)
         IRET=I4DY(MY_IDATE)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IF(IDATE.LT.10**8) THEN
         IY = IDATE/10**6
         IF(IY.GT.40) THEN
            IRET = IDATE + 19*100000000
         ELSE
            IRET = IDATE + 20*100000000
         ENDIF
      ELSE
         IRET = IDATE
      ENDIF

      RETURN
      END
