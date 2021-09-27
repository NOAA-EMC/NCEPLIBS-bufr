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
C> @param[in] IDATE - integer: Date-time in format of either YYMMDDHH
C>                    (2-digit year) or YYYYMMDDHH (4-digit year)
C> @returns i4dy - integer: Date-time in format of YYYYMMDDHH (4-digit
C>                 year)
C>
C> <b>Program History Log:</b>
C> - 1998-07-08  J. Woollen -- Original author
C> - 1998-12-14  J. Woollen -- Modified to use 20 as the 2-digit year for
C>                           windowing to a 4-digit year (00-20 ==> add
C>                           2000; 21-99 ==> ADD 1900)
C> - 2003-11-04  D. Keyser  -- Modified date calculations to no longer use
C>                           floating point arithmetic since this can
C>                           lead to round off error and an improper
C>                           resulting date on some machines
C> - 2018-06-29  J. Ator    -- Changed 2-digit->4-digit year window range 
C>                           to (00-40 ==> add 2000; 41-99 ==> add 1900)
C>
      FUNCTION I4DY(IDATE)

      IF(IDATE.LT.10**8) THEN
         IY = IDATE/10**6
         IF(IY.GT.40) THEN
            I4DY = IDATE + 19*100000000
         ELSE
            I4DY = IDATE + 20*100000000
         ENDIF
      ELSE
         I4DY = IDATE
      ENDIF

      RETURN
      END
