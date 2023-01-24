C> @file
C> @brief Copy a specified number of bytes from
C> one packed binary array to another.      
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced call to cray routine "abort" with bort().
C> 1998-10-27 | J. Woollen | Modified to correct problems caused by in- lining code with fpp directives.
C> 2002-05-14 | J. Woollen | Removed old cray compiler directives.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | Unified/portable for wrf; documentation; outputs more diagnostic info.
C> 2005-11-29 | J. Ator    | Maximum number of bytes to copy increased from 24000 to mximb.
C> 2014-10-22 | J. Ator    | Merge two do loops into one, and remove mximb parameter and dimensioning of nval.
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine copies a specified number of bytes from
C> one packed binary array to another.
C>
C> @param[in] IB1 - integer: packed input binary array.
C> @param[in] NB1 - integer: pointer to first byte in IB1 to copy from.
C> @param[out] IB2 - integer: packed output binary array.
C> @param[in] NB2 - integer: pointer to first byte in IB2 to copy to.
C> @param[in] NBM - integer: number of bytes to copy.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE MVB(IB1,NB1,IB2,NB2,NBM)

      CHARACTER*128 BORT_STR
      DIMENSION     IB1(*),IB2(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      JB1 = 8*(NB1-1)
      JB2 = 8*(NB2-1)

      DO N=1,NBM
        CALL UPB(NVAL,8,IB1,JB1)
        CALL PKB(NVAL,8,IB2,JB2)
      ENDDO

C  EXITS
C  -----

      RETURN
      END
