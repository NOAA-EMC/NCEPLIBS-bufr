C> @file
C> @brief Adds up the complete length of the delayed
C> replication sequence beginning at index n of the data subset.      
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1996-10-09 | J. Woollen | Original author.
C> 1999-11-18 | J. Woollen | Increased the number of open bufr files to 32.
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | maxjl increased to 16000; unified/portable for wrf; documentation.
C> 2009-03-31 | J. Woollen | Added documentation.
C> 2014-12-10 | J. Ator    | Use modules instead of common blocks.
C>
C> @author Woollen @date 1996-10-09
      
C> This function adds up the complete length of the delayed
C> replication sequence beginning at index n of the data subset.
C>
C> @param[in] N - integer: index to start of delayed replication sequence.
C> @param[in] LUN - integer: i/o stream index into internal memory arrays.
C>
C> @return complete length of delayed replication sequence within data subset.
C>
C> @author Woollen @date 1996-10-09
      FUNCTION NWORDS(N,LUN)

      USE MODA_USRINT

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NWORDS = 0

      DO K=1,NINT(VAL(N,LUN))
      NWORDS = NWORDS + NINT(VAL(NWORDS+N+1,LUN))
      ENDDO

      RETURN
      END
