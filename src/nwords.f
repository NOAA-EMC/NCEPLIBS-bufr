C> @file
C> @brief Compute the length of a delayed replication sequence
C>
C> @author Woollen @date 1996-10-09

C> Compute the complete length of the delayed
C> replication sequence beginning at index N of a data subset.
C>
C> @param[in] N - integer: index to start of delayed replication sequence.
C> @param[in] LUN - integer: file ID
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
