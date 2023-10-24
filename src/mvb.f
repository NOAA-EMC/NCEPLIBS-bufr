C> @file
C> @brief Copy a specified number of bytes from
C> one packed binary array to another.
C>
C> @author Woollen @date 1994-01-06

C> Copy a specified number of bytes from
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
