C> @file
C> @brief Encode one or more 8-byte integer values as 4-byte integer
C> values.
C>
C> @author J. Woollen @date 2022-10-12

C> Encode one or more 8-byte integer values as 4-byte integer
C> values.
C>
C> @note IIN8 and IOUT4 may be the same array.
C>
C> @param[in] IIN8 - integer(*): Value(s) encoded as 8-byte integers
C> @param[out] IOUT4 - integer(*): Value(s) from IIN8 now re-encoded as 4-byte integers
C> @param[in] NVAL - integer: Number of values in IIN8 to be re-encoded
C>
C> @author J. Woollen @date 2022-10-12

        SUBROUTINE X84(IIN8,IOUT4,NVAL)

        INTEGER   IIN8(*),IOUT4(*)

C       Call subroutine WRDLEN, in case it hasn't already been called.

        CALL WRDLEN

C       ILO points to the first byte of the low-order (i.e. least
C       significant) 4-byte integer word within an 8-byte integer word.

#ifdef BIG_ENDIAN
        ILO=5
#else
        ILO=1
#endif

C       Re-encode the 8-byte values as 4-byte values.

        DO K = 1, NVAL
          L=2*K-1
          CALL MVB(IIN8(L),ILO,ITMP,1,4)
          IOUT4(K)=ITMP
        END DO

        RETURN
        END
