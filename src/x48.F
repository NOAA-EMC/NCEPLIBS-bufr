C> @file
C> @brief Encode one or more 4-byte integer values as 8-byte integer values.
C>
C> ### Program history log
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 2022-10-12 | J. Woollen  | Original author
C>
C> @author J. Woollen @date 2022-10-12

C> This subroutine reads an array containing a specified number
C> of 4-byte integer values and then re-encodes them as a corresponding
C> array of 8-byte integer values.
C>
C> @note IIN4 and IOUT8 may be the same array, as long as the underlying
C> memory contains sufficient space for the equivalent number of 8-byte
C> integer values.
C>
C> @param[in] IIN4 - integer(*): Value(s) encoded as 4-byte integers
C> @param[out] IOUT8 - integer(*): Value(s) from IIN4 now re-encoded as 8-byte integers
C> @param[in] NVAL - integer: Number of values in IIN4 to be re-encoded
C>
C> @author J. Woollen @date 2022-10-12

        SUBROUTINE X48(IIN4,IOUT8,NVAL)

        INTEGER   IIN4(*),IOUT8(*)

        INTEGER   ONES /-1/
        INTEGER   ZERO / 0/

C       Call subroutine WRDLEN, in case it hasn't already been called.

        CALL WRDLEN

C       IHI points to the first byte of the high-order (i.e. most
C       significant) 4-byte integer word within an 8-byte integer word.
C       ILO points to the first byte of the low-order (i.e. least
C       significant) 4-byte integer word within an 8-byte integer word.

#ifdef BIG_ENDIAN
        IHI=1
        ILO=5
#else
        IHI=5
        ILO=1
#endif

C       Re-encode the 4-byte values as 8-byte values.

        DO K = NVAL,1,-1
          L=2*K-1
          CALL MVB(IIN4(K),1,ITMP,ILO,4)
          IOUT8(L)=ITMP
          IF(IOUT8(L)<0) THEN
             CALL MVB(ONES,1,IOUT8(L),IHI,4)
          ELSE
             CALL MVB(ZERO,1,IOUT8(L),IHI,4)
          ENDIF
        END DO

        RETURN
        END
