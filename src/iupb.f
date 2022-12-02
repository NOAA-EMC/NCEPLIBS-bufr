C> @file
C> @brief Decode an integer value from an integer array.

C> This function decodes an integer value from within a specified
C> number of bits of an integer array, starting with the first
C> bit of a specified byte of the array.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] MBAY    -- integer(*): Array containing encoded value
C> @param[in] NBYT    -- integer: Byte within MBAY at whose first
C>                       bit to begin decoding
C> @param[in] NBIT    -- integer: Number of bits to be decoded
C> @returns iupb      -- integer: Decoded value
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 2003-11-04 | J. Ator    | Added documentation |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation |
C> | 2022-10-04 | J. Ator    | Added 8-byte wrapper |

      RECURSIVE FUNCTION IUPB(MBAY,NBYT,NBIT) RESULT(IRET)

      USE MODV_IM8B

      DIMENSION MBAY(*)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(NBYT,MY_NBYT,1)
         CALL X84(NBIT,MY_NBIT,1)
         IRET = IUPB(MBAY,MY_NBYT,MY_NBIT)

         IM8B=.TRUE.
         RETURN
      ENDIF

      MBIT = (NBYT-1)*8
      CALL UPB(IRET,NBIT,MBAY,MBIT)

      RETURN
      END
