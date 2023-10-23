C> @file
C> @brief Search for a Table A descriptor within the internal DX
C> BUFR tables
C>
C> @author J. Woollen @date 1994-01-06

C> Search for a descriptor within Table A of the
C> internal DX BUFR tables.
C>
C> This subroutine is similar to subroutine nemtbax(), except that
C> it calls
C> subroutine bort() if the descriptor is not found in Table A,
C> whereas nemtbax() will return an INOD value of 0 in such cases.
C>
C> @param[in] LUN - integer: Internal I/O stream index associated
C>                  with DX BUFR tables
C> @param[in] NEMO - character*(*): Mnemonic for Table A descriptor
C> @param[out] MTYP - integer: Message type corresponding to NEMO
C> @param[out] MSBT - integer: Message subtype corresponding to NEMO
C> @param[out] INOD - integer: Positional index of NEMO within
C>                    internal Table A
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE NEMTBA(LUN,NEMO,MTYP,MSBT,INOD)

      CHARACTER*(*) NEMO
      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  LOOK FOR NEMO IN TABLE A
C  ------------------------

      CALL NEMTBAX(LUN,NEMO,MTYP,MSBT,INOD)
      IF(INOD.EQ.0) GOTO 900

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NEMTBA - CAN''T FIND MNEMONIC ",A)')
     . NEMO
      CALL BORT(BORT_STR)
      END
