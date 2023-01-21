C> @file
C> @brief Search for a Table A descriptor within the internal DX
C> BUFR tables
C>
C> ### Program history log
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1994-01-06 | J. Woollen | Original author
C> 1995-06-28 | J. Woollen | Increased the size of internal BUFR table arrays in order to handle bigger files
C> 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort()
C> 1999-11-18 | J. Woollen | The number of bufr files which can be opened at one time increased from 10 to 32
C> 2003-11-04 | J. Ator    | Added documentation
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally
C> 2009-05-07 | J. Ator    | Use nemtbax()
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine searches for a descriptor within Table A of the
C> internal DX BUFR tables.
C>
C> It is similar to subroutine nemtbax(), except that it calls
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
