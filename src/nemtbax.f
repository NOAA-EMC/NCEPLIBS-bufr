C> @file
C> @brief Search for a Table A descriptor within the internal DX
C> BUFR tables
C>
C> @author J. Woollen @date 1999-11-18

C> Search for a descriptor within Table A of the
C> internal DX BUFR tables.
C>
C> This subroutine is similar to subroutine nemtba(), except it returns
C> an INOD value of 0 if the descriptor is not found in Table A, whereas
C> nemtba() will call subroutine bort() in such cases.
C>
C> @param[in] LUN - integer: Internal I/O stream index associated
C>                   with DX BUFR tables
C> @param[in] NEMO - character*(*): Mnemonic for Table A descriptor
C> @param[out] MTYP - integer: Message type corresponding to NEMO
C> @param[out] MSBT - integer: Message subtype corresponding to NEMO
C> @param[out] INOD - integer:
C>                     - Positional index of NEMO within internal
C>                       Table A, if found
C>                     - 0, otherwise
C>
C> @author J. Woollen @date 1999-11-18
      SUBROUTINE NEMTBAX(LUN,NEMO,MTYP,MSBT,INOD)

      USE MODA_TABABD

      CHARACTER*(*) NEMO
      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      INOD = 0

C  LOOK FOR NEMO IN TABLE A
C  ------------------------

      DO I=1,NTBA(LUN)
      IF(TABA(I,LUN)(4:11).EQ.NEMO) THEN
         MTYP = IDNA(I,LUN,1)
         MSBT = IDNA(I,LUN,2)
         INOD = MTAB(I,LUN)
         IF(MTYP.LT.0 .OR. MTYP.GT.255) GOTO 900
         IF(MSBT.LT.0 .OR. MSBT.GT.255) GOTO 901
         GOTO 100
      ENDIF
      ENDDO

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NEMTBAX - INVALID MESSAGE TYPE (",I4'//
     . ',") RETURNED FOR MENMONIC ",A)') MTYP,NEMO
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: NEMTBAX - INVALID MESSAGE SUBTYPE ("'//
     . ',I4,") RETURNED FOR MENMONIC ",A)') MSBT,NEMO
      CALL BORT(BORT_STR)
      END
