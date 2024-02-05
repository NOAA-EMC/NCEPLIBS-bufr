C> @file
C> @brief Get the WMO bit-wise representation of the FXY value
C> corresponding to a child mnemonic in a Table D sequence.
C>
C> @author J. Woollen @date 1994-01-06

C> Returns the WMO bit-wise representation of the FXY value corresponding
C> to a child mnemonic of a Table D sequence parent mnemonic.
C>
C> For a description of the WMO bit-wise representation of the FXY
C> value, see ifxy().
C>
C> @param[in] ID - integer: Positional index of parent mnemonic
C> within internal BUFR Table D array tabd.
C> @param[in] LUN - integer: File ID.
C> @param[in] IENT - integer: Ordinal indicator of child mnemonic to
C> return from within tabd(id,lun) sequence. 0 indicates that a count
C> of the total number of child mnemonics within the sequence will be
C> returned.
C> @param[out] IRET - integer: Return value:
C> - WMO bit-wise representation of FXY value corresponding to IENT'th
C>   child mnemonic, if input IENT was > 0.
C> - Total number of child mnemonics, if input IENT was 0.
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE UPTDD(ID,LUN,IENT,IRET)

      use moda_tababd

      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)

      CHARACTER*128 BORT_STR
      CHARACTER*56  DXSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LDD = LDXD(IDXV+1)+1

C  CHECK IF IENT IS IN BOUNDS
C  --------------------------

      NDSC = IUPM(TABD(ID,LUN)(LDD:LDD),8)

      IF(IENT.EQ.0) THEN
         IRET = NDSC
         GOTO 100
      ELSEIF(IENT.LT.0 .OR. IENT.GT.NDSC) THEN
         GOTO 900
      ENDIF

C  RETURN THE DESCRIPTOR INDICATED BY IENT
C  ---------------------------------------

      IDSC = LDD+1 + (IENT-1)*2
      IRET = IUPM(TABD(ID,LUN)(IDSC:IDSC),16)

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: UPTDD - VALUE OF THIRD ARGUMENT IENT'//
     . ' (INPUT) IS OUT OF RANGE (IENT =",I4,")")') IENT
      CALL BORT(BORT_STR)
      END
