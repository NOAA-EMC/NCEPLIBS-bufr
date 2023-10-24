C> @file
C> @brief Get information about a descriptor, based on the WMO
C> bit-wise representation of an FXY value.
C>
C> @author J. Woollen @date 1994-01-06

C> Return information about a descriptor from the
C> internal DX BUFR tables, based on the WMO bit-wise representation
C> of the FXY value associated with that descriptor.
C>
C> For an description of the WMO bit-wise representation of the FXY
C> value, see ifxy().
C>
C> @param[in] LUN - integer: file ID associated with DX BUFR tables.
C> @param[in] IDN - integer: WMO bit-wise representation of FXY value
C> for descriptor.
C> @param[out] NEMO - character*(*): Mnemonic associated with IDN.
C> @param[out] TAB - character: Type associated with IDN:
C> - 'B' Table B descriptor
C> - 'D' Table D descriptor
C> - 'C' Table C operator
C> - 'R' Replication descriptor
C> - 'F' Replication factor
C> @param[out] IRET - integer:
C> - Positional index of IDN within internal Table B, if TAB = 'B'.
C> - Positional index of IDN within internal Table D, if TAB = 'D'.
C> - The X portion of the FXY value in IDN, if TAB = 'C'.
C> - ((-1) * the Y portion of the FXY value in IDN), if TAB = 'R' and the
C>   replication is regular (i.e. non-delayed).
C> - 5 if TAB = 'R' or TAB = 'F' and the replication is 1-bit delayed.
C> - 4 if TAB = 'R' or TAB = 'F' and the replication is 8-bit delayed (stack).
C> - 3 if TAB = 'R' or TAB = 'F' and the replication is 8-bit delayed.
C> - 2 if TAB = 'R' or TAB = 'F' and the replication is 16-bit delayed.
C> - 0 otherwise
C>
C> @author J. Woollen @date 1994-01-06
      SUBROUTINE NUMTAB(LUN,IDN,NEMO,TAB,IRET)

C     Note that the values within the COMMON /REPTAB/ arrays were
C     initialized within subroutine BFRINI.

      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)

      CHARACTER*(*) NEMO
      CHARACTER*6   ADN30,CID
      CHARACTER*3   TYPS
      CHARACTER*1   REPS,TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NEMO = ' '
      IRET = 0
      TAB = ' '

C  LOOK FOR A REPLICATOR OR A REPLICATION FACTOR DESCRIPTOR
C  --------------------------------------------------------

      IF(IDN.GE.IDNR(1,1) .AND. IDN.LE.IDNR(1,2)) THEN

C        Note that the above test is checking whether IDN is the bit-
C        wise representation of a FXY (descriptor) value denoting F=1
C        regular (i.e. non-delayed) replication, since, as was
C        initialized within subroutine BFRINI,
C        IDNR(1,1) = IFXY('101000'), and IDNR(1,2) = IFXY('101255').

         TAB  = 'R'
         IRET = -MOD(IDN,256)
         GOTO 100
      ENDIF

      DO I=2,5
         IF(IDN.EQ.IDNR(I,1)) THEN
            TAB  = 'R'
            IRET = I
            GOTO 100
         ELSEIF(IDN.EQ.IDNR(I,2)) THEN
            TAB  = 'F'
            IRET = I
            GOTO 100
         ENDIF
      ENDDO

C  LOOK FOR IDN IN TABLE B AND TABLE D
C  -----------------------------------

      CALL NUMTBD(LUN,IDN,NEMO,TAB,IRET)
      IF(IRET.NE.0) GOTO 100

C  LOOK FOR IDN IN TABLE C
C  -----------------------

      CID = ADN30(IDN,6)
      IF (IOKOPER(CID).EQ.1) THEN
         NEMO = CID(1:6)
         READ(NEMO,'(1X,I2)') IRET
         TAB  = 'C'
         GOTO 100
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
