C> @file
C> @brief Get information about a descriptor, based on the FXY value

C> This subroutine returns information about a descriptor from the
C> internal DX BUFR tables, based on the bit-wise representation of
C> the FXY value associated with that descriptor.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUN - integer: Internal I/O stream index associated
C>                  with DX BUFR tables
C> @param[in] IDN - integer: Bit-wise representation of FXY value
C>                  for descriptor
C> @param[out] NEMO - character*(*): Mnemonic associated with IDN
C> @param[out] TAB - character: Type associated with IDN
C>                     - 'B' = Table B descriptor
C>                     - 'D' = Table D descriptor
C>                     - 'C' = Table C operator
C>                     - 'R' = Replication descriptor
C>                     - 'F' = Replication factor
C> @param[out] IRET - integer:
C>                     - Positional index of IDN within internal
C>                       Table B, if TAB = 'B'
C>                     - Positional index of IDN within internal
C>                       Table D, if TAB = 'D'
C>                     - The X portion of the FXY value in IDN, if
C>                       TAB = 'C'
C>                     - ((-1) * the Y portion of the FXY value in IDN),
C>                       if TAB = 'R' and the replication is regular
C>                       (i.e. non-delayed)
C>                     - 5, if TAB = 'R' or TAB = 'F' and the
C>                       replication is 1-bit delayed
C>                     - 4, if TAB = 'R' or TAB = 'F' and the
C>                       replication is 8-bit delayed (stack)
C>                     - 3, if TAB = 'R' or TAB = 'F' and the
C>                       replication is 8-bit delayed
C>                     - 2, if TAB = 'R' or TAB = 'F' and the
C>                       replication is 16-bit delayed
C>                     - 0, otherwise
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1995-06-28  J. Woollen -- Increased the size of internal BUFR table
C>                           arrays in order to handle bigger files
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to 32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2000-09-19  J. Woollen -- Added capability to encode and decode data
C>                           using the operator descriptors (BUFR table
C>                           C) for changing width and changing scale
C> - 2003-11-04  J. Ator    -- Added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                           documentation; corrected typo
C> - 2005-11-29  J. Ator    -- Added support for 207 and 208 operators
C> - 2009-04-21  J. Ator    -- Use numtbd()
C> - 2010-03-19  J. Ator    -- Added support for 204 and 205 operators
C> - 2012-03-02  J. Ator    -- Added support for 203 operator
C> - 2015-02-25  J. Ator    -- Allow processing of 2-2x, 2-3x and 2-4X
C>                           non-marker operators in DX tables
C>
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
