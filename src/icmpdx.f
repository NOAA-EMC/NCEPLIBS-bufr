C> @file
C> @brief Check whether two BUFR files have the same DX BUFR table
C> information.
C>
C> @author J. Ator @date 2009-06-18

C> Check whether the full set of associated
C> [DX BUFR Table information](@ref dfbftab) is identical between
C> two Fortran logical units.
C>
C> Note that two different logical units can have identical DX BUFR
C> Table information associated with them even if they aren't actually
C> sharing the same DX BUFR table.
C>
C> @param[in]  LUD     -- integer: file ID for first BUFR file
C> @param[in]  LUN     -- integer: file ID for second BUFR file
C> @returns icmpdx     -- integer: Flag indicating whether the
C>                        BUFR file associated with LUD and the BUFR
C>                        file associated with LUN have the same DX
C>                        BUFR table information
C>                        - 0 = No
C>                        - 1 = Yes
C>
C> @author J. Ator @date 2009-06-18
      INTEGER FUNCTION ICMPDX(LUD,LUN)

      use moda_tababd

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     First, check whether the two units are actually sharing tables.
C     If so, then they obviously have the same table information.

      ICMPDX = ISHRDX(LUD,LUN)
      IF ( ICMPDX .EQ. 1 ) RETURN

C     Otherwise, check whether the internal Table A, B and D entries are
C     all identical between the two units.

      IF ( ( NTBA(LUD) .EQ. 0 ) .OR.
     .          ( NTBA(LUN) .NE. NTBA(LUD) ) ) RETURN
      DO I = 1, NTBA(LUD)
        IF ( IDNA(I,LUN,1) .NE. IDNA(I,LUD,1) ) RETURN
        IF ( IDNA(I,LUN,2) .NE. IDNA(I,LUD,2) ) RETURN
        IF ( TABA(I,LUN) .NE. TABA(I,LUD) ) RETURN
      ENDDO

        IF ( ( NTBB(LUD) .EQ. 0 ) .OR.
     .          ( NTBB(LUN) .NE. NTBB(LUD) ) ) RETURN
      DO I = 1, NTBB(LUD)
        IF ( IDNB(I,LUN) .NE. IDNB(I,LUD) ) RETURN
        IF ( TABB(I,LUN) .NE. TABB(I,LUD) ) RETURN
      ENDDO

      IF ( ( NTBD(LUD) .EQ. 0 ) .OR.
     .          ( NTBD(LUN) .NE. NTBD(LUD) ) ) RETURN
      DO I = 1, NTBD(LUD)
        IF ( IDND(I,LUN) .NE. IDND(I,LUD) ) RETURN
        IF ( TABD(I,LUN) .NE. TABD(I,LUD) ) RETURN
      ENDDO

      ICMPDX = 1

      RETURN
      END
