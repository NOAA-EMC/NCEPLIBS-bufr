C> @file
C> @brief Check whether two BUFR files are sharing the same DX BUFR
C> table.
C>
C> @author J. Ator @date 2009-06-18

C> Check whether the same
C> [DX BUFR Table](@ref dfbftab) is being shared between two
C> Fortran logical units.
C>
C> @param[in]  LUD     -- integer: file ID for first BUFR file
C> @param[in]  LUN     -- integer: file ID for second BUFR file
C> @returns ishrdx     -- integer: Flag indicating whether the same
C>                        DX BUFR table is being shared between the
C>                        BUFR file associated with LUD and the BUFR
C>                        file associated with LUN
C>                        - 0 = No
C>                        - 1 = Yes
C>
C> @author J. Ator @date 2009-06-18
      INTEGER FUNCTION ISHRDX(LUD,LUN)

      USE MODA_TABABD

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Note that, for any file ID LUx, the MTAB(*,LUx)
C     array contains pointer indices into the internal jump/link table
C     for each of the Table A mnemonics that is currently defined for
C     that LUx value.  Thus, if all of these indices are identical for
C     two different LUx values, then the associated logical units are
C     sharing table information.

      IF ( ( NTBA(LUD) .GE. 1 ) .AND.
     +      ( NTBA(LUD) .EQ. NTBA(LUN) ) ) THEN
        II = 1
        ISHRDX = 1
        DO WHILE ( ( II .LE. NTBA(LUD) ) .AND. ( ISHRDX .EQ. 1 ) )
          IF ( ( MTAB(II,LUD) .NE. 0 ) .AND.
     +          ( MTAB(II,LUD) .EQ. MTAB(II,LUN) ) ) THEN
            II = II + 1
          ELSE
            ISHRDX = 0
          ENDIF
        ENDDO
      ELSE
        ISHRDX = 0
      ENDIF

      RETURN
      END
