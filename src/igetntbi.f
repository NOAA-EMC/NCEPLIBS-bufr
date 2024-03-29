C> @file
C> @brief Get the next index for storing an entry within an internal
C> DX BUFR table
C>
C> @author J. Ator @date 2009-03-23

C> Return the next available index for storing an
C> entry within a specified internal DX BUFR table.
C>
C> @param[in]  LUN   -- integer: file ID
C> @param[in]  CTB   -- character: Type of internal DX BUFR table for
C>                      which to return the next available index
C>                        - 'A' = Table A
C>                        - 'B' = Table B
C>                        - 'D' = Table D
C> @returns igetntbi -- integer: Next available index for storing an
C>                      entry within CTB
C>
C> @author J. Ator @date 2009-03-23
        FUNCTION IGETNTBI ( LUN, CTB )

        use moda_tababd

        CHARACTER*128 BORT_STR
        CHARACTER*1   CTB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        IF ( CTB .EQ. 'A' ) THEN
          IGETNTBI = NTBA(LUN) + 1
          IMAX = NTBA(0)
        ELSE IF ( CTB .EQ. 'B' ) THEN
          IGETNTBI = NTBB(LUN) + 1
          IMAX = NTBB(0)
        ELSE  ! CTB .EQ. 'D'
          IGETNTBI = NTBD(LUN) + 1
          IMAX = NTBD(0)
        ENDIF
        IF ( IGETNTBI .GT. IMAX ) GOTO 900

        RETURN
900     WRITE(BORT_STR,'("BUFRLIB: IGETNTBI - NUMBER OF INTERNAL TABLE'
     .    //'",A1," ENTRIES EXCEEDS THE LIMIT (",I4,")")') CTB, IMAX
        CALL BORT(BORT_STR)
        END
