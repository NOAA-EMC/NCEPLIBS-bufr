C> @file
C> @brief Print a summary of merge activity.
C>
C> @author J. Woollen @date 1996-10-09

C> Print a summary of merge activity.
C>
C> @author J. Woollen @date 1996-10-09
      SUBROUTINE MRGINV

      COMMON /MRGCOM/ NRPL,NMRG,NAMB,NTOT
      COMMON /QUIET / IPRT

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++BUFRLIB+++++++++++++++++++++++')
      CALL ERRWRT('---------------------------------------------------')
      CALL ERRWRT('INVENTORY FROM MERGE PROCESS IN SUBROUTINE INVMRG:')
      CALL ERRWRT('---------------------------------------------------')
      WRITE ( UNIT=ERRSTR, FMT='(A,I8)' )
     .  'NUMBER OF DRB EXPANSIONS  = ', NRPL
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8)' )
     .  'NUMBER OF MERGES          = ', NMRG
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8)' )
     .  'NUMBER THAT ARE AMBIGUOUS = ', NAMB
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('---------------------------------------------------')
      WRITE ( UNIT=ERRSTR, FMT='(A,I9)' )
     .  'TOTAL NUMBER OF VISITS    = ', NTOT
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('---------------------------------------------------')
      CALL ERRWRT('+++++++++++++++++++++BUFRLIB+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

      RETURN
      END
