C> @file
C> @brief Close a previously opened system file and disconnect it from
C> the BUFRLIB software.

C> This subroutine closes the connection between logical unit
C> LUNIT and the BUFRLIB software.
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 1994-01-06
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR file
C>
C> @remarks
C> - This subroutine will execute a Fortran "CLOSE" on logical unit LUNIT,
C> even though subroutine openbf() didn't previously handle the corresponding
C> Fortran "OPEN" of the same file.
C> - It's a good idea to call this subroutine for every LUNIT that was
C> opened to the software via openbf(); however, it's especially
C> important to do so when writing/encoding a BUFR file, in order to
C> ensure that all output is properly flushed to LUNIT.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 2003-11-04 | J. Ator    | Don't close lunit if opened as a NULL file by openbf() |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added history documentation |
C> | 2012-09-15 | J. Woollen | Modified for C/I/O/BUFR interface; added call to closfb() to close C files |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2020-07-16 | J. Ator    | Add sanity check to ensure that openbf() was previously called (needed for GSI) |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE CLOSBF(LUNIT)

      USE MODA_NULBFR
      USE MODV_IM8B

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL CLOSBF(MY_LUNIT)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IF ( .NOT. ALLOCATED(NULL) ) THEN
        CALL ERRWRT('++++++++++++++++++++WARNING++++++++++++++++++++++')
        ERRSTR = 'BUFRLIB: CLOSBF WAS CALLED WITHOUT HAVING ' //
     .           'PREVIOUSLY CALLED OPENBF'
        CALL ERRWRT(ERRSTR)
        CALL ERRWRT('++++++++++++++++++++WARNING++++++++++++++++++++++')
        RETURN
      ENDIF

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.GT.0 .AND. IM.NE.0) CALL CLOSMG(LUNIT)
      IF(IL.NE.0 .AND. NULL(LUN).EQ.0) CALL CLOSFB(LUN)
      CALL WTSTAT(LUNIT,LUN,0,0)

C  CLOSE fortran UNIT IF NULL(LUN) = 0
C  -----------------------------------

      IF(NULL(LUN).EQ.0) CLOSE(LUNIT)

      RETURN
      END
