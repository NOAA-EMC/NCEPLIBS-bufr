C> @file
C> @brief Close a previously opened file and disconnect it from
C> the NCEPLIBS-bufr software.
C>
C> @author J. Woollen, J. Ator @date 1994-01-06

C> Close the connection between logical unit
C> LUNIT and the NCEPLIBS-bufr software.
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
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file.
C>
C> @author J. Woollen, J. Ator @date 1994-01-06

      RECURSIVE SUBROUTINE CLOSBF(LUNIT)

      use bufrlib

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
      IF(IL.NE.0 .AND. NULL(LUN).EQ.0) CALL CLOSFB_C(LUN)
      CALL WTSTAT(LUNIT,LUN,0,0)

C  CLOSE fortran UNIT IF NULL(LUN) = 0
C  -----------------------------------

      IF(NULL(LUN).EQ.0) CLOSE(LUNIT)

      RETURN
      END
