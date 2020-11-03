C> @file
C> @authors Jack Woollen
C> @authors Jeff Ator
C> @date 2020-07-16
C>      
C> @brief This subroutine closes the connection between logical unit
C> LUNIT and the BUFRLIB software.
C>
C> @param[in] LUNIT    - integer: Fortran logical unit number for BUFR file
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
C> PROGRAM HISTORY LOG:
C> - 1994-01-06  J. Woollen -- ORIGINAL AUTHOR
C> - 2003-11-04  J. Ator    -- Don't close lunit if opened as a NULL file
C>                             by openbf()
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation
C> - 2012-09-15  J. Woollen -- Modified for C/I/O/BUFR interface;
C>                             added call to closfb() to close C files
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C> - 2020-07-16  J. Ator    -- Add sanity check to ensure that openbf()
C>                             was previously called (needed for GSI)
C> 
C> THIS ROUTINE CALLS:        closfb() closmg() status() wtstat()
C> 
C> THIS ROUTINE IS CALLED BY: copybf() exitbufr() mesgbf() ufbinx()
C>                            ufbmem() ufbmex()   ufbtab()
C>                            <br>Also called by application programs.
C>
      SUBROUTINE CLOSBF(LUNIT)

      USE MODA_NULBFR

      INCLUDE 'bufrlib.inc'
      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

#ifdef DYNAMIC_ALLOCATION
      IF ( .NOT. ALLOCATED(NULL) ) THEN
        CALL ERRWRT('++++++++++++++++++++WARNING++++++++++++++++++++++')
        ERRSTR = 'BUFRLIB: CLOSBF WAS CALLED WITHOUT HAVING ' //
     .           'PREVIOUSLY CALLED OPENBF'
        CALL ERRWRT(ERRSTR)
        CALL ERRWRT('++++++++++++++++++++WARNING++++++++++++++++++++++')
        RETURN
      ENDIF
#endif

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.GT.0 .AND. IM.NE.0) CALL CLOSMG(LUNIT)
      IF(IL.NE.0 .AND. NULL(LUN).EQ.0) CALL CLOSFB(LUN)
      CALL WTSTAT(LUNIT,LUN,0,0)

C  CLOSE fortran UNIT IF NULL(LUN) = 0
C  -----------------------------------

      IF(NULL(LUN).EQ.0) CLOSE(LUNIT)

      RETURN
      END
