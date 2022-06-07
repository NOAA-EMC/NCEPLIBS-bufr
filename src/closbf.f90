!> @file
!> @brief Close a previously opened system file and disconnect it from the BUFRLIB software.

!> This subroutine closes the connection between logical unit LUNIT and the BUFRLIB software.
!>
!> @authors J. Woollen
!> @authors J. Ator
!> @date 1994-01-06
!>
!> <b>Usage:</b> call closbf( LUNIT )
!>
!> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR file
!>
!> @remarks
!> - This subroutine will execute a Fortran "CLOSE" on logical unit LUNIT, even though subroutine openbf() didn't previously
!> handle the corresponding Fortran "OPEN" of the same file.
!> - It's a good idea to call this subroutine for every LUNIT that was opened to the software via openbf(); however, it's
!> especially important to do so when writing/encoding a BUFR file, in order to ensure that all output is properly flushed to LUNIT.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 2003-11-04 | J. Ator    | Don't close lunit if opened as a NULL file by openbf() |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added history documentation |
!> | 2012-09-15 | J. Woollen | Modified for C/I/O/BUFR interface; added call to closfb() to close C files |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2020-07-16 | J. Ator    | Add sanity check to ensure that openbf() was previously called (needed for GSI) |
!> | 2022-06-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!> 

module subroutine_closbf

    private
    public closbf

    interface closbf
        module procedure closbf_4_d, closbf_8
    end interface

    contains

    subroutine closbf_4_d( lunit )
!       used when call argument to closbf is a 4-byte integer

        implicit none

        integer(kind=4), intent(in) :: lunit

        integer :: my_lunit

        my_lunit = lunit

        call closbf_body( my_lunit )

    end subroutine closbf_4_d

    subroutine closbf_8( lunit )
!       used when call argument to closbf is a 8-byte integer

        implicit none

        integer(kind=8), intent(in) :: lunit

        integer :: my_lunit

        my_lunit = lunit

        call closbf_body( my_lunit )

    end subroutine closbf_8

    subroutine closbf_body( lunit )

      USE MODA_NULBFR

      CHARACTER*128 ERRSTR

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IF ( .NOT. ALLOCATED(NULL) ) THEN
        CALL ERRWRT('++++++++++++++++++++WARNING++++++++++++++++++++++')
        ERRSTR = 'BUFRLIB: CLOSBF WAS CALLED WITHOUT HAVING PREVIOUSLY CALLED OPENBF'
        CALL ERRWRT(ERRSTR)
        CALL ERRWRT('++++++++++++++++++++WARNING++++++++++++++++++++++')
        RETURN
      ENDIF

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.GT.0 .AND. IM.NE.0) CALL CLOSMG(LUNIT)
      IF(IL.NE.0 .AND. NULL(LUN).EQ.0) CALL CLOSFB(LUN)
      CALL WTSTAT(LUNIT,LUN,0,0)

!  Close Fortran unit if NULL(LUN) = 0
!  -----------------------------------

      IF(NULL(LUN).EQ.0) CLOSE(LUNIT)

      RETURN

    end subroutine closbf_body

end module
