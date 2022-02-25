!> @file
!> @brief Open a new message for output in a BUFR file that was
!> previously opened for writing.

!> This subroutine opens and initializes a new BUFR message within
!> internal arrays, for eventual output to logical unit LUNIT.
!>
!> @author J. Woollen
!> @date 1994-01-06
!>
!> <b>Usage:</b> call openmg( LUNIT, SUBSET, JDATE )
!>
!> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR
!>                     file
!> @param[in] SUBSET -- character*(*): Table A mnemonic for type of
!>                      BUFR message to be opened
!>                      (see [DX BUFR Tables](@ref dfbftab) for
!>                      further information about Table A mnemonics)
!> @param[in] JDATE -- integer: Date-time to be stored within Section 1
!>                     of BUFR message being opened, in format of either
!>                     YYMMDDHH or YYYYMMDDHH
!>
!> <p>Logical unit LUNIT should have already been opened for output
!> operations via a previous call to subroutine openbf().
!>
!> <p>This subroutine is similar to subroutine openmb(), except that it
!> will always open a new message for output, regardless of the values
!> of SUBSET and JDATE.  Any existing message within the internal
!> arrays will be automatically flushed and written to logical unit LUNIT
!> via an internal call to subroutine closmg().
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
!> | 2003-11-04 | J. Ator    | Added documentation |
!> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
!> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_openmg

    private
    public openmg

    interface openmg
        module procedure openmg_4_d, openmg_8
    end interface

    contains

    subroutine openmg_4_d( lunit, subset, jdate )
!       used when call arguments to openmg are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: lunit, jdate
        character(len=*), intent(in) :: subset

        integer :: my_lunit, my_jdate

        my_lunit = lunit
        my_jdate = jdate

        call openmg_body( my_lunit, subset, my_jdate )

    end subroutine openmg_4_d

    subroutine openmg_8( lunit, subset, jdate )
!       used when call arguments to openmg are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: lunit, jdate
        character(len=*), intent(in) :: subset

        integer :: my_lunit, my_jdate

        my_lunit = lunit
        my_jdate = jdate

        call openmg_body( my_lunit, subset, my_jdate )

    end subroutine openmg_8

end module

subroutine openmg_body( lunit, subset, jdate )

      USE MODA_MSGCWD
      use subroutine_closmg

      CHARACTER*(*) SUBSET

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  CHECK THE FILE STATUS
!  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.NE.0) CALL CLOSMG(LUNIT)
      CALL WTSTAT(LUNIT,LUN,IL, 1)

!  GET SOME SUBSET PARTICULARS
!  ---------------------------

!  .... Given SUBSET, returns MTYP,MSTB,INOD
      CALL NEMTBA(LUN,SUBSET,MTYP,MSTB,INOD)
!  .... Set pos. index for new Tbl A mnem.
      INODE(LUN) = INOD
!  .... Set date for new message
      IDATE(LUN) = I4DY(JDATE)

!  INITIALIZE THE OPEN MESSAGE
!  ---------------------------

      CALL MSGINI(LUN)
      CALL USRTPL(LUN,1,1)

!  EXITS
!  -----

      RETURN
900   CALL BORT('BUFRLIB: OPENMG - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: OPENMG - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')

end
