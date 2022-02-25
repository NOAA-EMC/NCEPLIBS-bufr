!> @file
!> @brief Close and write the current message to a BUFR file that was
!> previously opened for writing.
      
!> This subroutine closes the BUFR message that is currently open for
!> writing within internal arrays associated with logical unit
!> ABS(LUNIN), and it then writes the message to that logical unit.
!>
!> @authors J. Woollen
!> @authors D. Keyser
!> @date 1994-01-06
!>
!> <b>Usage:</b> call closmg( LUNIT )
!>
!> @param[in] LUNIN -- integer: Absolute value is Fortran logical unit
!>                     number for BUFR file
!>
!> <p>Logical unit ABS(LUNIN) should have already been opened for output
!> operations via a previous call to subroutine openbf().
!>
!> <p>If LUNIN < 0, then any message containing zero data subsets will
!> not be written to logical unit ABS(LUNIN) for the remainder of the
!> life of the application program.  This includes suppressing the
!> writing of any dummy messages containing dump center and initiation
!> times that normally appear in the first 2 messages of NCEP dump files.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 1994-01-06 | J. Woollen | Original author |
!> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant |
!> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 (necessary in order to process multiple BUFR files under the MPI) |
!> | 2000-09-19 | J. Woollen | Maximum message length increased from 10,000 to 20,000 bytes |
!> | 2003-05-19 | J. Woollen | Corrected a bug which prevented the dump center and initiation time messages from being written out |
!> | 2003-11-04 | J. Ator  | Added documentation |
!> | 2003-11-04 | S. Bender | Added remarks and routine interdependencies |
!> | 2003-11-04 | D. Keyser | Unified/portable for WRF; added history documentation; outputs more complete diagnostic info when routine terminates abnormally |
!> | 2004-08-09 | J. Ator | Maximum message length increased from 20,000 to 50,000 bytes |
!> | 2005-05-26 | D. Keyser | Add LUNIN < 0 option to suppress writing of all future zero-subset messsages to ABS(LUNIN) |
!> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator    | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_closmg

    private
    public closmg

    interface closmg
        module procedure closmg_4_d, closmg_8
    end interface

    contains

    subroutine closmg_4_d( lunit )
!       used when call argument to closmg is a 4-byte integer

        implicit none

        integer(kind=4), intent(in) :: lunit

        integer :: my_lunit

        my_lunit = lunit

        call closmg_body( my_lunit )

    end subroutine closmg_4_d

    subroutine closmg_8( lunit )
!       used when call argument to closmg is a 8-byte integer

        implicit none

        integer(kind=8), intent(in) :: lunit

        integer :: my_lunit

        my_lunit = lunit

        call closmg_body( my_lunit )

    end subroutine closmg_8

end module

subroutine closmg_body( lunin )

      USE MODA_MSGCWD
      USE MODA_MSGLIM
      USE MODA_BITBUF

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  CHECK THE FILE STATUS
!  ---------------------

      LUNIT = ABS(LUNIN)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(LUNIT.NE.LUNIN) MSGLIM(LUN) = 0
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.NE.0) THEN
         IF(NSUB(LUN).GT.0) THEN
            CALL MSGWRT(LUNIT,MBAY(1,LUN),MBYT(LUN))
         ELSE IF(NSUB(LUN).EQ.0.AND.NMSG(LUN).LT.MSGLIM(LUN)) THEN
            CALL MSGWRT(LUNIT,MBAY(1,LUN),MBYT(LUN))
         ELSE IF(NSUB(LUN).LT.0) THEN
            CALL WRCMPS(-LUNIT)
         ENDIF
      ENDIF
      CALL WTSTAT(LUNIT,LUN,IL,0)

!  EXITS
!  -----

      RETURN
900   CALL BORT('BUFRLIB: CLOSMG - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: CLOSMG - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')

end
