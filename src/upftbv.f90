!> @file
!> @brief Determine the bit settings equivalent to a numerical
!> flag table value.

!> Given a Table B mnemonic with flag table units and a
!> corresponding numerical data value, this subroutine determines
!> the bit settings equivalent to that numerical value.
!>
!> @author J. Ator
!> @date 2005-11-29
!>
!> <b>Usage:</b> call upftbv( LUNIT, NEMO, VAL, MXIB, IBIT, NIB )
!>
!> @param[in] LUNIT -- integer: Fortran logical unit number for
!>                     BUFR file
!> @param[in] NEMO  -- character*(*): Table B mnemonic with
!>                     flag table units
!> @param[in] VAL   -- real*8: Value corresponding to NEMO
!> @param[in] MXIB  -- integer: Dimensioned size (in integers) of
!>                     IBIT; used by the subroutine to ensure that
!>                     it doesn't overflow the IBIT array
!> @param[out] IBIT -- integer(*): Bit numbers which were set to
!>                     "On" (i.e. set to "1") in VAL
!> @param[out] NIB  -- integer: Number of bit numbers returned in
!>                     IBIT
!>
!> @remarks
!> - This subroutine is the logical inverse of function pkftbv().
!> - According to the WMO standard, bits within a bit field are
!> numbered from left to right, so bit #1 is always the high-order
!> (i.e. most significant) bit in any bit field.
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2005-11-29 | J. Ator | Original version |
!> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
!> | 2022-02-01 | J. Ator | Converted to module to consolidate _4, _d, and _8 variations into one build |
!>

module subroutine_upftbv

    private
    public upftbv

    interface upftbv
        module procedure upftbv_4_d, upftbv_8
    end interface

    contains

    subroutine upftbv_4_d( lunit, nemo, val, mxib, ibit, nib )
!       used when call arguments to upftbv are 4-byte integers

        implicit none

        integer(kind=4), intent(in) :: lunit, mxib
        character(len=*), intent(in) :: nemo
        real(kind=8), intent(in) :: val
        integer(kind=4), intent(out) :: ibit(:), nib

        integer :: my_lunit, my_mxib, my_ibit(mxib), my_nib

        my_lunit = lunit
        my_mxib = mxib

        call upftbv_body( my_lunit, nemo, val, my_mxib, my_ibit, my_nib )

        ibit(1:mxib) = my_ibit(1:mxib)
        nib = my_nib

    end subroutine upftbv_4_d

    subroutine upftbv_8( lunit, nemo, val, mxib, ibit, nib )
!       used when call arguments to upftbv are 8-byte integers

        implicit none

        integer(kind=8), intent(in) :: lunit, mxib
        character(len=*), intent(in) :: nemo
        real(kind=8), intent(in) :: val
        integer(kind=8), intent(out) :: ibit(:), nib

        integer :: my_lunit, my_mxib, my_ibit(mxib), my_nib

        my_lunit = lunit
        my_mxib = mxib

        call upftbv_body( my_lunit, nemo, val, my_mxib, my_ibit, my_nib )

        ibit(1:mxib) = my_ibit(1:mxib)
        nib = my_nib

    end subroutine upftbv_8

end module

subroutine upftbv_body(LUNIT,NEMO,VAL,MXIB,IBIT,NIB)

      USE MODA_TABABD

      REAL*8  VAL,R8VAL,R82I

      INTEGER IBIT (*)

      CHARACTER*(*) NEMO
      CHARACTER*128 BORT_STR
      CHARACTER*1   TAB

!----------------------------------------------------------------------
!----------------------------------------------------------------------

!     Perform some sanity checks.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

      CALL NEMTAB(LUN,NEMO,IDN,TAB,N)
      IF(N.EQ.0) GOTO 901
      IF(TABB(N,LUN)(71:74).NE.'FLAG') GOTO 902

!     Figure out which bits are set.
      
      NIB = 0
      R8VAL = VAL
      NBITS = VALX(TABB(N,LUN)(110:112))
      DO I=(NBITS-1),0,-1
          R82I = (2.)**I
          IF(ABS(R8VAL-R82I).LT.(0.005)) THEN
              NIB = NIB + 1
              IF(NIB.GT.MXIB) GOTO 903
              IBIT(NIB) = NBITS-I
              RETURN
          ELSEIF(R82I.LT.R8VAL) THEN
              NIB = NIB + 1
              IF(NIB.GT.MXIB) GOTO 903
              IBIT(NIB) = NBITS-I
              R8VAL = R8VAL - R82I
          ENDIF
      ENDDO

      RETURN
900   CALL BORT('BUFRLIB: UPFTBV - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
901   WRITE(BORT_STR,'("BUFRLIB: UPFTBV - MNEMONIC ",A," NOT FOUND IN TABLE B")') NEMO
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: UPFTBV - MNEMONIC ",A," IS NOT A FLAG TABLE")') NEMO
      CALL BORT(BORT_STR)
903   CALL BORT('BUFRLIB: UPFTBV - IBIT ARRAY OVERFLOW')

end
