C> @file
C> @brief Determine the bit settings equivalent to a numerical flag table value.
C>
C> @author J. Ator @date 2005-11-29

C> Given a Table B mnemonic with flag table units and a
C> corresponding numerical data value, this subroutine determines
C> the bit settings equivalent to that numerical value.
C>
C> This subroutine is the logical inverse of function pkftbv().
C>
C> According to the WMO standard, bits within a bit field are
C> numbered from left to right, so bit #1 is always the high-order
C> (i.e. most significant) bit in any bit field.
C>
C> @param[in]  LUNIT - integer: Fortran logical unit number for BUFR file.
C> @param[in]  NEMO - character*(*): Table B mnemonic with flag table units.
C> @param[in]  VAL - real*8: Value corresponding to NEMO.
C> @param[in]  MXIB - integer: Dimensioned size (in integers) of IBIT in the calling
C> program; used by the subroutine to ensure that it doesn't overflow the IBIT array.
C> @param[out] IBIT - integer: Bit numbers which were set to "On" (i.e. set to "1")
C> in VAL.
C> @param[out] NIB - integer: Number of bit numbers returned in IBIT.
C>
C> @author J. Ator @date 2005-11-29

      RECURSIVE SUBROUTINE UPFTBV(LUNIT,NEMO,VAL,MXIB,IBIT,NIB)

      USE MODV_IM8B

      USE MODA_TABABD

      REAL*8  VAL, R8VAL, R82I

      INTEGER IBIT(*), NIB(*)

      CHARACTER*(*) NEMO
      CHARACTER*128 BORT_STR
      CHARACTER*1   TAB

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(MXIB,MY_MXIB,1)
         CALL UPFTBV( MY_LUNIT, NEMO, VAL, MY_MXIB*2, IBIT, NIB )
         CALL X48(IBIT,IBIT,NIB(1))
         CALL X48(NIB,NIB,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

C     Perform some sanity checks.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

      CALL NEMTAB(LUN,NEMO,IDN,TAB,N)
      IF(N.EQ.0) GOTO 901
      IF(TABB(N,LUN)(71:74).NE.'FLAG') GOTO 902

C     Figure out which bits are set.

      NIB(1) = 0
      R8VAL = VAL
      CALL STRNUM(TABB(N,LUN)(110:112),NBITS,IERSN)
      DO I=(NBITS-1),0,-1
          R82I = (2.)**I
          IF(ABS(R8VAL-R82I).LT.(0.005)) THEN
              NIB(1) = NIB(1) + 1
              IF(NIB(1).GT.MXIB) GOTO 903
              IBIT(NIB(1)) = NBITS-I
              RETURN
          ELSEIF(R82I.LT.R8VAL) THEN
              NIB(1) = NIB(1) + 1
              IF(NIB(1).GT.MXIB) GOTO 903
              IBIT(NIB(1)) = NBITS-I
              R8VAL = R8VAL - R82I
          ENDIF
      ENDDO

      RETURN
900   CALL BORT('BUFRLIB: UPFTBV - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   WRITE(BORT_STR,'("BUFRLIB: UPFTBV - MNEMONIC ",A,'//
     . '" NOT FOUND IN TABLE B")') NEMO
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: UPFTBV - MNEMONIC ",A,'//
     . '" IS NOT A FLAG TABLE")') NEMO
      CALL BORT(BORT_STR)
903   CALL BORT('BUFRLIB: UPFTBV - IBIT ARRAY OVERFLOW')
      END
