C> @file
C> @brief Read the next data subset from a BUFR file that was
C> previously opened for reading.
C>
C> @author J. Woollen @date 1994-01-06

C> This function calls BUFRLIB subroutine readns() and passes
C> back its return code as the function value.
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for
C>                       BUFR file
C> @param[out] SUBSET  -- character*8: Table A mnemonic for type of
C>                        data subset that was read
C>                        (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] IDATE   -- integer: Date-time stored within Section 1 of
C>                        BUFR message containing data subset that
C>                        was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C> @returns ireadns -- integer:
C>                     - 0 = new BUFR data subset was successfully
C>                           read into internal arrays
C>                     - -1 = there are no more BUFR data subsets in
C>                           the file connected to logical unit LUNIT
C>
C> @remarks
C> - The use of this function allows the return code from readns() to be
C> used as the target variable within an iterative program loop.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE FUNCTION IREADNS(LUNIT,SUBSET,IDATE) RESULT(IRET)

      USE MODV_IM8B

      CHARACTER*8 SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         IRET=IREADNS(MY_LUNIT,SUBSET,IDATE)
         CALL X48(IDATE,IDATE,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL READNS(LUNIT,SUBSET,IDATE,IRET)

      RETURN
      END
