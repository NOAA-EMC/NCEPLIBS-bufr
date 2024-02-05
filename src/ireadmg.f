C> @file
C> @brief Read the next message from a BUFR file that was previously
C> opened for reading.
C>
C> @author J. Woollen @date 1994-01-06

C> Call subroutine readmg() and pass
C> back its return code as the function value.
C>
C> The use of this function allows the return code from readmg() to be
C> used as the target variable within an iterative program loop.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file
C> @param[out] SUBSET - character*8: Table A mnemonic for type of BUFR
C> message that was read (see [DX BUFR Tables](@ref dfbftab)
C> for further information about Table A mnemonics)
C> @param[out] IDATE - integer: date-time stored within Section 1 of
C> BUFR message that was read, in format of either YYMMDDHH or YYYYMMDDHH,
C> depending on the most recent call to subroutine datelen()
C> @returns ireadmg - integer: Return code:
C> - 0 = new BUFR message was successfully read into internal arrays
C> - -1 = there are no more BUFR messages in the file connected to logical
C> unit LUNIT
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE FUNCTION IREADMG(LUNIT,SUBSET,IDATE) RESULT(IRET)

      use modv_vars, only: im8b

      CHARACTER*8 SUBSET

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         IRET=IREADMG(MY_LUNIT,SUBSET,IDATE)
         CALL X48(IDATE,IDATE,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL READMG(LUNIT,SUBSET,IDATE,IRET)

      END FUNCTION
