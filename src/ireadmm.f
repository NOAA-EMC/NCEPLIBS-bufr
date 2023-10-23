C> @file
C> @brief Read a BUFR message from internal arrays.
C>
C> @author J. Woollen @date 1999-11-18

C> Call NCEPLIBS-bufr subroutine readmm() and pass back its
C> return code as the function value.
C>
C> The use of this function allows the return code from readmm() to be
C> used as the target variable within an iterative program loop.
C>
C> @param[in,out] IMSG - integer: Message pointer within internal arrays
C> - On input, IMSG is the number of the BUFR message to be read into
C>   scope for further processing, counting from the beginning of the
C>   internal arrays in memory.
C>  - On output, IMSG is incremented by one from its input value.
C> @param[out] SUBSET - character*8: Table A mnemonic for type of
C> BUFR message that was read into scope (see [DX BUFR Tables](@ref
C> dfbftab) for further information about Table A mnemonics).
C> @param[out] IDATE - integer: Date-time stored within Section 1 of
C> BUFR message that was read into scope, in format of either
C> YYMMDDHH or YYYYMMDDHH, depending on the most recent call to
C> subroutine datelen().
      
C> @returns ireadmm - integer:
C> - 0 new BUFR message was successfully read into scope.
C> - -1 requested message number could not be found in internal arrays.
C>
C> @author J. Woollen @date 1999-11-18
      RECURSIVE FUNCTION IREADMM(IMSG,SUBSET,IDATE) RESULT(IRET)

      USE MODV_IM8B

      CHARACTER*8 SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(IMSG,IMSG,1)
         IRET=IREADMM(IMSG,SUBSET,IDATE)
         CALL X48(IMSG,IMSG,1)
         CALL X48(IDATE,IDATE,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL READMM(IMSG,SUBSET,IDATE,IRET)

      RETURN
      END
