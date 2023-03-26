C> @file
C> @brief Read a specified BUFR message from internal arrays.
C>
C> @author J. Woollen @date 1999-11-18

C> Call readmm() and return its return code.
C>
C> @param[in,out] IMSG -- integer: Message pointer within internal arrays
C>                        - On input, IMSG is the number of the BUFR
C>                          message to be read into scope for further
C>                          processing, counting from the beginning of
C>                          the internal arrays in memory
C>                        - On output, IMSG is incremented by one from
C>                          its input value
C> @param[out] SUBSET -- character*8: Table A mnemonic for type of BUFR
C>                       message that was read into scope
C>                       (see [DX BUFR Tables](@ref dfbftab) for
C>                       further information about Table A mnemonics)
C> @param[out] IDATE  -- integer: Date-time stored within Section 1 of
C>                       BUFR message that was read into scope,
C>                       in format of either YYMMDDHH or YYYYMMDDHH,
C>                       depending on the most
C>                       recent call to subroutine datelen()
C> @returns ireadmm   -- integer:
C>                          - 0 = new BUFR message was successfully
C>                                read into scope
C>                          - -1 = requested message number could not
C>                                 be found in internal arrays
C>
C> @remarks
C> - The use of this function allows the return code from readmm() to be
C> used as the target variable within an iterative program loop.
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
