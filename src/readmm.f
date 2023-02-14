C> @file
C> @brief Read a specified BUFR message from internal arrays.
C>
C> @author J. Woollen @date 1999-11-18

C> This subroutine reads a specified BUFR message from internal
C> arrays in memory, so that it is now in scope for processing
C> via a subsequent call to subroutine rdmems().
C>
C> <p>BUFR messages should already be stored within internal
C> arrays in memory via one or more previous calls to
C> subroutine ufbmem().
C>
C> <p>This subroutine is similar to subroutine rdmemm(), except that
C> this subroutine increments the value of IMSG prior to returning to
C> the calling program, which in turn allows it to be easily called
C> within an iterative program loop.
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
C> @param[out] JDATE  -- integer: Date-time stored within Section 1 of
C>                       BUFR message that was read into scope,
C>                       in format of either YYMMDDHH or YYYYMMDDHH,
C>                       depending on the most
C>                       recent call to subroutine datelen()
C> @param[out] IRET   -- integer: return code
C>                          - 0 = requested message was
C>                                successfully read into scope
C>                          - -1 = requested message number could not
C>                                 be found in internal arrays
C>
C> @author J. Woollen @date 1999-11-18
      RECURSIVE SUBROUTINE READMM(IMSG,SUBSET,JDATE,IRET)

      USE MODV_IM8B

      CHARACTER*8 SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(IMSG,IMSG,1)
         CALL READMM(IMSG,SUBSET,JDATE,IRET)
         CALL X48(IMSG,IMSG,1)
         CALL X48(JDATE,JDATE,1)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL RDMEMM(IMSG,SUBSET,JDATE,IRET)

      IMSG = IMSG+1

      RETURN
      END
