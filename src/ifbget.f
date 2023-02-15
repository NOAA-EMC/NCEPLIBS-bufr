C> @file
C> @brief Check whether there are any more data subsets available to be
C> read from a BUFR message.
C>
C> @author J. Woollen @date 1994-01-06

C> This function checks whether there are any more data subsets
C> available to be read from within the BUFR message that is
C> open for reading via the most recent call to any of the
C> [message-reading subroutines](@ref hierarchy) for a specified
C> Fortran logical unit.
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                     BUFR file
C> @returns ifbget -- integer:
C>                     -  0 = there is at least one more data subset
C>                            to be read from the BUFR message
C>                     - -1 = there are no more data subsets
C>                            to be read from the BUFR message
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE FUNCTION IFBGET(LUNIT) RESULT(IRET)

      USE MODV_IM8B

      USE MODA_MSGCWD

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         IRET=IFBGET(MY_LUNIT)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  MAKE SURE A FILE/MESSAGE IS OPEN FOR INPUT
C  ------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE
C  ---------------------------------------------

      IF(NSUB(LUN).LT.MSUB(LUN)) THEN
         IRET = 0
      ELSE
         IRET = -1
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: IFBGET - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: IFBGET - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: IFBGET - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
      END
