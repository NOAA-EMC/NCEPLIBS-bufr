C> @file
C> @brief Read the tank receipt time from Section 1 of a BUFR message.
C>
C> @author J. Ator @date 2009-03-23

C> Return the tank receipt time (if one exists) from
C> Section 1 of a BUFR message.
C>
C> This subroutine is similar to subroutine rtrcptb(),
C> except that rtrcptb() operates on a BUFR message passed in via a
C> memory array, whereas this subroutine operates on the BUFR message
C> that was read into internal arrays via the most recent call to any
C> of the other [message-reading subroutines](@ref hierarchy) for a
C> specified Fortran logical unit.
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                     BUFR file
C> @param[out] IYR  -- integer: Tank receipt year
C> @param[out] IMO  -- integer: Tank receipt month
C> @param[out] IDY  -- integer: Tank receipt day
C> @param[out] IHR  -- integer: Tank receipt hour
C> @param[out] IMI  -- integer: Tank receipt minute
C> @param[out] IRET -- integer: return code
C>                     - 0 = normal return
C>                     - -1 = no tank receipt time exists within the
C>                            BUFR message currently open for input
C>                            within internal arrays
C>
C> @author J. Ator @date 2009-03-23
      RECURSIVE SUBROUTINE RTRCPT(LUNIT,IYR,IMO,IDY,IHR,IMI,IRET)

      use modv_vars, only: im8b

      use moda_bitbuf

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL RTRCPT(MY_LUNIT,IYR,IMO,IDY,IHR,IMI,IRET)
         CALL X48(IYR,IYR,1)
         CALL X48(IMO,IMO,1)
         CALL X48(IDY,IDY,1)
         CALL X48(IHR,IHR,1)
         CALL X48(IMI,IMI,1)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

C     Check the file status.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C     Unpack the tank receipt time.

      CALL RTRCPTB(MBAY(1,LUN),IYR,IMO,IDY,IHR,IMI,IRET)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: RTRCPT - INPUT BUFR FILE IS CLOSED; IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: RTRCPT - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT; IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: RTRCPT - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE; NONE ARE')
      END
