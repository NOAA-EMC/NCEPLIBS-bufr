C> @file
C> @brief Read the tank receipt time from Section 1 of a BUFR message.
C>
C> @author J. Ator @date 2013-10-07

C> Return the tank receipt time (if one exists) from
C> Section 1 of a BUFR message.
C>
C> This subroutine is similar to subroutine rtrcpt(),
C> except that it operates on a BUFR message passed in via a memory
C> array, whereas rtrcpt() operates on the BUFR message that was read
C> into internal arrays via the most recent call to any of the other
C> [message-reading subroutines](@ref hierarchy) for a specified
C> Fortran logical unit.
C>
C> @param[in]  MBAY -- integer(*): BUFR message.
C> @param[out] IYR  -- integer: Tank receipt year.
C> @param[out] IMO  -- integer: Tank receipt month.
C> @param[out] IDY  -- integer: Tank receipt day.
C> @param[out] IHR  -- integer: Tank receipt hour.
C> @param[out] IMI  -- integer: Tank receipt minute.
C> @param[out] IRET -- integer: return code:
C> - 0 = normal return.
C> - -1 = no tank receipt time exists within MBAY.
C>
C> @author J. Ator @date 2013-10-07
      RECURSIVE SUBROUTINE RTRCPTB(MBAY,IYR,IMO,IDY,IHR,IMI,IRET)

      USE MODV_IM8B

      DIMENSION MBAY (*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL RTRCPTB(MBAY,IYR,IMO,IDY,IHR,IMI,IRET)
         CALL X48(IYR,IYR,1)
         CALL X48(IMO,IMO,1)
         CALL X48(IDY,IDY,1)
         CALL X48(IHR,IHR,1)
         CALL X48(IMI,IMI,1)
         CALL X48(IRET,IRET,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IRET = -1

C     Check whether the message contains a tank receipt time.

      IF(IUPBS01(MBAY,'BEN').EQ.4) THEN
        IS1BYT = 23
      ELSE
        IS1BYT = 19
      ENDIF
      IF( (IS1BYT+5) .GT. IUPBS01(MBAY,'LEN1') ) RETURN

C     Unpack the tank receipt time.

C     Note that IS1BYT is a starting byte number relative to the
C     beginning of Section 1, so we still need to account for
C     Section 0 when specifying the actual byte numbers to unpack
C     within the overall message.

      IMGBYT = IS1BYT + IUPBS01(MBAY,'LEN0')

      IYR = IUPB(MBAY,IMGBYT,16)
      IMO = IUPB(MBAY,IMGBYT+2,8)
      IDY = IUPB(MBAY,IMGBYT+3,8)
      IHR = IUPB(MBAY,IMGBYT+4,8)
      IMI = IUPB(MBAY,IMGBYT+5,8)

      IRET = 0

      RETURN
      END
