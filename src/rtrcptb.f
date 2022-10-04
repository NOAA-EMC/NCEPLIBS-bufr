C> @file
C> @brief Read the tank receipt time from Section 1 of a BUFR message.

C> This subroutine reads the tank receipt time (if one exists) from
C> Section 1 of a BUFR message.  It is similar to subroutine rtrcpt(),
C> except that it operates on a BUFR message passed in via a memory
C> array, whereas rtrcpt() operates on the BUFR message that was read
C> into internal arrays via the most recent call to any of the other
C> [message-reading subroutines](@ref hierarchy) for a specified
C> Fortran logical unit.
C>
C> @author J. Ator
C> @date 2013-10-07
C>
C> @param[in]  MBAY -- integer(*): BUFR message
C> @param[out] IYR  -- integer: Tank receipt year
C> @param[out] IMO  -- integer: Tank receipt month
C> @param[out] IDY  -- integer: Tank receipt day
C> @param[out] IHR  -- integer: Tank receipt hour
C> @param[out] IMI  -- integer: Tank receipt minute
C> @param[out] IRET -- integer: return code
C>                     - 0 = normal return
C>                     - -1 = no tank receipt time exists within
C>                            MBAY
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2013-10-07 | J. Ator | Original author; adapted from rtrcpt() |
C> | 2022-10-04 | J. Ator | Added 8-byte wrapper |

      SUBROUTINE RTRCPTB(MBAY,IYR,IMO,IDY,IHR,IMI,IRET)

      USE MODV_IM8B

      DIMENSION	MBAY (*)

      INTEGER*8 IYR_8,IMO_8,IDY_8,IHR_8,IMI_8,IRET_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL RTRCPTB_8(MBAY,IYR_8,IMO_8,IDY_8,IHR_8,IMI_8,IRET_8)
         IYR=IYR_8
         IMO=IMO_8
         IDY=IDY_8
         IHR=IHR_8
         IMI=IMI_8
         IRET=IRET_8

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

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine rtrcptb().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine rtrcptb() directly.
C>
C> @author J. Ator
C> @date 2022-10-04
C>
C> @param[in]  MBAY -- integer(*): BUFR message
C> @param[out] IYR_8  -- integer*8: Tank receipt year
C> @param[out] IMO_8  -- integer*8: Tank receipt month
C> @param[out] IDY_8  -- integer*8: Tank receipt day
C> @param[out] IHR_8  -- integer*8: Tank receipt hour
C> @param[out] IMI_8  -- integer*8: Tank receipt minute
C> @param[out] IRET_8  -- integer*8: return code
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-10-04 | J. Ator    | Original author      |

      SUBROUTINE RTRCPTB_8(MBAY,IYR_8,IMO_8,IDY_8,IHR_8,IMI_8,IRET_8)

      DIMENSION	MBAY (*)

      INTEGER*8 IYR_8,IMO_8,IDY_8,IHR_8,IMI_8,IRET_8

      CALL RTRCPTB(MBAY,IYR,IMO,IDY,IHR,IMI,IRET)
      IYR_8=IYR
      IMO_8=IMO
      IDY_8=IDY
      IHR_8=IHR
      IMI_8=IMI
      IRET_8=IRET

      RETURN
      END
