C> @file
C> @brief Write a minutes value into Section 1 of a BUFR message.
C>
C> @author J. Woollen @date 1994-01-06

C> Write a minutes value into Section 1 of the BUFR
C> message that was most recently opened for writing via a call to
C> one of the [message-writing subroutines](@ref hierarchy) for a
C> specified Fortran logical unit.
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                              BUFR file
C> @param[in] MINI  -- integer: Minutes value
C>
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE MINIMG(LUNIT,MINI)

      use modv_vars, only: im8b

      use moda_bitbuf

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(MINI,MY_MINI,1)
         CALL MINIMG(MY_LUNIT,MY_MINI)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

      CALL PKBS1(MINI,MBAY(1,LUN),'MINU')

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: MINIMG - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: MINIMG - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: MINIMG - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
      END
