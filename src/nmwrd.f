C> @file
C> @brief Determine the array size needed to store a BUFR message.
C>
C> @author J. Ator @date 2005-11-29

C> Given an integer array containing Section 0 from a BUFR
C> message, determine the array size (in integers)
C> needed to store the entire BUFR message.
C>
C> This function is similar to function lmsg(), except that it
C> takes an integer array as input rather than a character string.
C>
C> @param[in]  MBAY  -- integer(*): Section 0 from a BUFR message
C> @returns   nmwrd  -- integer: Array size (in integers) needed to
C>                      store entire BUFR message
C>
C> @remarks
C> - In some cases, the value returned may be slightly larger than
C> the minimum number of integers needed to store the entire BUFR
C> message.
C>
C> @author J. Ator @date 2005-11-29
        RECURSIVE FUNCTION NMWRD(MBAY) RESULT(IRET)

        use modv_vars, only: im8b

        COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)

        DIMENSION MBAY(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Check for I8 integers.

        IF (IM8B) THEN
            IM8B = .FALSE.

            IRET = NMWRD(MBAY)

            IM8B = .TRUE.
            RETURN
        END IF

        LENM = IUPBS01(MBAY,'LENM')
        IF(LENM.EQ.0) THEN
            IRET = 0
        ELSE
            IRET = ((LENM/8)+1)*(8/NBYTW)
        ENDIF

        RETURN
        END
