C> @file
C> @brief Read the next data subset from a BUFR message
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 2002-05-14 | J. Woollen | Changed from an entry point to increase portability to other platforms |
C> | 2003-11-04 | S. Bender | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser | Unified/portable for WRF; added documentation |
C> | 2022-10-04 | J. Ator | Added 8-byte wrapper |
C>
C> @author J. Woollen @date 1994-01-06
      
C> This function calls BUFRLIB subroutine readsb() and 
C> passes back its return code as the function value.
C>
C> @remarks
C> - The use of this function allows the return code from readsb() to be
C> used as the target variable within an iterative program loop.
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for
C>                         BUFR file
C> @returns ireadsb -- integer:
C>                     - 0 = new BUFR data subset was successfully
C>                           read into internal arrays
C>                     - -1 = there are no more BUFR data subsets in
C>                            the BUFR message
C>
C> @author J. Woollen @date 1994-01-06

      RECURSIVE FUNCTION IREADSB(LUNIT) RESULT(IRET)

      USE MODV_IM8B

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      
C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         IRET=IREADSB(MY_LUNIT)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL READSB(LUNIT,IRET)

      RETURN
      END
