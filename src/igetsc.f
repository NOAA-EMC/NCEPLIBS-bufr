C> @file
C> @brief Check for an abnormal status code associated with the
C> processing of a file.
C>
C> @author J. Ator @date 2010-05-11

C> Return a status code associated with any file that
C> was previously opened via a call to subroutine openbf(), so that
C> the application program can check whether the NCEPLIBS-bufr software
C> encountered any specific problems while processing the file.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file.
C> @returns igetsc - integer:
C> - 0 = no problems were encountered
C> - 1 = replication factor overflow in subroutine usrtpl()
C>
C> @remarks
C> - Once subroutine openbf() has been called for LUNIT, this function
C> can then be called any number of times and at any point throughout
C> the remainder of the life of the application program.
C>
C> @author J. Ator @date 2010-05-11
      RECURSIVE FUNCTION IGETSC(LUNIT) RESULT(IRET)

      use modv_vars, only: im8b

      use moda_stcode

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF (IM8B) THEN
        IM8B = .FALSE.

        CALL X84(LUNIT,MY_LUNIT,1)
        IRET = IGETSC(MY_LUNIT)

        IM8B = .TRUE.
        RETURN
      END IF

      IRET = 0

C     Make sure the specified logical unit is connected to the library.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

      IRET = ISCODES(LUN)

      RETURN
 900  CALL BORT('BUFRLIB: IGETSC - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
      END
