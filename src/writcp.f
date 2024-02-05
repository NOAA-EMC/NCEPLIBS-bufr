C> @file
C> @brief Write a data subset into a BUFR message using compression.
C>
C> @author J. Woollen @date 2002-05-14

C> Write a data subset into a BUFR message using compression.
C>
C> This subroutine is similar to subroutine writsb(), except that
C> when the subset is encoded and packed into the current message
C> for the BUFR file associated with logical unit LUNIT, it is
C> packed using compression as prescribed within the
C> [official WMO BUFR regulations](@ref manual).
C>
C> This subroutine activates compression via an internal call to
C> subroutine cmpmsg(), followed by an internal call to subroutine
C> writsb(), followed by a second internal call to subroutine
C> cmpmsg() to deactivate compression.  For this reason, most
C> application programs which write compressed BUFR messages now
C> call subroutines cmpmsg() and writsb() directly; however, this
C> subroutine is still supported within the NCEPLIBS-bufr software for
C> backwards-compatibility with certain legacy application programs.
C>
C> @param[in] lunit - integer: Fortran logical unit number for BUFR file.
C>
C> @author J. Woollen @date 2002-05-14

      RECURSIVE SUBROUTINE WRITCP(LUNIT)

      use modv_vars, only: im8b

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL WRITCP(MY_LUNIT)

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL CMPMSG('Y')

      CALL WRITSB(LUNIT)

      CALL CMPMSG('N')

      RETURN
      END
