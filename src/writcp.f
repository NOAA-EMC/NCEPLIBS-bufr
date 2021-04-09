C> @file
C> @brief Write a data subset into a BUFR message using compression

C> This subroutine is similar to subroutine writsb(), except that 
C> when the subset is encoded and packed into the current message
C> for the BUFR file associated with logical unit LUNIT, it is 
C> packed using compression as prescribed within the
C> [official WMO BUFR regulations](@ref manual).
C>
C> @author J. Woollen
C> @date 2002-05-14
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for BUFR file
C>
C> @remarks
C> - This subroutine activates compression via an internal call to
C> subroutine cmpmsg(), followed by an internal call to subroutine
C> writsb(), followed by a second internal call to subroutine
C> cmpmsg() to deactivate compression.  For this reason, most
C> application programs which write compressed BUFR messages now
C> call subroutines cmpmsg() and writsb() directly; however, this
C> subroutine is still supported within the BUFRLIB software for
C> backwards-compatibility with certain legacy application programs.
C>
C> <b>Program history log:</b>
C> - 2002-05-14  J. Woollen -- Original author
C> - 2005-03-09  J. Ator    -- Modified to use cmpmsg() and writsb()
C>
      SUBROUTINE WRITCP(LUNIT)

      CALL CMPMSG('Y')

      CALL WRITSB(LUNIT)

      CALL CMPMSG('N')

      RETURN
      END
