C> @file
C> @brief Specify the use of standardization when writing BUFR messages.
C>
C> @author J. Ator @date 2004-08-18

C> This subroutine is used to specify whether BUFR messages output by
C> future calls to [message-writing subroutines](@ref hierarchy) and
C> [subset-writing subroutines](@ref hierarchy) should be internally
C> reformatted to remove all BUFRLIB software extensions to the
C> WMO standard, prior to actually writing each message.
C>
C> <p>It is strongly recommended to use this subroutine (or,
C> alternatively, subroutine stndrd() for messages which already exist
C> in memory arrays) whenever BUFR messages are being written that will
C> potentially be read using software other than the BUFRLIB software.
C> Otherwise, by default the output messages will contain a number of
C> extensions to allow for faster reading and more efficient storage,
C> but which will be encoded using non-standard descriptors in
C> Section 3, and therefore likely be unrecognizable to other software
C> packages.
C>
C> <p>This subroutine can be called at any time after the first call
C> to subroutine openbf(), and the specified value for CF will remain
C> in effect for all future calls to
C> [message-writing subroutines](@ref hierarchy) and
C> [subset-writing subroutines](@ref hierarchy) for all Fortran logical
C> units that are open for output within the application program,
C> unless a subsequent call is made to this subroutine to reset the
C> value of CF again.  If this subroutine is never called, a default
C> value of 'N' is used for CF, as set within subroutine bfrini().
C>
C> @param[in] CF   -- character*1: Flag indicating whether future BUFR
C>                    output messages are to be standardized
C>                     - 'N' = No (the default)
C>                     - 'Y' = Yes
C>
C> @author J. Ator @date 2004-08-18
      SUBROUTINE STDMSG(CF)

      COMMON /MSGSTD/ CSMF

      CHARACTER*128 BORT_STR
      CHARACTER*1   CSMF, CF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      CALL CAPIT(CF)
      IF(CF.NE.'Y'.AND. CF.NE.'N') GOTO 900
      CSMF = CF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: STDMSG - INPUT ARGUMENT IS ",A1,'//
     . '", IT MUST BE EITHER Y OR N")') CF
      CALL BORT(BORT_STR)
      END
