C> @file
C> @brief Specify the use of compression when writing BUFR messages.
C>
C> @author J. Ator @date 2005-03-09

C> Specify whether BUFR messages output
C> by future calls to [message-writing subroutines](@ref hierarchy) and
C> [subset-writing subroutines](@ref hierarchy) are to be compressed.
C>
C> This subroutine can be called at any time after the first call
C> to subroutine openbf(), and the specified value for CF will remain
C> in effect for all future calls to
C> [message-writing subroutines](@ref hierarchy) and
C> [subset-writing subroutines](@ref hierarchy) for all Fortran logical
C> units that are open for output within the application program, unless
C> a subsequent call is made to this subroutine to reset the value of
C> CF again.  If this subroutine is never called, a default value of
C> 'N' is used for CF, as set within subroutine bfrini().
C>
C> When compression is activated, it is implemented using the
C> algorithm for data subset compression prescribed within the
C> [official WMO BUFR regulations](@ref manual).
C> Compression is most useful when the data subsets to be
C> compressed are devoid of any delayed replication, and when
C> there is minimal variation of corresponding data values among
C> different data subsets within the same BUFR message. Otherwise,
C> compression may provide little to no benefit, and which is why
C> it is not activated by default.
C>
C> @param[in] CF - character*1: Flag indicating whether future BUFR
C> output messages are to be compressed
C>  - 'N' = No (the default)
C>  - 'Y' = Yes
C>
C> @author J. Ator @date 2005-03-09
      SUBROUTINE CMPMSG(CF)

      COMMON /MSGCMP/ CCMF

      CHARACTER*128 BORT_STR
      CHARACTER*1   CCMF, CF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      CALL CAPIT(CF)
      IF(CF.NE.'Y'.AND. CF.NE.'N') GOTO 900
      CCMF = CF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: CMPMSG - INPUT ARGUMENT IS ",A1,'//
     . '", IT MUST BE EITHER Y OR N")') CF
      CALL BORT(BORT_STR)
      END
