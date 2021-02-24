C> @file
C> @brief Specify the use of compression when writing BUFR messages

C> This subroutine is used to specify whether BUFR messages output
C> by future calls to subroutines writsb() or writsa() are to be
C> compressed.
C>
C> @author J. Ator
C> @date 2005-03-09
C>
C> @param[in] CF    - character*1: Flag indicating whether BUFR
C>                    messages output by future calls to
C>                    subroutines writsb() or writsa() are to
C>                    be compressed
C>                     - 'N' = No (the default)
C>                     - 'Y' = Yes
C>
C> <p>This subroutine can be called at any time after the first call
C> to subroutine openbf(), and the specified value for CF will remain
C> in effect for all future calls to subroutines writsb() or writsa()
C> for all Fortran logical units that are open for output within the
C> application program, unless a
C> subsequent call is made to this subroutine to reset the value of
C> CF again.  If this subroutine is never called, a default value of
C> 'N' is used for CF, as set within subroutine bfrini().
C>
C> <p>When compression is activated, it is implemented using the
C> algorithm for data subset compression prescribed within the
C> [official WMO BUFR regulations](https://library.wmo.int/index.php?lvl=notice_display&id=10684#.X68yu8hKiUn).
C> Compression is most useful when the data subsets to be
C> compressed are devoid of any delayed replication, and when
C> there is minimal variation of corresponding data values among
C> different data subsets within the same BUFR message. Otherwise,
C> compression may provide little to no benefit, and which is why
C> it is not activated by default.
C>
C> <b>Program history log:</b>
C> - 2005-03-09  J. Ator    -- Original author
C>
C>
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
