C> @file
C> @brief Specify the use of standardization when writing BUFR messages.

C> This subroutine is used to specify whether BUFR messages output by
C> future calls to subroutines writsb(), writsa(), writcp(), copybf(),
C> copymg() or cpymem() should be internally reformatted to remove
C> all BUFRLIB software extensions to the WMO standard, prior to
C> actually writing each message. 
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
C> in effect for all future calls to subroutines writsb(), writsa(),
C> writcp(), copybf(), copymg() or cpymem() for all Fortran logical
C> units that are open for output within the application program,
C> unless a subsequent call is made to this subroutine to reset the
C> value of CF again.  If this subroutine is never called, a default
C> value of 'N' is used for CF, as set within subroutine bfrini().
C>
C> @author J. Ator
C> @date 2004-08-18
C>
C> @param[in] CF    - character*1: Flag indicating whether BUFR
C>                    messages output by future calls to
C>                    subroutines writsb(), writsa(), writcp(),
C>                    copybf(), copymg() or cpymem() are to be
C>                    standardized
C>                     - 'N' = No (the default)
C>                     - 'Y' = Yes
C>
C> <b>Program history log:</b>
C> - 2004-08-18  J. Ator    -- Original author
C>
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
