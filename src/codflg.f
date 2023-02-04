C> @file
C> @brief Specify inclusion of code and flag tables when reading
C> master BUFR tables.

C> This subroutine is used to specify whether or not code and flag
C> table information should be included during all future reads of
C> master BUFR tables.
C>
C> @author J. Ator
C> @date 2017-10-13
C>
C> @param[in] CF   -- character*1: Flag indicating whether
C>                    or not to include code and flag table
C>                    information during all future reads of
C>                    master BUFR tables
C>                     - 'N' = No (the default)
C>                     - 'Y' = Yes
C>
C> <p>See [Master BUFR Tables](@ref dfbfmstab)
C> for more information about master BUFR tables.  In particlar, note
C> that Table B and Table D files are always read whenever master BUFR
C> tables are being used, but the reading of Code/Flag table files is
C> optional and should only be included if the user intends to make
C> one or more future calls to subroutine getcfmng(); otherwise, the
C> reading of Code/Flag table files will result in the unnecessary use
C> of memory and other system resources.
C>
C> <p>If Code/Flag tables are to be read and used, they must reside in
C> the same directory as the master Table B and Table D files on the
C> local filesystem, as specified within a separate call to
C> subroutine mtinfo().
C>
C> <p>This subroutine can be called at any time after the first call
C> to subroutine openbf(), and the specified value for CF will remain
C> in effect for all future reads of master BUFR tables, unless a
C> subsequent call is made to this subroutine to reset the value of
C> CF again.  If this subroutine is never called, a default value of
C> 'N' is used for CF, as set within subroutine bfrini().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2017-10-13 | J. Ator  | Original author |
C>
      SUBROUTINE CODFLG(CF)

      COMMON /TABLEF/ CDMF

      CHARACTER*128 BORT_STR
      CHARACTER*1   CDMF, CF

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      CALL CAPIT(CF)
      IF(CF.NE.'Y'.AND. CF.NE.'N') GOTO 900
      CDMF = CF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: CODFLG - INPUT ARGUMENT IS ",A1,'//
     . '", IT MUST BE EITHER Y OR N")') CF
      CALL BORT(BORT_STR)
      END
