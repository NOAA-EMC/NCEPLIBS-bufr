/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    STRTBFE
C   PRGMMR: ATOR             ORG: NCEP       DATE: 2017-11-13
C
C ABSTRACT:  THIS ROUTINE STORES A NEW ENTRY INTO THE INTERNAL MEMORY
C   STRUCTURE FOR CODE/FLAG TABLE INFORMATION.
C
C PROGRAM HISTORY LOG:
C 2017-11-13  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL STRTBFE ( IFXYN, IVAL, MEANING, LMEANING,
C                          IDFXY, IDVAL )
C
C   INPUT ARGUMENT LIST:
C     IFXYN    - INTEGER: BIT-WISE REPRESENTATION OF FXY NUMBER FOR
C                WHICH IVAL IS A DEFINED CODE OR FLAG TABLE ENTRY
C     IVAL     - INTEGER: CODE FIGURE OR BIT NUMBER
C     MEANING  - CHARACTER*(*): MEANING ASSOCIATED WITH IVAL
C     LMEANING - INTEGER: LENGTH (IN BYTES) OF MEANING
C     IDFXY    - INTEGER: BIT-WISE REPRESENTATION OF OTHER FXY NUMBER
C                UPON WHICH IVAL IS DEPENDENT, IF ANY
C		   -1 = NO DEPENDENCY
C     IDVAL    - INTEGER: CODE FIGURE OR BIT NUMBER ASSOCIATED WITH
C                IDFXY AND UPON WHICH IVAL IS DEPENDENT, IF ANY
C		   -1 = NO DEPENDENCY
C           
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT
C    THIS ROUTINE IS CALLED BY: SNTBFE
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"
#include "cfe.h"

void strtbfe( f77int *ifxyn, f77int *ival, char *meaning, f77int *lmeaning,
	      f77int *idfxy, f77int *idval )
{
    unsigned int mnlen;

    static char brtstr[50] = "BUFRLIB: STRTBFE - MXMTBF OVERFLOW";

    /*
    **  Confirm that there's room for another entry in the structure.
    */
    if ( nmtf >= mxmtbf ) bort( brtstr, ( f77int ) strlen( brtstr ) );

    /*
    **  Store the new entry.
    */
    cfe[nmtf].iffxyn = *ifxyn;
    cfe[nmtf].ifval = *ival;
    mnlen = ( *lmeaning > MAX_MEANING_LEN ? MAX_MEANING_LEN : *lmeaning );
    strncpy( &cfe[nmtf].ifmeaning[0], meaning, mnlen );
    cfe[nmtf].ifmeaning[mnlen] = '\0';
    cfe[nmtf].iffxynd = *idfxy;
    cfe[nmtf].ifvald  = *idval;
    nmtf++;
}
