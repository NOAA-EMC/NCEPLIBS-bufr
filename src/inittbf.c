/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INITTBF
C   PRGMMR: ATOR             ORG: NCEP       DATE: 2017-11-03
C
C ABSTRACT:  THIS ROUTINE INITIALIZES THE INTERNAL MEMORY STRUCTURE
C   FOR STORING CODE/FLAG TABLE INFORMATION, INCLUDING DYNAMICALLY
C   ALLOCATING SPACE FOR THIS STRUCTURE IF NEEDED.
C
C PROGRAM HISTORY LOG:
C 2017-11-03  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL INITTBF
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     IGETPRM
C    THIS ROUTINE IS CALLED BY: RDMTBF
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#include "bufrlib.h"
#define IN_INITTBF
#include "cfe.h"

void inittbf( void )
{
    char brtstr[50] = "BUFRLIB: INITTBF FAILED ALLOCATING CFE";

    /*
    **  Has array space for the internal memory structure been
    **  allocated yet?
    */
    if ( cfe == NULL ) {
	
        mxmtbf = igetprm( "MXMTBF", 6 );

        if ( ( cfe = malloc( mxmtbf * sizeof(struct code_flag_entry) ) )
			== NULL ) {
            bort( brtstr, ( f77int ) strlen( brtstr ) );
        }
    }

    nmtf = 0;
}
