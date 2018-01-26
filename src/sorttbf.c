/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SORTTBF
C   PRGMMR: ATOR             ORG: NCEP       DATE: 2017-11-16
C
C ABSTRACT:  THIS ROUTINE SORTS THE CONTENTS OF THE INTERNAL MEMORY
C   STRUCTURE FOR STORING CODE/FLAG TABLE INFORMATION, IN PREPARATION
C   FOR LATER SEARCHES USING THE BSEARCH (BINARY SEARCH) FUNCTION.
C
C PROGRAM HISTORY LOG:
C 2017-11-16  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL SORTTBF
C
C REMARKS:
C    THIS ROUTINE CALLS:        CMPSTIA1
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
#include "cfe.h"

void sorttbf( void )
{
    qsort( &cfe[0], ( size_t ) nmtf, sizeof( struct code_flag_entry ),
	( int (*) ( const void *, const void * ) ) cmpstia1 );
}
