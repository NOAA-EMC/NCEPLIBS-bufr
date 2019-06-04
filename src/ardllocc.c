/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ARDLLOCC
C   PRGMMR: ATOR             ORG: NP12       DATE: 2014-12-04
C
C ABSTRACT:  THIS ROUTINE FREES ANY MEMORY THAT WAS DYNAMICALLY
C   ALLOCATED BY A PREVIOUS CALL TO BUFR ARCHIVE LIBRARY ROUTINE
C   ARALLOCC.
C
C PROGRAM HISTORY LOG:
C 2014-12-04  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL ARDLLOCC
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: ARDLLOCF
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#ifdef DYNAMIC_ALLOCATION

#include "bufrlib.h"
#include "cread.h"
#include "mstabs.h"

void ardllocc( void )
{

/*
**  cread arrays
*/

    free( pb );
    free( lstpos );

/*
**  mstabs arrays
*/

    free( MSTABS_BASE(ibfxyn) );
    free( MSTABS_BASE(cbscl) );
    free( MSTABS_BASE(cbsref) );
    free( MSTABS_BASE(cbbw) );
    free( MSTABS_BASE(cbunit) );
    free( MSTABS_BASE(cbmnem) );
    free( MSTABS_BASE(cbelem) );
    free( MSTABS_BASE(idfxyn) );
    free( MSTABS_BASE(cdseq) );
    free( MSTABS_BASE(cdmnem) );
    free( MSTABS_BASE(ndelem) );
    free( MSTABS_BASE(idefxy) );

}

#endif
