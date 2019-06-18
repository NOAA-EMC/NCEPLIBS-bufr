/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ARALLOCC
C   PRGMMR: ATOR             ORG: NP12       DATE: 2014-12-04
C
C ABSTRACT:  IF DYNAMIC MEMORY ALLOCATION IS BEING USED, THIS ROUTINE
C   IS CALLED DURING THE FIRST CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE
C   OPENBF TO DYNAMICALLY ALLOCATE MEMORY FOR ALL REQUIRED C LANGUAGE
C   ARRAYS.  THESE ARRAYS ARE SIZED USING VALUES INPUT DURING ONE OR
C   MORE PREVIOUS CALLS TO BUFR ARCHIVE LIBRARY SUBROUTINE ISETPRM, OR
C   ELSE USING ONE OR MORE DEFAULT VALUES SPECIFIED IN MODULE FILES IF
C   ISETPRM IS NEVER CALLED FOR THOSE PARTICULAR SIZE VALUES.
C
C   MEMORY ALLOCATED WITHIN THIS ROUTINE CAN BE FREED VIA A USER CALL
C   TO BUFR ARCHIVE LIBRARY ROUTINE ARDLLOCF (IN CASES WHERE THE
C   APPLICATION PROGRAM MAY WISH TO MOVE ON TO OTHER TASKS NOT
C   REQUIRING ANY FURTHER CALLS TO BUFR ARCHIVE LIBRARY ROUTINES), OR
C   ELSE IT WILL BE FREED AUTOMATICALLY WHEN THE APPLICATION PROGRAM
C   TERMINATES.
C
C PROGRAM HISTORY LOG:
C 2014-12-04  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL ARALLOCC
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     IGETPRM
C    THIS ROUTINE IS CALLED BY: OPENBF
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
#define IN_ARALLOCC
#include "cread.h"
#include "mstabs.h"

void arallocc( void )
{

    char brtstr[50] = "BUFRLIB: ARALLOCC FAILED ALLOCATING ";

    f77int nfiles;

    f77int mxmtbb;
    f77int mxmtbd;
    f77int maxcd;

/*
**  cread arrays
*/

    nfiles = igetprm( "NFILES", 6 );

    if ( ( pb = malloc( (nfiles+1) * sizeof(FILE *) ) ) == NULL ) {
	strcat( brtstr, "PB" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( lstpos = malloc( (nfiles+1) * sizeof(fpos_t) ) ) == NULL ) {
	strcat( brtstr, "LSTPOS" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

/*
**  mstabs arrays
*/

    mxmtbb = igetprm( "MXMTBB", 6 );
    mxmtbd = igetprm( "MXMTBD", 6 );
    maxcd = igetprm( "MAXCD", 5 );

    if ( ( MSTABS_BASE(ibfxyn) = malloc( mxmtbb * sizeof(f77int) ) ) == NULL ) {
	strcat( brtstr, "IBFXYN" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(cbscl) = malloc( mxmtbb * 4 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBSCL" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(cbsref) = malloc( mxmtbb * 12 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBSREF" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(cbbw) = malloc( mxmtbb * 4 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBBW" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(cbunit) = malloc( mxmtbb * 14 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBUNIT" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(cbmnem) = malloc( mxmtbb * 8 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBMNEM" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(cbelem) = malloc( mxmtbb * 120 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBELEM" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(idfxyn) = malloc( mxmtbd * sizeof(f77int) ) ) == NULL ) {
	strcat( brtstr, "IDFXYN" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(cdseq) = malloc( mxmtbd * 120 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CDSEQ" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(cdmnem) = malloc( mxmtbd * 8 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CDMNEM" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(ndelem) = malloc( mxmtbd * sizeof(f77int) ) ) == NULL ) {
	strcat( brtstr, "NDELEM" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( MSTABS_BASE(idefxy) = malloc( mxmtbd * maxcd * sizeof(f77int) ) ) == NULL ) {
	strcat( brtstr, "IDEFXY" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

}

#endif
