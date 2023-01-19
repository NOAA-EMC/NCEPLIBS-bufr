/** @file
 *  @brief Dynamically allocate C language arrays within internal memory.
 *
 * ### Program History Log
 * Date | Programmer | Comments 
 * -----|------------|----------
 * 2014-12-04 | J. Ator | Original author.
 * 2021-05-17 | J. Ator | Allow up to 24 characters in cbunit.
 * 2023-01-18 | J. Ator | Remove MSTABS_BASE macro.
 *
 * @author J. Ator @date 2014-12-04
*/

#include "bufrlib.h"
/** Macro to control whether certain variables are explicitly declared or referenced as extern. */
#define IN_ARALLOCC
#include "cread.h"
#include "mstabs.h"

/**
 * This subroutine is called internally during the first call to
 * subroutine openbf() from an application program, in order to
 * dynamically allocate internal C language arrays based on parameter
 * values set during one or more previous calls to function isetprm().
 *
 * This subroutine isn't normally called directly from an application
 * program, since it's automatically called internally during the first
 * call to subroutine openbf() from an application program.
 *
 * @remarks
 * All memory allocated within this subroutine can be freed via a
 * subsequent call to subroutine exitbufr() from within the
 * application program, or else it will be freed automatically by the
 * operating system once the application program terminates.
 *
 * @author J. Ator @date 2014-12-04
 */

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

    if ( ( ibfxyn_c = malloc( mxmtbb * sizeof(f77int) ) ) == NULL ) {
	strcat( brtstr, "IBFXYN" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( cbscl_c = malloc( mxmtbb * 4 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBSCL" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( cbsref_c = malloc( mxmtbb * 12 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBSREF" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( cbbw_c = malloc( mxmtbb * 4 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBBW" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( cbunit_c = malloc( mxmtbb * 24 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBUNIT" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( cbmnem_c = malloc( mxmtbb * 8 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBMNEM" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( cbelem_c = malloc( mxmtbb * 120 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CBELEM" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( idfxyn_c = malloc( mxmtbd * sizeof(f77int) ) ) == NULL ) {
	strcat( brtstr, "IDFXYN" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( cdseq_c = malloc( mxmtbd * 120 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CDSEQ" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( cdmnem_c = malloc( mxmtbd * 8 * sizeof(char) ) ) == NULL ) {
	strcat( brtstr, "CDMNEM" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( ndelem_c = malloc( mxmtbd * sizeof(f77int) ) ) == NULL ) {
	strcat( brtstr, "NDELEM" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

    if ( ( idefxy_c = malloc( mxmtbd * maxcd * sizeof(f77int) ) ) == NULL ) {
	strcat( brtstr, "IDEFXY" );
        bort( brtstr, ( f77int ) strlen( brtstr ) );
    }

}
