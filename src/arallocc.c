/** @file
 *  @brief Dynamically allocate C language arrays within internal memory.
*/

#include "bufrlib.h"
#define IN_ARALLOCC
#include "cread.h"
#include "mstabs.h"

/**
 * For dynamic allocation builds of the library, this subroutine is
 * called internally during the first call to subroutine openbf() from
 * an application program, in order to dynamically allocate internal
 * C language arrays based on parameter values set during one or more
 * previous calls to function isetprm().
 *
 * <p>This subroutine isn't normally called directly from an application
 * program, since it's automatically called internally during the first
 * call to subroutine openbf() from an application program.
 *
 * @author J. Ator
 * @date 2014-12-04
 *
 * @remarks
 * - All memory allocated within this subroutine can be freed via a
 *   subsequent call to subroutine exitbufr() from within the
 *   application program, or else it will be freed automatically by the
 *   operating system once the application program terminates.
 *
 * <b>Program history log:</b>
 * - 2014-12-04  J. Ator    -- Original author
 * - 2021-05-17  J. Ator    -- Allow up to 24 characters in cbunit
 */

void arallocc( void )
{

#ifdef DYNAMIC_ALLOCATION

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

    if ( ( MSTABS_BASE(cbunit) = malloc( mxmtbb * 24 * sizeof(char) ) ) == NULL ) {
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

#endif

}
