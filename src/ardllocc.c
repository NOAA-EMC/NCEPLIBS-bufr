/** @file
 *  @brief Free all dynamically-allocated memory within
 *  internal C language arrays.
 *
 * ### Program History Log
 * Date | Programmer | Comments
 * -----|------------|----------
 * 2014-12-04 | J. Ator | Original author.
 *
 * @author J. Ator @date 2014-12-04
 */

#include "bufrlib.h"
#include "cread.h"
#include "mstabs.h"

/**
 * This subroutine frees any memory that was dynamically allocated
 * during a previous call to subroutine arallocc().
 *
 * @author J. Ator @date 2014-12-04
 */

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

