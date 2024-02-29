/** @file
 *  @brief Free all dynamically-allocated memory within
 *  internal C language arrays.
 * @author J. Ator @date 2014-12-04
 */

#include "bufrlib.h"
#include "cread.h"
#include "mstabs.h"

/**
 * Free all memory that was dynamically allocated
 * during a previous call to subroutine arallocc().
 *
 * @author J. Ator @date 2014-12-04
 */

void
ardllocc(void)
{

/*
**  cread arrays
*/

    free( pb );
    free( lstpos );

/*
**  mstabs arrays
*/

    free( ibfxyn_c );
    free( cbscl_c );
    free( cbsref_c );
    free( cbbw_c );
    free( cbunit_c );
    free( cbmnem_c );
    free( cbelem_c );
    free( idfxyn_c );
    free( cdseq_c );
    free( cdmnem_c );
    free( ndelem_c );
    free( idefxy_c );
    free( iafpk );
}

