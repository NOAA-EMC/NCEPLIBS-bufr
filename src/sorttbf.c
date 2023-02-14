/** @file
 *  @brief Sort entries within the master Code/Flag table.
 *
 *  @author J. Ator @date 2017-11-16
 */
#include "bufrlib.h"
#include "cfe.h"

/**
 *  This subroutine sorts the entries within the internal memory
 *  structure for storage of master Code/Flag table entries, in
 *  preparation for future searches using subroutine srchtbf().
 *
 *  @author J. Ator @date 2017-11-16
*/
void sorttbf( void )
{
    qsort( &cfe[0], ( size_t ) nmtf, sizeof( struct code_flag_entry ),
        ( int (*) ( const void *, const void * ) ) cmpstia1 );
}
