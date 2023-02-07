/** @file
 *  @brief Close all system files previously opened via a C language
 *  interface.
 *
 *  ### Program History
 * | Date | Programmer | Comments |
 * | -----|------------|----------|
 * | 2005-11-29 | J. Ator | Original author |
 *
 *  @author J. Ator  @date 2005-11-29
 */
#include "bufrlib.h"
#include "cobfl.h"

/**
 *  This subroutine closes all system files that were opened via
 *  previous calls to subroutine cobfl().
 *
 *  @author J. Ator  @date 2005-11-29
 */
void ccbfl( void )
{
    unsigned short i;

    for ( i = 0; i < 2; i++ ) {
        if ( pbf[i] != NULL ) fclose( pbf[i] );
    }
}
