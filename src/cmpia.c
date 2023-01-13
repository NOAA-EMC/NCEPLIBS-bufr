/** @file
 * @brief Defines a comparison between two integers for use by the
 * binary search function bsearch.
 *
 * ### Program History Log
 * Date | Programmer | Comments
 * -----|------------|---------
 * 2009-03-23 | J. Ator | Initial.
 *
 * @author Ator @date 2009-03-23
*/

#include "bufrlib.h"

/**
 * This routine defines a comparison between two integers
 * for use by the binary search function bsearch.
 *
 * @param pf1 - first integer to be compared.
 * @param pf2 - second integer to be compared.
 *
 * @return 
 * - -1 PF1 is less than PF2
 * - 0 PF1 is equal to PF2
 * - 1 PF1 is greater than PF2
 *
 * @author Ator @date 2009-03-23
 */
int cmpia( const void *pf1, const void *pf2 )
{
	f77int *mypf1 = ( f77int * ) pf1;
	f77int *mypf2 = ( f77int * ) pf2;

	if ( *mypf1 == *mypf2 ) return 0;

	return ( *mypf1 < *mypf2 ? -1 : 1 );
}
