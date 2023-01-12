/** @file
 * @brief Given the bit-wise representation of a descriptor,
 * this routine adds it to an ongoing array of descriptors, after
 * first making sure that there is enough room in the array.
 * @author J. Ator @date 2004-08-18
*/

#include "bufrlib.h"

/**
 * Given the bit-wise representation of a descriptor,
 * this routine adds it to an ongoing array of descriptors, after
 * first making sure that there is enough room in the array.
 * 
 * If an array overflow occurs, then an appropriate error message
 * will be written via bort().
 *
 * @param desc - bit-wise representation of descriptor to be written
 * into descary.
 * @param descary - array of descriptors.
 *
 * @param ndescary - number of descriptors written so far into
 * descary.
 *
 * @author J. Ator @date 2004-08-18
 */
void wrdesc( f77int desc, f77int descary[], f77int *ndescary )
{
    char errstr[129];

/*
**  Is there room in descary for desc ?
*/
    if ( ( *ndescary + 1 ) < MAXNC ) {
	descary[(*ndescary)++] = desc;
    }
    else {
	sprintf( errstr, "BUFRLIB: WRDESC - EXPANDED SECTION 3 CONTAINS"
			" MORE THAN %d DESCRIPTORS", MAXNC );
	bort( errstr, ( f77int ) strlen( errstr ) );
    }

    return;
}
