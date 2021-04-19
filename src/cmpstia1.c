/** @file
 *  @brief Define a comparison between two master Code/Flag table
 *  entries.
 */
#include "bufrlib.h"
#include "cfe.h"

/**
 * This function defines a comparison between two entries within the
 * internal memory structure for storage of master Code/Flag table
 * entries.  The comparison is used by the intrinsic C functions
 * qsort and bsearch, and it differs from the the comparison in
 * function cmpstia2() because it compares all of the iffxyn, ifval,
 * iffxynd and ifvald components of the structure, whereas
 * cmpstia2() only compares the iffxyn and ifval components.
 *
 * @author J. Ator
 * @date 2017-11-13
 *
 * @param[in] pe1 - struct code_flag_entry*: First master Code/Flag
 *                  table entry
 * @param[in] pe2 - struct code_flag_entry*: Second master Code/Flag
 *                  table entry
 * @returns cmpstia1 - integer:
 *                     - -1 = pe1 is less than pe2
 *                     -  0 = pe1 is equal to pe2
 *                     -  1 = pe1 is greater than pe2
 *
 * <b>Program history log:</b>
 * - 2017-11-13  J. Ator    -- Original author
*/
int cmpstia1( const void *pe1, const void *pe2 )
{
	struct code_flag_entry *mype1 = ( struct code_flag_entry * ) pe1;
	struct code_flag_entry *mype2 = ( struct code_flag_entry * ) pe2;

	if ( mype1->iffxyn == mype2->iffxyn ) {
	    if ( mype1->ifval == mype2->ifval ) {
		if ( mype1->iffxynd == mype2->iffxynd ) {
		    if ( mype1->ifvald == mype2->ifvald ) return 0;
		    return ( mype1->ifvald < mype2->ifvald ? -1 : 1 );
		}
		else {
		    return ( mype1->iffxynd < mype2->iffxynd ? -1 : 1 );
		}
	    }
	    else {
		return ( mype1->ifval < mype2->ifval ? -1 : 1 );
	    }
	}
	else {
	    return ( mype1->iffxyn < mype2->iffxyn ? -1 : 1 );
	}
}
