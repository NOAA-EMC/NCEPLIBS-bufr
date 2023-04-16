/** @file
 *  @brief Define a comparison between two master Code/Flag table
 *  entries.
 *
 * @author J. Ator @date 2017-11-13
 */
#include "bufrlib.h"
#include "cfe.h"

/**
 * This function defines a comparison between two entries within the
 * internal memory structure for storage of master Code/Flag table
 * entries.  The comparison is used by the intrinsic C function
 * bsearch, and it differs from the the comparison in
 * function cmpstia1() because it only compares the iffxyn and ifval
 * components of the structure, whereas cmpstia1() compares all of
 * the iffxyn, ifval, iffxynd and ifvald components of the structure.
 *
 * @param pe1 - struct code_flag_entry*: First master Code/Flag table entry
 * @param pe2 - struct code_flag_entry*: Second master Code/Flag table entry
 * @returns cmpstia2 - integer:
 * - -1 = pe1 is less than pe2
 * -  0 = pe1 is equal to pe2
 * -  1 = pe1 is greater than pe2
 *
 * @author J. Ator @date 2017-11-13
*/
int
cmpstia2(const void *pe1, const void *pe2)
{
        struct code_flag_entry *mype1 = ( struct code_flag_entry * ) pe1;
        struct code_flag_entry *mype2 = ( struct code_flag_entry * ) pe2;

        if ( mype1->iffxyn == mype2->iffxyn ) {
            if ( mype1->ifval == mype2->ifval ) return 0;
            return ( mype1->ifval < mype2->ifval ? -1 : 1 );
        }
        else {
            return ( mype1->iffxyn < mype2->iffxyn ? -1 : 1 );
        }
}
