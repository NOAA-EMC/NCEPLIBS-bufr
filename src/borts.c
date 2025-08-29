/** @file
 *  @brief C language interface for returning to an application program in the
 *  event of a bort error.
 *
 *  @authors J. Ator, D. O'Connor
 *  @date 2025-08-20
 */

#include "bufrlib.h"
#include <setjmp.h>

/** Context information for returning to an application program. */
jmp_buf context;

/**
 *  Return to the previously-set target location after a bort error.
 *
 *  @author J. Ator @date 2025-08-20
*/
void
bort_goto_target(void)
{
    longjmp(context, 1);
}

/**
 * Catch any bort error inside of subroutine readmg().
 *
 * @param lunxx - Absolute value is Fortran logical unit number for BUFR file
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 * @param iret - return code:
 *   - 0 = new BUFR message was successfully read into internal arrays
 *   - -1 = there are no more messages in the BUFR file
 *
 * @author J. Ator @date 2025-08-20
*/
void
catch_bort_readmg(int lunxx, char* subset, int* jdate, int subset_str_len, int* iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    readmg_f(lunxx, subset, jdate, subset_str_len, iret);
}
