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
 * Set target location for returning to an application program in the event of a bort error.
 *
 * @returns bort_set_target:
 * - 0 = the target location was set during this call
 * - 1 = the previously-set target location was returned to during this call, following a bort error
 * that occurred elsewhere within the library
 *
 * @author J. Ator @date 2025-08-20
*/
int
bort_set_target(void)
{
    return setjmp(context);
}

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
