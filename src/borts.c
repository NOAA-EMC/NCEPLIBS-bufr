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
 * Catch any bort error inside of subroutine openbf().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cio - Flag indicating how lunit is to be used by the software
 * @param lundx - Fortran logical unit number containing DX BUFR table information
 * @param cio_str_len - Length of cio
 *
 * @author J. Ator @date 2025-09-05
*/
void
catch_bort_openbf(int lunit, char *cio, int lundx, int cio_str_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cio, for use with c_f_string inside of openbf_f. */
    cio[cio_str_len] = '\0';

    /* Recursively call the subroutine. */
    openbf_f(lunit, cio, lundx);
}

/**
 * Catch any bort error inside of subroutine closbf().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 *
 * @author J. Ator @date 2025-09-05
*/
void
catch_bort_closbf(int lunit)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    closbf_f(lunit);
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
catch_bort_readmg(int lunxx, char *subset, int *jdate, int subset_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    readmg_f(lunxx, subset, jdate, subset_str_len, iret);
}

/**
 * Catch any bort error inside of subroutine readns().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 * @param iret - return code:
 *   - 0 = new BUFR data subset was successfully read into internal arrays
 *   - -1 = there are no more data subsets in the BUFR file
 *
 * @author J. Ator @date 2025-09-05
*/
void
catch_bort_readns(int lunit, char *subset, int *jdate, int subset_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    readns_f(lunit, subset, jdate, subset_str_len, iret);
}

/**
 * Catch any bort error inside of subroutine readsb().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param iret - return code:
 *   - 0 = new BUFR data subset was successfully read into internal arrays
 *   - -1 = there are no more data subsets in the BUFR file
 *
 * @author J. Ator @date 2025-09-05
*/
void
catch_bort_readsb(int lunit, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    readsb_f(lunit, iret);
}
