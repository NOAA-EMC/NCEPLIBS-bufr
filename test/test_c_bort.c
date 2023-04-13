/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests bort calls from C code. This test is expected to
 * active a bort() and return with a non-zero exit code.
 *
 * Ed Hartnett 4/9/23
 */

#include <stdio.h>
#include "bufrlib.h"

void wrdesc(f77int desc, f77int *descary, f77int *ndescary);

/* Test C borts. */
int main()
{
    f77int descary, ndescary = 100000;
    
    /* Test wrdesc(). */
    wrdesc(1, &descary, &ndescary);

    return 0;
}
