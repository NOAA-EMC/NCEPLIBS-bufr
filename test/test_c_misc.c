/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests misc C API stuff.
 *
 * Ed Hartnett 3/25/23 */

#include <stdio.h>
#include "bufr_interface.h"

int rbytes(char *bmg, int mxmb, int isloc, int newbytes);

/* Test various C things. */
int main()
{
    
    /* Test rbytes error condition. */
    if (rbytes(NULL, 0, 1, 1) != 1)
        return 1;
    return 0;
}
