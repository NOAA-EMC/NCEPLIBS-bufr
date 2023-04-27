/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests misc C API stuff.
 *
 * Ed Hartnett 3/25/23 */

#include <stdio.h>
#include "bufr_interface.h"

#define TESTFILE_IN1 "testfiles/IN_1"

int rbytes(char *bmg, int mxmb, int isloc, int newbytes);

/* Test various C things. */
int main()
{
    char bmg[200];
    int nmb, iret;

    printf("Testing misc C functions.\n");
    
    /* Test rbytes error condition. */
    if (rbytes(NULL, 0, 1, 1) != 1)
        return 1;

    /* Test crbmg error condition. */
    cobfl(TESTFILE_IN1, 'r');
    crbmg(bmg, 1, &nmb, &iret);
    if (iret != 1)
        return 1;
    ccbfl();
    
    printf("SUCCESS!!\n");
    return 0;
}
