/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests misc C API stuff.
 *
 * Ed Hartnett 3/25/23 */

#include <stdio.h>
#include "bufr_interface.h"
#include "bufrlib.h"

#define TESTFILE_IN1 "testfiles/IN_1"

int rbytes(char *bmg, int mxmb, int isloc, int newbytes);

/* Test various C things. */
int main()
{
    char bmg[200];
    int nmb, iret;
    /* f77int lun = 1, tddesc = 11, nctddesc = 10, ctddesc[10]; */

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

    /* Perform library allocations. */
    openbf_f(11, "QUIET", 0);
    
    /* Test restd(). */
    /* restd(&lun, &tddesc, &nctddesc, ctddesc); */
    
    printf("SUCCESS!!\n");
    return 0;
}
