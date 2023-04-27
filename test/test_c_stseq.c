/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests the stseq() function.
 *
 * Ed Hartnett 4/27/23 */

#include <stdio.h>
#include "bufr_interface.h"
#include "bufrlib.h"

/* Prototype. */
void stseq(f77int *lun, f77int *irepct, f77int *idn, char *nemo,
           char *cseq, f77int *cdesc, f77int *ncdesc);

/* Test the stseq() function. */
int main()
{
    f77int lun = 0, irepct = 0, idn = 0;
    char nemo[] = "d";
    char cseq[] = "d";
    f77int cdesc[1], ncdesc = 1;
    
    printf("Testing the stseq() function.\n");

    stseq(&lun, &irepct, &idn, nemo, cseq, cdesc, &ncdesc);
    printf("SUCCESS!!\n");
    
    return 0;
}
