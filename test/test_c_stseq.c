/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests the stseq() function.
 *
 * Ed Hartnett 4/27/23 */

#include <stdio.h>
#include "bufr_interface.h"
#include "bufrlib.h"
#include "cread.h"

#define IN_9 "testfiles/IN_9"
#define TEST_ERR 1
#define MSG_SIZE 14928 /* in bytes */
#define MSG_SIZE_INT ( MSG_SIZE / sizeof(int) ) /* in ints */

/* Prototype. */
void stseq(f77int *lun, f77int *irepct, f77int *idn, char *nemo,
           char *cseq, f77int *cdesc, f77int *ncdesc);

/* Test the stseq() function. */
int main()
{
    f77int lun = 1, irepct = 0, idn = 42;
    char nemo[] = "d";
    char cseq[] = "d";
    f77int cdesc[1], ncdesc = 1;
    int *msg;
    
    printf("Testing the stseq() function.\n");

    /* Allocate internal arrays. */
    /* arallocc(); */

    /* Allocate storage to read a BUFR message into. */
    if (!(msg = malloc(MSG_SIZE)))
        return TEST_ERR;

    /* Perform library allocations. */
    openbf_f(11, "QUIET", 0);
    
    /* Open a real BUFR file, read a BUFR message, and close the file. */
    openrb(0, IN_9);
    if (!pb[0])
        return TEST_ERR;
    if (crdbufr(0, msg, MSG_SIZE_INT))
        return TEST_ERR;

    stseq(&lun, &irepct, &idn, nemo, cseq, cdesc, &ncdesc);

    /* Close the file. */
    closfb(0);
    
    /* Free memory. */
    free(msg);

    printf("SUCCESS!!\n");
    
    return 0;
}
