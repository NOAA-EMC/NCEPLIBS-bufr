/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests crwbmg.c.
 *
 * Ed Hartnett 4/16/23 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bufr_interface.h"

#define FILE_NAME "testfiles/IN_4"
#define DATA_SIZE 4
#define BIG_DATA_SIZE 3582
#define TEST_ERR 1

/* Prototpyes. */
int rbytes(char *bmg, int mxmb, int isloc, int newbytes);

int main()
{
    char data[DATA_SIZE];
    char *data2;
    
    printf("Testing crwbmg.c.\n");

    /* Open a file. */
    cobfl(FILE_NAME, 'r');

    /* Try to read 4 bytes, but fail. */
    if (rbytes(data, 3, 0, DATA_SIZE) != 1)
        return TEST_ERR;

    /* Read 4 bytes. */
    if (rbytes(data, DATA_SIZE, 0, DATA_SIZE))
        return TEST_ERR;
    if (strncmp(data, "BUFR", DATA_SIZE))
        return TEST_ERR;

    /* Allocate storage. */
    if (!(data2 = malloc(BIG_DATA_SIZE * sizeof(char))))
        return TEST_ERR;

    /* Read almost the entire rest of the file. */
    if (rbytes(data2, BIG_DATA_SIZE, 0, BIG_DATA_SIZE))
        return TEST_ERR;

    /* Try to read 4 bytes, but there are not 4 bytes left in the
     * file. */
    if (rbytes(data, DATA_SIZE, 0, DATA_SIZE) != -1)
        return TEST_ERR;

    /* Close the file. */
    ccbfl();

    /* Free storage. */
    free(data2);
    
    printf("SUCCESS!\n");
    return 0;
}

