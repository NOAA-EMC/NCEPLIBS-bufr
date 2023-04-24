/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests misc C API stuff.
 *
 * Ed Hartnett 3/25/23 */

#include <stdio.h>
#include "bufrlib.h"
#include "cread.h"

#define FILENAME "test_c_cread.data"
#define BAD_FILENAME "BAD_test_c_cread.data"
#define IN_9 "testfiles/IN_9"
#define TEST_ERR 1
#define MSG_SIZE 14928 /* in bytes */

void arallocc_(void);

int main()
{
    int *msg, *msg2;
    int old_val, i;
    
    printf("Testing cread functions.\n");

    /* Allocate internal arrays. */
    arallocc();

    /* Create a file. */
    openwb(0, FILENAME);
    if (!pb[0])
        return TEST_ERR;
    closfb(0);

    /* Reopen the (empty) file for reading. */
    openrb(0, FILENAME);
    if (!pb[0])
        return TEST_ERR;
    closfb(0);

    /* Reopen the (empty) file for appending. */
    openab(0, FILENAME);
    if (!pb[0])
        return TEST_ERR;
    closfb(0);

    /* Allocate storage to read a BUFR message into. Without the +8
     * these buffers will be overflowed by the cdrbufr(). See
     * https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/450. */
    if (!(msg = malloc(MSG_SIZE + 8)))
        return TEST_ERR;
    if (!(msg2 = malloc(MSG_SIZE + 8)))
        return TEST_ERR;
        
    /* Open a real BUFR file, read a BUFR message, and close the file. */
    openrb(0, IN_9);
    if (!pb[0])
        return TEST_ERR;
    if (crdbufr(0, msg, MSG_SIZE/sizeof(int) + 1))
        return TEST_ERR;
    closfb(0);

    /* Reopen the (empty) file for appending. */
    openab(0, FILENAME);
    if (!pb[0])
        return TEST_ERR;

    /* Write the message to the empty file. Note that the third
     * argument is the number of elements in the msg array. */
    cwrbufr(0, msg, MSG_SIZE/sizeof(int));
    
    /* Close the file. */
    closfb(0);
    
    /* Open the just-written BUFR file, read the BUFR message, and close the file. */
    openrb(0, IN_9);
    if (!pb[0])
        return TEST_ERR;
    if (crdbufr(0, msg2, MSG_SIZE))
        return TEST_ERR;
    closfb(0);

    /* Messages are the same. */
    for (i = 0; i < MSG_SIZE / 4; i++)
        if (msg[i] != msg2[i])
            return TEST_ERR;

    /* Open a file to write a bad message to. Write a message with the
     * last '7777' missing. */
    openwb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    cwrbufr(0, msg, MSG_SIZE/sizeof(int) - 1);
    closfb(0);

    /* Try to read the bad file. */
    openrb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    if (crdbufr(0, msg2, MSG_SIZE) != -2)
        return TEST_ERR;
    closfb(0);
    
    /* Open a file to write a bad message to. Write a message with the
     * last '7777' changed. */
    openwb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    old_val = msg[MSG_SIZE/sizeof(int) - 1];
    msg[MSG_SIZE/sizeof(int) - 1] = 0;
    cwrbufr(0, msg, MSG_SIZE/sizeof(int));
    msg[MSG_SIZE/sizeof(int) - 1] = old_val;
    closfb(0);

    /* Try to read the bad file. */
    openrb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    if (crdbufr(0, msg2, MSG_SIZE) != -2)
        return TEST_ERR;
    closfb(0);
    
    /* Open a file to write a bad message to. Write a message with the
     * inital 'BUFR' changed. */
    openwb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    old_val = msg[0];
    msg[0] = 0;
    cwrbufr(0, msg, MSG_SIZE/sizeof(int));
    msg[0] = old_val;
    closfb(0);

    /* Try to read the bad file. */
    openrb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    if (crdbufr(0, msg2, MSG_SIZE) != -1)
        return TEST_ERR;
    closfb(0);
    
    /* Open a file to write a bad message to. Write a message with
     * only the inital 'BUFR'. */
    openwb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    cwrbufr(0, msg, 1);
    closfb(0);

    /* Try to read the bad file. */
    openrb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    if (crdbufr(0, msg2, MSG_SIZE) != -1)
        return TEST_ERR;
    closfb(0);
    
    /* Open a file to write a bad message to. Change the size of the
     * message. */
    openwb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    old_val = msg[1];
    msg[1] = MSG_SIZE + 10;
    cwrbufr(0, msg, 100);
    msg[1] = old_val;
    closfb(0);

    /* Try to read the bad file. */
    openrb(0, BAD_FILENAME);
    if (!pb[0])
        return TEST_ERR;
    if (crdbufr(0, msg2, MSG_SIZE) != -3)
        return TEST_ERR;
    closfb(0);
    
    /* Free memory. */
    free(msg);
    free(msg2);

    printf("SUCCESS!!!\n");
    return 0;
}
