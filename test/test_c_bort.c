/* This is a test for NCEPLIBS-bufr.
 *
 * This file tests bort calls from C code. This test is expected to
 * activate a bort() and return with a non-zero exit code. So returning
 * with a 0 error code causes the test to fail.
 *
 * This program is called from the test script run_test_bort.sh. The
 * test script calls this program like this:
 *
 * test_c_bort cobfl 1
 * 
 * The first parameter is the name of the C function to test. The
 * second parameter is the test case number.
 *
 * Ed Hartnett 4/9/23
 */

#include <stdio.h>
#include <string.h>
#include "bufr_interface.h"

#define TOO_LONG_FILENAME "01234567890012345678900123456789001234567890012345678900123456789001234567890012345678900123456789001234567890012345678900123456789001234567890012345678900123456789001234567890012345678900123456789001234567890012345678901"
#define TESTFILE_IN1 "testfiles/IN_1"
#define BAD_TESTFILE "does_not_exist"

int main(int argc, char **argv)
{
    char bmg[200];
    int nmb, iret;

    if (argc != 3)
        return 0;

    if (!strcmp(argv[1], "cobfl"))
    {
        if (!strcmp(argv[2], "1"))
            cobfl(TOO_LONG_FILENAME, 'r');
        else if (!strcmp(argv[2], "2"))
            cobfl(TESTFILE_IN1, 'x');
        else if (!strcmp(argv[2], "3"))
            cobfl(BAD_TESTFILE, 'r');
    }
    else if (!strcmp(argv[1], "crbmg"))
    {
        if (!strcmp(argv[2], "1"))
            crbmg(bmg, 1, &nmb, &iret);
    }
    else if (!strcmp(argv[1], "cwbmg"))
    {
        if (!strcmp(argv[2], "1"))
            cwbmg(bmg, 1, &iret);
    }

    return 0;
}
