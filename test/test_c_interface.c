/** @file
    @author Ronald Mclaren
    @date 2021-02-26

    @brief This file is a quick test of bufr_c_interface_mod.
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bufr.interface.h"


static const int MAX_SUBSETS = 100;
static const int BUFR_FILE_UNIT = 12;
static const int DUMP_FILE_UNIT = 13;
static const int TABLE_1_FILE_UNIT = 14;
static const int TABLE_2_FILE_UNIT = 15;
static const int SUBSET_STRING_LEN = 8;

static const char* INPUT_FILE = "testfiles/data/1bamua";
static const char* TABLE_DUMP_FILE = "testrun/1bamua.c.table";


// Supporting functions

unsigned int countSubsets(const char* subset)
{
    open_f(BUFR_FILE_UNIT, INPUT_FILE);
    openbf_f(BUFR_FILE_UNIT, "IN", BUFR_FILE_UNIT);

    int iddate;
    char msg_subset[SUBSET_STRING_LEN];
    unsigned int subset_cnt = 0;

    while (ireadmg_f(BUFR_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN) == 0)
    {
        if (strcmp(subset, msg_subset))
        {
            while (ireadsb_f(BUFR_FILE_UNIT) == 0 && (subset_cnt < MAX_SUBSETS))
            {
                subset_cnt++;
            }
        }
    }

    closbf_f(BUFR_FILE_UNIT);
    close_f(BUFR_FILE_UNIT);

    return subset_cnt;
}


// Tests

void test_readBufrFile()
{
    const char* subset = "NC021053";
    const char* mnemonic = "CLAT";

    int iret;
    int iddate;
    char msg_subset[SUBSET_STRING_LEN];
    unsigned int subset_cnt = countSubsets(subset);

    open_f(BUFR_FILE_UNIT, INPUT_FILE);
    openbf_f(BUFR_FILE_UNIT, "IN", BUFR_FILE_UNIT);

    double data_int = 0;
    double data_rep = 0;
    double* data_int_ptr = &data_int;
    double* data_rep_ptr = &data_rep;
    double data_int_buf[subset_cnt];
    double data_rep_buf[subset_cnt];

    int subset_idx = 0;
    while (ireadmg_f(BUFR_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN) == 0)
    {
        if (strcmp(subset, msg_subset))
        {
            while ((ireadsb_f(BUFR_FILE_UNIT) == 0) && (subset_idx < MAX_SUBSETS))
            {
                ufbint_f(BUFR_FILE_UNIT, (void**) &data_int_ptr, 1, 1, &iret, mnemonic);
                ufbrep_f(BUFR_FILE_UNIT, (void**) &data_rep_ptr, 1, 1, &iret, mnemonic);
                data_int_buf[subset_idx] = data_int;
                data_rep_buf[subset_idx] = data_rep;
                subset_idx++;
            }
        }
    }

    // Check a few values, and make sure they are good.
    double check_vals[] = {45.818, 45.935, 46.046, 46.152};
    for (unsigned int idx = 0; idx < 4; idx++)
    {
        if (fabs(data_int_buf[idx] - check_vals[idx]) > 0.001)
        {
            printf("%s", "Got bad data from BUFR file!");
            exit(1);
        }
    }

    // Check consistency between data read from ufbint and ufbrep
    for (unsigned int idx = 0; idx < subset_cnt; idx++)
    {
        if (data_int_buf[idx] != data_rep_buf[idx])
        {
            printf("%s", "Data from ufbint didn't match ufbrep!");
            exit(1);
        }
    }

    closbf_f(BUFR_FILE_UNIT);
    close_f(BUFR_FILE_UNIT);
}


void test_dxDump()
{
    open_f(BUFR_FILE_UNIT, INPUT_FILE);
    open_f(DUMP_FILE_UNIT, TABLE_DUMP_FILE);
    openbf_f(BUFR_FILE_UNIT, "IN", BUFR_FILE_UNIT);

    dxdump_f(BUFR_FILE_UNIT, DUMP_FILE_UNIT);

    closbf_f(BUFR_FILE_UNIT);
    close_f(DUMP_FILE_UNIT);
    close_f(BUFR_FILE_UNIT);
}


void test_mtInfo()
{
    open_f(BUFR_FILE_UNIT, INPUT_FILE);
    openbf_f(BUFR_FILE_UNIT, "IN", BUFR_FILE_UNIT);

    mtinfo_f("testfiles", TABLE_1_FILE_UNIT, TABLE_2_FILE_UNIT);

    exitbufr_f();
}


int main()
{
    test_readBufrFile();
    test_dxDump();
    test_mtInfo();

    return 0;
}
