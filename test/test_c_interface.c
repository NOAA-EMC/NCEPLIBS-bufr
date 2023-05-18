/* This is a test for NCEPLIBS-bufr.
   This file is a quick test of bufr_c2f_interface_mod.
   Ronald Mclaren 2021-02-26
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "bufr_interface.h"


static const int MAX_SUBSETS = 100;
static const int BUFR_FILE_UNIT = 12;
static const int SUBSET_STRING_LEN = 10;

static const char* INPUT_FILE = "testfiles/data/1bamua";


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
        if (strncmp(subset, msg_subset,8))
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

void test_basicInterface()
{
    const char* subset = "NC021053";
    const char* mnemonic = "CLAT";

    int iret;
    int iddate;
    char msg_subset[SUBSET_STRING_LEN];
    unsigned int subset_cnt = countSubsets(subset);
    unsigned int idx;

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
        if (strncmp(subset, msg_subset, 8))
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
    for (idx = 0; idx < 4; idx++)
    {
        if (fabs(data_int_buf[idx] - check_vals[idx]) > 0.001)
        {
            printf("%s", "Got bad data from BUFR file!");
            exit(1);
        }
    }

    // Check consistency between data read from ufbint and ufbrep
    for (idx = 0; idx < subset_cnt; idx++)
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


int cIdx(int fortranIdx)
{
    return fortranIdx - 1;
}


const char* tag(char* tagPtr, int nodeIdx)
{
    return tagPtr + cIdx(nodeIdx) * 10;
}


const char* typ(char* typPtr, int nodeIdx)
{
    return typPtr + cIdx(nodeIdx) * 3;
}


void test_intrusiveInterface()
{
    const char* Subset = "NC021023";
    const int NumSubsetsToParse = 10;
    int i;

    typedef struct
    {
        unsigned int nodeIdx;
        unsigned int seqNodeIdx;
        bool isString;
        char mnemonic[4];
        char nextNode[4];
        char seqNode[8];
        char repNode[10];
        char typ[3];
        unsigned int numReps;
        double** data;
    } Target;

    Target target;
    target.nodeIdx = 65;
    target.seqNodeIdx = 0;
    memcpy(target.mnemonic, "TMBR", 4);
    memcpy(target.nextNode, "CSTC", 4);
    memcpy(target.seqNode, "BRITCSTC", 8);
    memcpy(target.repNode, "\"BRITCSTC\"", 10);
    memcpy(target.typ, "NUM", 3);
    target.isString = false;
    target.numReps = 15;

    target.data = malloc(sizeof(double*) * NumSubsetsToParse);

    for (i = 0; i < NumSubsetsToParse; i++)
    {
        target.data[i] = malloc(sizeof(double) * target.numReps);
    }

    int iddate;
    char msg_subset[11];

    // BUFR table parameters
    int* iscPtr = NULL;
    int iscSize = 0;
    int* linkPtr = NULL;
    int linkSize = 0;
    int* itpPtr = NULL;
    int itpSize = 0;
    char* typPtr = NULL;
    int typSize = 0;
    int typStrLen = 0;
    char* tagPtr = NULL;
    int tagSize = 0;
    int tagStrLen = 0;
    int* jmpbPtr = NULL;
    int jmpbSize = 0;
    int* irfPtr = NULL;
    int irfSize = 0;


    int bufrLoc;
    int il, im; // throw away

    open_f(BUFR_FILE_UNIT, INPUT_FILE);
    openbf_f(BUFR_FILE_UNIT, "IN", BUFR_FILE_UNIT);

    int subsetIdx = 0;
    while (ireadmg_f(BUFR_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN) == 0)
    {
        if (strncmp(Subset, msg_subset, 8) == 0)
        {
            while ((ireadsb_f(BUFR_FILE_UNIT) == 0) && (subsetIdx < NumSubsetsToParse))
            {
                status_f(BUFR_FILE_UNIT, &bufrLoc, &il, &im);

                // Get the table data. Must be read here because otherwise you get invalid data for WMO BUFR files.
                if (!iscPtr)
                {
                    get_isc_f(&iscPtr, &iscSize);
                    get_link_f(&linkPtr, &linkSize);
                    get_itp_f(&itpPtr, &itpSize);
                    get_typ_f(&typPtr, &typStrLen, &typSize);
                    get_tag_f(&tagPtr, &tagStrLen, &tagSize);
                    get_jmpb_f(&jmpbPtr, &jmpbSize);
                    get_irf_f(&irfPtr, &irfSize);

                    int startNode;
                    get_inode_f(bufrLoc, &startNode);
                    int numNodes = iscPtr[cIdx(startNode)];

                    if (numNodes != 66)
                    {
                        printf("%s", "Incorrect number of nodes in the BUFR table!");
                        exit(1);
                    }
                }

                // BUFR data parameters
                int nval = 0;
                double *dataPtr = NULL;
                int dataSize = 0;
                int *invPtr = NULL;
                int invSize = 0;

                get_nval_f(bufrLoc, &nval);
                get_val_f(bufrLoc, &dataPtr, &dataSize);
                get_inv_f(bufrLoc, &invPtr, &invSize);

                int repIdx = 0;
                int dataCursor;
                for (dataCursor = 1; dataCursor <= nval; dataCursor++)
                {
                    int nodeIdx = invPtr[cIdx(dataCursor)];

                    if (strncmp(tag(tagPtr, nodeIdx), target.repNode, 10) == 0 &&
                        irfPtr[cIdx(nodeIdx)] != target.numReps)
                    {
                        printf("%s", "Incorrect number of repetitions for fixed replication.");
                        exit(1);
                    }

                    if (target.nodeIdx == nodeIdx)
                    {
                        if (strncmp(tag(tagPtr, nodeIdx), target.mnemonic, 4) != 0)
                        {
                            printf("%s", "Mnemonic didn't match the target.");
                            exit(1);
                        }

                        if (strncmp(typ(typPtr, nodeIdx), target.typ, 3) != 0)
                        {
                            printf("%s", "Type didn't match the target.");
                            exit(1);
                        }

                        if (repIdx >= target.numReps)
                        {
                            printf("%s", "Too many repetitions.");
                            exit(1);
                        }

                        if (strncmp(tag(tagPtr, linkPtr[cIdx(nodeIdx)]), target.nextNode, 4) != 0)
                        {
                            printf("%s", "Next node didn't match the target.");
                            exit(1);
                        }

                        if (strncmp(tag(tagPtr, jmpbPtr[cIdx(nodeIdx)]), target.seqNode, 8) != 0)
                        {
                            printf("%s", "SeqNode didn't match the target.");
                            exit(1);
                        }

                        if (itpPtr[cIdx(nodeIdx)] != (target.isString ? 3 : 2))
                        {
                            printf("%s", "Is string didn't match the target.");
                            exit(1);
                        }

                        target.data[subsetIdx][repIdx] = dataPtr[cIdx(dataCursor)];
                        repIdx++;
                    }
                }

                if (repIdx != target.numReps)
                {
                    printf("%s", "Too few repetitions found.");
                    exit(1);
                }

                subsetIdx++;
            }
        }
        else
        {
            break;
        }
    }

    if (fabs(target.data[0][0] - 150.53) > 0.001 ||
        fabs(target.data[0][1] - 154.12) > 0.001 ||
        fabs(target.data[1][0] - 148.83) > 0.001 ||
        fabs(target.data[2][2] - 215.75) > 0.001  ||
        fabs(target.data[5][10] - 226.45) > 0.001 ||
        fabs(target.data[9][14] - 239.16) > 0.001)
    {
        printf("%s", "Invalid values in output data.");
        exit(1);
    }

    // Memory cleanup
    for (i = 0; i < NumSubsetsToParse; i++)
    {
        free(target.data[i]);
    }
    free(target.data);

    delete_table_data_f();

    closbf_f(BUFR_FILE_UNIT);
    close_f(BUFR_FILE_UNIT);
}


void test_getTypeInfo()
{
    static const int UNIT_STR_LEN = 20;
    static const int DESC_STR_LEN = 54;

    char msg_subset[SUBSET_STRING_LEN];

    int bufrLoc, iddate, il, im;

    open_f(BUFR_FILE_UNIT, INPUT_FILE);
    openbf_f(BUFR_FILE_UNIT, "IN", BUFR_FILE_UNIT);

    status_f(BUFR_FILE_UNIT, &bufrLoc, &il, &im);

    int iret;
    int scale;
    int reference;
    int bits;
    char unit[UNIT_STR_LEN];
    char desc[DESC_STR_LEN];

    if (ireadns_f(BUFR_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN) != 0)
    {
        printf("%s", "Call of ireadns_f failed.");
        exit(1);
    }

    nemdefs_f(BUFR_FILE_UNIT,
              "CLAT",
              unit,
              UNIT_STR_LEN,
              desc,
              DESC_STR_LEN,
              &iret);

    if (iret != 0)
    {
        printf("%s", "nemdefs_f: Call of nemdefs_f failed.");
        exit(1);
    }
    if (strncmp(unit, "DEGREE", 6) != 0)
    {
        printf("%s", "nemdefs_f: Wrong Unit String.");
        exit(1);
    }
    if (strncmp(desc, "LATITUDE (COARSE ACCURACY)", 26) != 0)
    {
        printf("%s", "nemdefs_f: Wrong Description String.");
        exit(1);
    }

    nemspecs_f(BUFR_FILE_UNIT,
               "CLAT",
               1,
               &scale,
               &reference,
               &bits,
               &iret);

    if (iret != 0)
    {
        printf("%s", "nemspecs_f: Call of nemspecs_f failed.");
        exit(1);
    }
    if (reference != -900000)
    {
        printf("%s", "nemspecs_f: Wrong reference number found.");
        exit(1);
    }
    if (scale != 4)
    {
        printf("%s", "nemspecs_f: Wrong scale number found.");
        exit(1);
    }
    if (bits != 22)
    {
        printf("%s", "nemspecs_f: Wrong number of bits found.");
        exit(1);
    }

    int descriptor;
    int table_idx;
    char table_type;

    nemtab_f(bufrLoc,
             "CLAT",
             &descriptor,
             &table_type,
             &table_idx);

    if (descriptor != 1282)
    {
        printf("%s", "nemtab_f: Wrong descriptor found.");
        exit(1);
    }
    if (table_type != 'B')
    {
        printf("%s", "nemtab_f: Table type wrong");
        exit(1);
    }
    if (table_idx != 41)
    {
        printf("%s", "nemtab_f: Table Idx is wrong.");
        exit(1);
    }

    nemtbb_f(bufrLoc,
             table_idx,
             unit,
             UNIT_STR_LEN,
             &scale,
             &reference,
             &bits);

    if (reference != -9000)
    {
        printf("%s", "nemtbb_f: Wrong reference number found.");
        exit(1);
    }
    if (scale != 2)
    {
        printf("%s", "nemtbb_f: Wrong scale number found.");
        exit(1);
    }
    if (bits != 15)
    {
        printf("%s", "nemtbb_f: Wrong number of bits found.");
        exit(1);
    }
    if (strncmp(unit, "DEGREE", 6) != 0)
    {
        printf("%s", "nemtbb_f: Wrong Unit String.");
        exit(1);
    }

    closbf_f(BUFR_FILE_UNIT);
    close_f(BUFR_FILE_UNIT);
}


int main()
{
    test_basicInterface();
    test_intrusiveInterface();
    test_getTypeInfo();

    return 0;
}
