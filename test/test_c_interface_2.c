/* This is a test for NCEPLIBS-bufr.
 *
 * Test some of the bufr_c2f_interface.mod functions.
 *
 * J. Ator 2021-03-01
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bufr_interface.h"

static const int BUFR_INPUT_FILE_UNIT = 11;
static const int BUFR_OUTPUT_FILE_UNIT = 51;
static const int TABLE_1_FILE_UNIT = 90;
static const int TABLE_2_FILE_UNIT = 91;
static const int SUBSET_STRING_LEN = 12;

static const char* INPUT_FILE = "testfiles/IN_4";
static const char* OUTPUT_FILE = "testfiles/test_c_interface_2.out";

int main() {

    int jj, lun, il, im;
    int iret;
    int iddate;
    char msg_subset[SUBSET_STRING_LEN];

    double r8arr[180][15];
    double* r8arr_ptr = &r8arr[0][0];

    /* Assign Fortran logical unit numbers to the input and output files. */
    open_f( BUFR_INPUT_FILE_UNIT, INPUT_FILE );
    open_f( BUFR_OUTPUT_FILE_UNIT, OUTPUT_FILE );

    /* Set and confirm a global library parameter. */
    if ( ( iret = isetprm_f( "NFILES" ,5 ) ) != 0 ) exit(1);
    if ( ( iret = igetprm_f( "NFILES" ) ) != 5 ) exit(1);

    /* Open the input file to the library. */
    openbf_f( BUFR_INPUT_FILE_UNIT, "SEC3", BUFR_INPUT_FILE_UNIT );

    /* Set and confirm the maximum output message size. */
    maxout_f( 25000 );
    if ( ( iret = igetmxby_f() ) != 25000 ) exit(1);

    /* Define the location of the master BUFR tables. */
    mtinfo_f( "../tables", TABLE_1_FILE_UNIT, TABLE_2_FILE_UNIT );

    /* Read a BUFR message from the input file. */
    if ( ( ireadmg_f( BUFR_INPUT_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN ) != 0 ) ||
         ( strncmp( msg_subset, "MSTTB001", 8) != 0 ) || ( iddate != 16041815 ) ) {
        printf( "%s\n", "ireadmg check FAILED!" );
        exit(1);
    }

    /* Read the first data subset from the BUFR message and check some values. */
    if ( ireadsb_f( BUFR_INPUT_FILE_UNIT ) != 0 ) {
        printf( "%s\n", "ireadsb check FAILED!" );
        exit(1);
    }
    ufbint_f( BUFR_INPUT_FILE_UNIT, (void**) &r8arr_ptr, 15, 180, &iret, "CLONH SAID SAZA" );
    if ( ( ( (int) round( r8arr[0][0] * 100000 ) ) != -4246453 ) ||
         ( ( (int) round( r8arr[0][1] ) ) != 57 ) ||
         ( ( (int) round( r8arr[0][2] * 100 ) ) != 5407 ) ) {
        printf( "%s\n", "ufbint check FAILED!" );
        exit(1);
    }
    ufbrep_f( BUFR_INPUT_FILE_UNIT, (void**) &r8arr_ptr, 15, 180, &iret, "PCCF" );
    if ( ( ( (int) round( r8arr[11][0] ) ) != 86 ) ||
         ( ( (int) round( r8arr[14][0] ) ) != 38 ) ||
         ( ( (int) round( r8arr[101][0] ) ) != 88 ) ||
         ( ( (int) round( r8arr[140][0] ) ) != 10 ) ) {
        printf( "%s\n", "ufbrep check FAILED!" );
        exit(1);
    }

    /* Read the second data subset from the BUFR message and check some values. */
    if ( ( ireadns_f( BUFR_INPUT_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN ) != 0 ) ||
         ( strncmp( msg_subset, "MSTTB001", 8) != 0 ) || ( iddate != 2016041815 ) ) {
        printf( "%s\n", "ireadns check FAILED!" );
        exit(1);
    }
    ufbseq_f( BUFR_INPUT_FILE_UNIT, (void**) &r8arr_ptr, 15, 180, &iret, "SIDENSEQ" );
    if ( ( ( (int) round( r8arr[0][11] * 100000 ) ) != -2355492 ) ||
         ( ( (int) round( r8arr[0][12] * 100000 ) ) != -4324478 ) ) {
        printf( "%s\n", "ufbseq check FAILED!" );
        exit(1);
    }
    for ( jj = 0; jj < 11; jj++ ) {
        if ( ibfms_f( r8arr[0][jj] ) ) {  /* none of these values should be "missing" */
            printf( "%s\n", "ibfms check FAILED!" );
            exit(1);
        }
    }

    /* Open the output file to the library, using the same DX table information in the input file. */
    openbf_f( BUFR_OUTPUT_FILE_UNIT, "NODX", BUFR_INPUT_FILE_UNIT );

    /* Disconnect the input file from the library and close the logical unit. */
    closbf_f( BUFR_INPUT_FILE_UNIT );
    close_f( BUFR_INPUT_FILE_UNIT );

    /* Open a new BUFR message for output, and check that it was successful. */
    openmb_f( BUFR_OUTPUT_FILE_UNIT, "MSTTB001", 2023051015 );
    status_f( BUFR_OUTPUT_FILE_UNIT, &lun, &il, &im );
    if ( il != 1 || im != 1 ) {
        printf( "%s\n", "status check FAILED!" );
        exit(1);
    }

    /* Disconnect the output file from the library and close the logical unit. */
    closbf_f( BUFR_OUTPUT_FILE_UNIT );
    close_f( BUFR_OUTPUT_FILE_UNIT );
}
