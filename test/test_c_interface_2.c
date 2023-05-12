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

static const int BUFR_FILE_UNIT = 11;
static const int TABLE_1_FILE_UNIT = 90;
static const int TABLE_2_FILE_UNIT = 91;
static const int SUBSET_STRING_LEN = 12;

static const char* INPUT_FILE = "testfiles/IN_4";

int main() {

    int jj;
    int iret;
    int iddate;
    char msg_subset[SUBSET_STRING_LEN];

    double r8arr[180][15];
    double* r8arr_ptr = &r8arr[0][0];

    open_f( BUFR_FILE_UNIT, INPUT_FILE );

    if ( ( iret = isetprm_f( "NFILES" ,5 ) ) != 0 ) exit(1);
    if ( ( iret = igetprm_f( "NFILES" ) ) != 5 ) exit(1);

    openbf_f( BUFR_FILE_UNIT, "SEC3", BUFR_FILE_UNIT );

    maxout_f( 25000 );
    if ( ( iret = igetmxby_f() ) != 25000 ) exit(1);

    mtinfo_f( "../tables", TABLE_1_FILE_UNIT, TABLE_2_FILE_UNIT );

    if ( ( ireadmg_f( BUFR_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN ) != 0 ) ||
         ( strncmp( msg_subset, "MSTTB001", 8) != 0 ) || ( iddate != 16041815 ) ) {
        printf( "%s\n", "ireadmg check FAILED!" );
        exit(1);
    }

    if ( ireadsb_f( BUFR_FILE_UNIT ) != 0 ) {
        printf( "%s\n", "ireadsb check FAILED!" );
        exit(1);
    }

    ufbint_f( BUFR_FILE_UNIT, (void**) &r8arr_ptr, 15, 180, &iret, "CLONH SAID SAZA" );
    if ( ( ( (int) round( r8arr[0][0] * 100000 ) ) != -4246453 ) ||
         ( ( (int) round( r8arr[0][1] ) ) != 57 ) ||
         ( ( (int) round( r8arr[0][2] * 100 ) ) != 5407 ) ) {
        printf( "%s\n", "ufbint check FAILED!" );
        exit(1);
    }
    ufbrep_f( BUFR_FILE_UNIT, (void**) &r8arr_ptr, 15, 180, &iret, "PCCF" );
    if ( ( ( (int) round( r8arr[11][0] ) ) != 86 ) ||
         ( ( (int) round( r8arr[14][0] ) ) != 38 ) ||
         ( ( (int) round( r8arr[101][0] ) ) != 88 ) ||
         ( ( (int) round( r8arr[140][0] ) ) != 10 ) ) {
        printf( "%s\n", "ufbrep check FAILED!" );
        exit(1);
    }

    if ( ( ireadns_f( BUFR_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN ) != 0 ) ||
         ( strncmp( msg_subset, "MSTTB001", 8) != 0 ) || ( iddate != 2016041815 ) ) {
        printf( "%s\n", "ireadns check FAILED!" );
        exit(1);
    }

    ufbseq_f( BUFR_FILE_UNIT, (void**) &r8arr_ptr, 15, 180, &iret, "SIDENSEQ" );
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

    closbf_f( BUFR_FILE_UNIT );

    close_f( BUFR_FILE_UNIT );
}
