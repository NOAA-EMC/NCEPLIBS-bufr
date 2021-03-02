/** @file
 *  @author J. Ator
 *  @date 2021-03-01
 *
 *  @brief Test some of the bufr_c_interface.mod functions.
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bufr_interface.h"

static const int BUFR_FILE_UNIT = 11;
static const int TABLE_1_FILE_UNIT = 90;
static const int TABLE_2_FILE_UNIT = 91;
static const int SUBSET_STRING_LEN = 8;

static const char* INPUT_FILE = "testfiles/IN_4";

int main() {

    int iret;
    int iddate;
    char msg_subset[SUBSET_STRING_LEN];

    double r8arr[180][3];
    double* r8arr_ptr = &r8arr[0][0];

    open_f( BUFR_FILE_UNIT, INPUT_FILE );

    openbf_f( BUFR_FILE_UNIT, "SEC3", BUFR_FILE_UNIT );

    mtinfo_f( "../tables", TABLE_1_FILE_UNIT, TABLE_2_FILE_UNIT );

    if ( ( ireadmg_f( BUFR_FILE_UNIT, msg_subset, &iddate, SUBSET_STRING_LEN ) != 0 ) ||
	 ( strcmp( msg_subset, "MSTTB001" ) != 0 ) || ( iddate != 16041815 ) ) {
	printf( "%s\n", "ireadmg check FAILED!" );
	exit(1);
    }
    else {
	if ( ireadsb_f( BUFR_FILE_UNIT ) != 0 ) {
	    printf( "%s\n", "ireadsb check FAILED!" );
	    exit(1);
	}
	else {
	    ufbint_f( BUFR_FILE_UNIT, (void**) &r8arr_ptr, 3, 180, &iret, "CLONH SAID SAZA" );
	    if ( ( ( (int) round( r8arr[0][0] * 100000 ) ) != -4246453 ) ||
		 ( ( (int) round( r8arr[0][1] ) ) != 57 ) ||
		 ( ( (int) round( r8arr[0][2] * 100 ) ) != 5407 ) ) {
	        printf( "%s\n", "ufbint check FAILED!" );
		exit(1);
	    }
	    ufbrep_f( BUFR_FILE_UNIT, (void**) &r8arr_ptr, 3, 180, &iret, "PCCF" );
	    if ( ( ( (int) round( r8arr[11][0] ) ) != 86 ) ||
		 ( ( (int) round( r8arr[14][0] ) ) != 38 ) ||
		 ( ( (int) round( r8arr[101][0] ) ) != 88 ) ||
		 ( ( (int) round( r8arr[140][0] ) ) != 10 ) ) {
	        printf( "%s\n", "ufbrep check FAILED!" );
		exit(1);
	    }
	}
    }

    closbf_f( BUFR_FILE_UNIT );

    close_f( BUFR_FILE_UNIT );
}
