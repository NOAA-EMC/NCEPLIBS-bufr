/*
** MAIN PROGRAM DOCUMENTATION BLOCK
**
** MAIN PROGRAM:  cnvrrs
**   PRGMMR: J. Ator          ORG: NCEP        DATE: 2015-XX-XX
**
** ABSTRACT: This program ...(TBD)...
**
** PROGRAM HISTORY LOG:
** 2015-XX-XX  J. Ator     Original author
**
** USAGE:
**   cnvrrs [-v] [-h] [-t mtbldir] [-o outfile] RRSfile tablefil
**
**   WHERE:
**     -v        prints version information and exits
**     -h        prints program help and usage information and exits
**     mtbldir   [path/]name of directory containing BUFR master
**               tables.  If left unspecified, the default is
**               "/nwprod/decoders/decod_shared/fix"
**     outfile   [path/]name of output file containing BUFR using
**               WMO-specified sequence for migrated radiosonde data.
**               If left unspecified, the default is "RFBN.cnvrrs.out"
**               in the current working directory, where RFBN is the
**               basename of the RRSfile (i.e. RRSfile with any
**               preceding [path/] removed).
**     RRSfile   [path/]name of RRS BUFR file to be converted
**     tablefil  [path/]name of NCEP DX tables file to be used for
**               writing the outfile
**
** REMARKS:
**   SUBPROGRAMS CALLED:
**     LOCAL      - fcnvrrs  prtusage
**     BUFRLIB    - ccbfl    cobfl    crbmg    
**
**   FORTRAN logical unit numbers 11, 12, 13, 14 and 51 are reserved
**   for use within the fcnvrrs subroutine.
**
** ATTRIBUTES:
**   LANGUAGE: C
**   MACHINE: Portable to all platforms
*/

#include "cnvrrs.h"

#define MXFLEN 300

int main( int argc, char *argv[ ] ) {

	int ch;

	char io = 'r';
	char mtbldir[MXFLEN];
	char outfile[MXFLEN];
	char tablefil[MXFLEN];
	char wkstr[MXFLEN];
	char wkstr2[MXFLEN];
	char bvstr[9] = "        ";

	unsigned short ii;
	
	/*
	**  Get and process the valid options from the command line:
	*/
	mtbldir[0] = '\0';  /* initialize to empty string */
	outfile[0] = '\0';  /* initialize to empty string */
	while ( ( ch = getopt ( argc, argv, "vht:o:" ) ) != EOF ) {
	    switch ( ch ) {
		case 'v':
		    bvers ( bvstr, sizeof(bvstr) );
		    /* append a trailing NULL to bvstr for printf */
		    for ( ii = 0; ii < sizeof(bvstr); ii++ ) {
			if ( ( bvstr[ii] != '.' ) && ( !isdigit(bvstr[ii]) ) ) {
			  bvstr[ii] = '\0';
			  break;
			}
		    }
		    printf( "This is cnvrrs v1.0.0, built with BUFRLIB v%s\n", bvstr );
		    return 0;
		case 'h':
		    printf( "\nPROGRAM %s\n", argv[0] );
		    printf( "\nABSTRACT: This program ...(TBD)...\n" );
		    prtusage( argv[0] );
		    return 0;
		case 't':
		    strcpy ( mtbldir, optarg );
		    break;
		case 'o':
		    strcpy ( outfile, optarg );
		    break;
	    }
	}

	/*
	**  There should be two remaining command line arguments.
	*/
	if ( (optind+2) != argc ) {
	    printf( "\nERROR: Incorrect number of command line arguments!\n" );
	    prtusage( argv[0] );
	    return -1;
        }

	/*
	**  Open the input RRS BUFR file.
	*/
	cobfl( argv[optind], &io );

	/*
	**  Get the DX tables file.
	*/
	strcpy( tablefil, argv[optind+1] );

	/*
	**  Check whether a master table directory was specified.
	*/
	if ( strlen( mtbldir ) == 0 ) strcpy( mtbldir, "%NULLDIR" );

	/*
	**  Check whether an output file was specified.  If not, make a default
	**  filename in the current working directory using the basename of the
	**  input BUFR file.
	*/
	if ( strlen( outfile ) == 0 ) {
	    strcpy( wkstr2, argv[optind] );
	    strcpy( outfile, basename( wkstr2 ) );
	    strcat( outfile, ".cnvrrs.out" );
	}

	/*
	**  Confirm that the output directory is writeable.
	*/
	strcpy( wkstr2, outfile );
	strcpy( wkstr, dirname( wkstr2 ) );
	if ( access( wkstr, W_OK ) != 0 ) {
	    printf( "\nERROR: Cannot write output file to directory %s\n",
		( strcmp( wkstr, "." ) == 0 ? getcwd( wkstr2, MXFLEN ) : wkstr ) );
	    prtusage( argv[0] );
	    return -1;
	}

	/*
	**  Read and decode each message from the input BUFR file.
	*/
	fcnvrrs( mtbldir, tablefil, outfile,
		 strlen(mtbldir), strlen(tablefil), strlen(outfile) );

	/*
	**  Close the input BUFR file.
	*/
	ccbfl( );

	return 0;
}
