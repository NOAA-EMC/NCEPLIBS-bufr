#include "cnvrrs.h"

void prtusage( char *prgnam ) {
/*
**	Prints program usage information to stdout.
*/
	printf( "\nUSAGE:\n" );
	printf( "  %s [-v] [-h] [-t mtbldir] [-o outfile] RRSfile tablefil\n\n", prgnam );
	printf( "WHERE:\n" );
	printf( "    -v        prints program version information and exits\n" );
	printf( "    -h        prints program help and usage information and exits\n" );
	printf( "    mtbldir   [path/]name of directory containing BUFR master tables.  If left\n" );
	printf( "              unspecified, the default is \"/nwprod/decoders/decod_shared/fix\".\n" );
	printf( "    outfile   [path/]name of output file containing BUFR using WMO-specified\n" );
	printf( "              sequence for migrated radiosonde data.  If left unspecified,\n" );
	printf( "              the default is \"RFBN.cnvrrs.out\" in the current working\n" );
	printf( "              working directory, where RFBN is the basename of the RRSfile\n" );
	printf( "              (i.e. RRSfile with any preceding [path/] removed).\n" );
	printf( "    RRSfile   [path/]name of RRS BUFR file to be converted\n" );
	printf( "    tablefil  [path/]name of NCEP DX tables file to be used for writing\n" );
	printf( "              the outfile\n" );
}
