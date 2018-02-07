/*
** MAIN PROGRAM DOCUMENTATION BLOCK
**
** MAIN PROGRAM:  debufr
**   PRGMMR: J. Ator          ORG: NP12        DATE: 2009-07-01
**
** ABSTRACT: This program decodes a specified BUFR file and generates a
**   verbose listing of the contents.
**
** PROGRAM HISTORY LOG:
** 2009-07-01  J. Ator     Original author
**
** USAGE:
**   ./debufr [-b] [-o outfile] [-t tabledir] bufrfile
**
**   WHERE:
**     -b        specifies the "basic" option, whereby only information
**               from Sections 0-3 of each BUFR message in the bufrfile
**               is decoded, and no attempt is made to decode the data
**               in Section 4
**     outfile   [path/]name of file to contain verbose output listing;
**               the default is "debufr.out" in the local run directory
**     tabledir  directory containing BUFR master tables to be used for
**               decoding; the default is "/nwprod/fix"
**     bufrfile  [path/]name of BUFR file to be decoded
**
** REMARKS:
**   SUBPROGRAMS CALLED:
**     LOCAL      - fdebufr
**     BUFRLIB    - ccbfl    cobfl    crbmg    datelen  dxdump
**                  ireadsb  iupbs01  iupbs3   mtinfo   openbf
**                  readerme ufdump   upds3
**
**   FORTRAN logical unit numbers 51, 60, 90 and 91 are reserved for
**   use within the fdebufr subroutine.
**
** ATTRIBUTES:
**   LANGUAGE: C
**   MACHINE: Portable to all platforms
*/

#include <stdio.h>
#include <unistd.h>

#ifdef UNDERSCORE
#define cobfl cobfl_
#define ccbfl ccbfl_
#define fdebufr fdebufr_
#endif

int main( int argc, char *argv[ ] ) {

	int ch;
	int errflg;

	char basic = 'F';
	char io = 'r';
	char outfile[120] = "debufr.out";
	char tbldir[120] = "/nwprod/fix";
	
	/*
	**  Get the valid options from the command line:
	**	-b	"basic" option - only decodes information from Sections 0-3
	**		of each input message, with no decoding of Section 4 data
	**	-o	defines the output filename (default is "debufr.out")
	**	-t	defines the master table directory (default is "/nwprod/fix")
	*/
	errflg = 0;
	while ( ( ch = getopt ( argc, argv, "bo:t:" ) ) != EOF ) {
	    switch ( ch ) {
		case 'b':
		    basic = 'T';
		    break;
		case 'o':
		    strcpy ( outfile, optarg );
		    break;
		case 't':
		    strcpy ( tbldir, optarg );
		    break;
	    }
	}

	/*
	**  There should be one remaining command line argument specifying the
	**  input BUFR file.
	*/
	if ( (optind+1) != argc ) {
	    printf( "\nUsage:  %s [options] BUFRfile\n\n", argv[0] );
	    printf( "  where possible options are:\n" );
	    printf( "    -b\n" );
	    printf( "    -o [path/]filename of output file\n" );
	    printf( "    -t [path/]filename of master table directory\n\n" );
	    printf( "  and BUFRfile is [path/]filename of BUFR input file\n\n" );
	    return -1;
        }

	/*
	**  Open the input BUFR file.
	*/
	cobfl( argv[optind], &io );

	/*
	**  Read and decode each message from the input BUFR file.
	*/
	fdebufr( outfile, tbldir, &basic,
		 strlen(outfile), strlen(tbldir) );

	/*
	**  Close the input BUFR file.
	*/
	ccbfl( );

	return 0;
}
