/*
** MAIN PROGRAM DOCUMENTATION BLOCK
**
** MAIN PROGRAM:  debufr
**   PRGMMR: J. Ator          ORG: NP12        DATE: 2009-07-01
**
** ABSTRACT: This program decodes a BUFR file and generates a verbose
**   listing of the contents.  If an NCEP DX dictionary tables file is
**   specified (using the -f option) or if the specified BUFR file
**   contains an embedded NCEP DX dictionary message as the first
**   message in the file, then this DX information is used to decode
**   the data messages in the file.  Otherwise, BUFR master tables are
**   read and used to decode the data messages in the file.
**
** PROGRAM HISTORY LOG:
** 2009-07-01  J. Ator     Original author
** 2012-06-18  J. Ator     Modified to allow decoding of BUFR files
**                         based on NCEP DX dictionary table
**                         information.  The program can now process
**                         any files that previously required the use
**                         of ckbufr.
**
** USAGE:
**   ./debufr [-b] [-o outfile] [-t tabledir] [-f tablefil] bufrfile
**
**   WHERE:
**     -b        specifies the "basic" option, whereby only information
**               from Sections 0-3 of each BUFR message in the bufrfile
**               is decoded, and no attempt is made to decode the data
**               in Section 4
**     outfile   [path/]name of file to contain verbose output listing.
**               The default is "debufr.out" in the local run directory.
**     tabledir  [path/]name of directory containing tables to be used
**               for decoding.  This directory contains the NCEP DX
**               dictionary tables file to be used (if one was specified
**               via the -f option), or it may contain all of the BUFR
**               master tables when these are being used to decode a
**               file.  The default is "/nwprod/fix" if unspecified.
**     tablefil  file within tabledir containing DX dictionary tables
**               file to be used for decoding.
**     bufrfile  [path/]name of BUFR file to be decoded
**
** REMARKS:
**   SUBPROGRAMS CALLED:
**     LOCAL      - fdebufr
**     BUFRLIB    - ccbfl    cobfl    crbmg    datelen  dxdump
**                  idxmsg   ireadsb  iupbs01  iupbs3   mtinfo
**                  openbf   readerme ufdump   upds3
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

	char basic = 'N';
	char io = 'r';
	char outfile[120] = "debufr.out";
	char tbldir[120] = "/nwprod/fix";
	char tblfil[240];
	char wkstr[120];
	
	/*
	**  Get the valid options from the command line:
	**	-b	"basic" option - only decodes information from Sections 0-3
	**		of each input message, with no decoding of Section 4 data
	**	-o	defines the output filename (default is "debufr.out")
	**	-t	defines the tables directory (default is "/nwprod/fix")
	**	-f	defines the DX tables file
	*/
	errflg = 0;
	wkstr[0] = '\0';  /* initialize to empty string */
	while ( ( ch = getopt ( argc, argv, "bo:t:f:" ) ) != EOF ) {
	    switch ( ch ) {
		case 'b':
		    basic = 'Y';
		    break;
		case 'o':
		    strcpy ( outfile, optarg );
		    break;
		case 't':
		    strcpy ( tbldir, optarg );
		    break;
		case 'f':
		    strcpy ( wkstr, optarg );
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
	    printf( "    -t [path/]filename of tables directory\n" );
	    printf( "    -f filename of DX tables file in tables directory\n\n" );
	    printf( "  and BUFRfile is [path/]filename of BUFR input file\n\n" );
	    return -1;
        }

	/*
	**  Open the input BUFR file.
	*/
	cobfl( argv[optind], &io );

	/*
	**  Check whether a DX tables file was specified.
	*/
	if ( strlen( wkstr ) > 0 ) {
	    sprintf( tblfil, "%s%c%s", tbldir, '/', wkstr );
	}
	else {
	    strcpy( tblfil, "NULLFILE" );
	}

	/*
	**  Read and decode each message from the input BUFR file.
	*/
	fdebufr( outfile, tbldir, tblfil, &basic,
		 strlen(outfile), strlen(tbldir), strlen(tblfil) );

	/*
	**  Close the input BUFR file.
	*/
	ccbfl( );

	return 0;
}
