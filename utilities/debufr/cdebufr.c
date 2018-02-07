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
**   the data messages in the file.  Otherwise, or whenever the -m option
**   is specified, BUFR master tables are read and used to decode the
**   data messages in the file.
**
** PROGRAM HISTORY LOG:
** 2009-07-01  J. Ator     Original author
** 2012-06-18  J. Ator     Modified to allow decoding of BUFR files
**                         based on NCEP DX dictionary table
**                         information.  The program can now process
**                         any files that previously required the use
**                         of ckbufr.
** 2012-12-07  J. Ator     Modified to add -m and -v options and inline
**                         version of OPENBT subroutine for mixed BUFR files
**
** USAGE:
**   debufr [-v] [-b] [-m] [-o outfile] [-t tabledir] [-f tablefil] bufrfile
**
**   WHERE:
**     -v        prints version information and exits
**     -b        specifies the "basic" option, meaning that only the
**               information in Sections 0-3 will be decoded from each
**               BUFR message in the bufrfile, and no attempt will be
**               made to decode the data in Section 4
**     -m        specifies that BUFR master tables will be used to
**               decode the data messages in the file, regardless of
**               whether they contain any embedded NCEP DX dictionary
**               messages.  This option can be used to view the actual
**               contents of dictionary messages, which otherwise would
**               not be printed in the output listing.
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
**     LOCAL      - fdebufr  openbt
**     BUFRLIB    - ccbfl    cobfl    crbmg    datelen  dxdump
**                  idxmsg   ireadsb  iupbs01  iupbs3   mtinfo
**                  openbf   readerme ufdump   upds3    bvers
**
**   FORTRAN logical unit numbers 51, 90, 91, 92 and 93 are reserved
**   for use within the fdebufr subroutine.
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
#define bvers bvers_
#endif

#ifdef F77_INTSIZE_8
    typedef long f77int;
#else
    typedef int f77int;
#endif

int main( int argc, char *argv[ ] ) {

	int ch;
	int errflg;

	char basic = 'N';
	char forcemt = 'N';
	char io = 'r';
	char outfile[120] = "debufr.out";
	char tbldir[120] = "/nwprod/fix";
	char tblfil[240];
	char wkstr[120];
	char bvstr[9] = "        ";

	unsigned short ii;

	f77int lentd;
	
	/*
	**  Get the valid options from the command line:
	**	-v	prints version information and exits
	**	-b	only decodes information from Sections 0-3 of each input
	**		message, with no decoding of Section 4 data
	**	-m	forces the use of master tables for decoding
	**	-o	defines the output filename (default is "debufr.out")
	**	-t	defines the tables directory (default is "/nwprod/fix")
	**	-f	defines the DX tables file
	*/
	errflg = 0;
	wkstr[0] = '\0';  /* initialize to empty string */
	while ( ( ch = getopt ( argc, argv, "vbmo:t:f:" ) ) != EOF ) {
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
		    printf( "This is debufr v2.1.0, built with BUFRLIB v%s\n",
			    bvstr );
		    return 0;
		case 'b':
		    basic = 'Y';
		    break;
		case 'm':
		    forcemt = 'Y';
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
	    printf( "    -v\n" );
	    printf( "    -b\n" );
	    printf( "    -m\n" );
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
	lentd = (f77int) strlen(tbldir);
	fdebufr( outfile, tbldir, &lentd, tblfil, &basic, &forcemt,
		 strlen(outfile), strlen(tbldir), strlen(tblfil) );

	/*
	**  Close the input BUFR file.
	*/
	ccbfl( );

	return 0;
}
