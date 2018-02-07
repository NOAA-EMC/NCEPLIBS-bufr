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
** 2013-10-07  J. Ator     Print Section 1 tank receipt time information
**                         for NCEP/NCO BUFR messages if available
** 2013-11-15  J. Ator     Add -h option and check for non-existent tablefil
** 2014-09-15  J. Ator     Change default path for tabledir, change default
**                         name for outfile, and confirm outfile is writeable
**
** USAGE:
**   debufr [-v] [-h] [-b] [-m] [-o outfile] [-t tabledir] [-f tablefil] bufrfile
**
**   WHERE:
**     -v        prints version information and exits
**     -h        prints program help and usage information and exits
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
**               The default is "bufrfilename.debufr.out" in the current
**               working directory, where bufrfilename is the basename of
**               the bufrfile (i.e. bufrfile with any preceding [path/]
**               removed).
**     tabledir  [path/]name of directory containing tables to be used
**               for decoding.  This directory contains the NCEP DX
**               dictionary tables file to be used (if one was specified
**               via the -f option), or it may contain all of the BUFR
**               master tables when these are being used to decode a
**               file. The default is "/nwprod/decoders/decod_shared/fix"
**               if unspecified.
**     tablefil  file within tabledir containing DX dictionary tables
**               file to be used for decoding.
**     bufrfile  [path/]name of BUFR file to be decoded
**
** REMARKS:
**   SUBPROGRAMS CALLED:
**     LOCAL      - fdebufr  openbt   prtusage
**     BUFRLIB    - ccbfl    cobfl    crbmg    datelen  dxdump
**                  idxmsg   ireadsb  iupbs01  iupbs3   mtinfo
**                  openbf   readerme ufdump   upds3    bvers
**                  rtrcptb
**
**   FORTRAN logical unit numbers 51, 90, 91, 92 and 93 are reserved
**   for use within the fdebufr subroutine.
**
** ATTRIBUTES:
**   LANGUAGE: C
**   MACHINE: Portable to all platforms
*/

#include <stdio.h>
#include <libgen.h>
#include <unistd.h>

#ifdef UNDERSCORE
#define cobfl cobfl_
#define ccbfl ccbfl_
#define fdebufr fdebufr_
#define bvers bvers_
#define prtusage prtusage_
#endif

#define MXFLEN 300

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
	char tbldir[MXFLEN] = "/nwprod/decoders/decod_shared/fix";
	char tblfil[MXFLEN];
	char outfile[MXFLEN];
	char wkstr[MXFLEN];
	char wkstr2[MXFLEN];
	char bvstr[9] = "        ";

	unsigned short ii;

	f77int lentd;
	
	/*
	**  Get and process the valid options from the command line:
	*/
	errflg = 0;
	wkstr[0] = '\0';  /* initialize to empty string */
	outfile[0] = '\0';  /* initialize to empty string */
	while ( ( ch = getopt ( argc, argv, "vhbmo:t:f:" ) ) != EOF ) {
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
		    printf( "This is debufr v2.2.0, built with BUFRLIB v%s\n",
			    bvstr );
		    return 0;
		case 'h':
		    printf( "\nPROGRAM %s\n", argv[0] );
		    printf( "\nABSTRACT: This program decodes a BUFR file and generates a verbose\n" );
		    printf( "  listing of the contents.  If an NCEP DX dictionary tables file is\n" );
		    printf( "  specified (using the -f option) or if the specified BUFR file\n" );
		    printf( "  contains an embedded NCEP DX dictionary message as the first\n" );
		    printf( "  message in the file, then this DX information is used to decode\n" );
		    printf( "  the data messages in the file.  Otherwise, or whenever the -m option\n" );
		    printf( "  is specified, BUFR master tables are read and used to decode the\n" );
		    printf( "  data messages in the file.\n" );
		    prtusage( argv[0] );
		    return 0;
		    break;
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
	    printf( "\nERROR: You must specify an input BUFR file to be decoded!\n" );
	    prtusage( argv[0] );
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
	**  Check whether an output file was specified.  If not, make a default
	**  filename in the current working directory using the basename of the
	**  input BUFR file.
	*/
	if ( strlen( outfile ) == 0 ) {
	    strcpy( wkstr2, argv[optind] );
	    strcpy( outfile, basename( wkstr2 ) );
	    strcat( outfile, ".debufr.out" );
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
	lentd = (f77int) strlen(tbldir);
	fdebufr( outfile, tbldir, &lentd, tblfil, &basic, &forcemt,
		 strlen(outfile), strlen(tbldir), strlen(tblfil) );

	/*
	**  Close the input BUFR file.
	*/
	ccbfl( );

	return 0;
}
