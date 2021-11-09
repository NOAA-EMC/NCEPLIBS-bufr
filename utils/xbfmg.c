/** @file
 *  @brief Split a BUFR file into separate BUFR files by message
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>
#include <unistd.h>
#include <sys/stat.h>

#include "bufrlib.h"

#ifdef UNDERSCORE
#define prtusage prtusage_
#endif

void prtusage( char * );

#define MXFLEN 125

/**
 * This function prints program usage information to standard output.
 *   
 * @author J. Ator
 * @date 2018-03-01
 *
 * @param[in]  prgnam -- char*: [path/]name of program executable
 *
 * <b>Program history log:</b>
 * | Date | Programmer | Comments |
 * | -----|------------|----------|
 * | 2018-03-01 | J. Ator | Original author |
 */
void prtusage( char *prgnam ) {
	printf( "\nUSAGE: %s [-v] [-h] [-g] bufrfile\n\n", prgnam );
	printf( "WHERE:\n" );
	printf( "    -v        prints program version information and exits\n" );
	printf( "    -h        prints program help and usage information and exits\n" );
	printf( "    -g        preserves within each output file any GTS bulletin header and\n" );
	printf( "              control characters associated with the corresponding BUFR message\n" );
	printf( "              from the input file\n" );
	printf( "   bufrfile   [path/]name of input file containing one or more BUFR messages\n" );
	printf( "              to be extracted into separate output files within the current\n" );
	printf( "              working directory\n\n" );
	printf( "The output will be stored within the current working directory using the\n" );
	printf( "following filenames:\n\n" );
	printf( "    (basename).xbfmg.out.000001\n" );
	printf( "    (basename).xbfmg.out.000002\n" );
	printf( "    (basename).xbfmg.out.000003\n" );
	printf( "      .\n" );
	printf( "      .\n" );
	printf( "    (basename).xbfmg.out.(last#)\n\n" );
	printf( "where:\n\n" );
	printf( "    (basename) = basename of bufrfile\n" );
	printf( "    (last#) = total number of BUFR messages in bufrfile\n\n" );
}

/**
 * This program splits a single file containing one or more BUFR messages
 * into one or more BUFR files each containing a single BUFR message.
 *
 * <p>The output BUFR files are written to the current working directory,
 * according to a pre-defined naming convention as described below.
 *
 * @author J. Ator
 * @date 2018-03-01
 *
 * <b>Usage</b><br>
 * <pre>
 *   xbfmg [-v] [-h] [-g] bufrfile
 *
 *     where:
 *       -v        prints version information and exits
 *
 *       -h        prints program help and usage information and exits
 *
 *       -g        preserves within each output file any GTS bulletin header
 *                 and control characters associated with the corresponding
 *                 BUFR message from the input file
 *
 *      bufrfile   [path/]name of input file containing one or more BUFR
 *                 messages to be extracted into separate output files within
 *                 the current working directory
 *
 *   The output will be stored within the current working directory using
 *   the following filenames:
 *
 *      (basename).xbfmg.out.000001
 *      (basename).xbfmg.out.000002
 *      (basename).xbfmg.out.000003
 *        and so on, up through
 *      (basename).xbfmg.out.(last#)
 *
 *   where:
 *
 *      (basename) = basename of bufrfile
 *
 *      (last#) = total number of BUFR messages in bufrfile
 * </pre>
 *
 * <b>Program history log:</b>
 * | Date | Programmer | Comments |
 * | -----|------------|----------|
 * | 2018-03-01 | J. Ator |  Original author |
 * | 2021-09-29 | J. Ator |  Use basename instead of pid in output filenames |
 * | 2021-10-08 | J. Ator |  Simplify bvstr instantiation and initialization |
 */

int main( int argc, char *argv[] ) {

	struct stat fileinfo;

	char *pc, *pmsg, *psb;

	int save_GTSbull = 0;

	char outfile[MXFLEN];
	char outfile_temp[MXFLEN];
	char wkstr[MXFLEN];

	char *bvstr;

	int ch;

	FILE *fp;

	f77int msglen, wkint;
	f77int c24 = 24, c1 = 1;

	unsigned long i, filesize, noutfile;

	unsigned short ii;

	/*
	**  Get the valid options from the command line:
	*/
        while ( ( ch = getopt ( argc, argv, "vgh" ) ) != EOF ) {
            switch ( ch ) {
		case 'v':
		    bvstr = ( char * ) calloc( 9, sizeof(char) );  /* allocate bvstr and initialize to all nulls */
		    bvers( bvstr, sizeof(bvstr) );
		    printf( "This is xbfmg v3.2.0, built with BUFRLIB v%s\n", bvstr );
		    return 0;
		case 'g':
		    save_GTSbull = 1;
		    break;
		case 'h':
                    printf( "\nPROGRAM %s\n", argv[0] );
                    printf( "\nABSTRACT: This program reads an input file containing one or more\n" );
                    printf( "  BUFR messages as given by the first argument.  It then extracts each\n" );
                    printf( "  each individual BUFR message into its own separate output file within\n" );
                    printf( "  the current working directory.\n" );
                    prtusage( argv[0] );
                    return 0;
                    break;
            }
        }

	/*
	**  There should be one remaining command line argument specifying the input file.
	*/
	if ( (optind+1) != argc ) {
	    printf( "\nERROR: You must specify an input BUFR file of BUFR messages!\n" );
            prtusage( argv[0] );
	    return -1;
	}

	/*
	**  Get the filesize of the input file.
	*/
	if ( stat( argv[optind], &fileinfo ) != 0 ) {
	    printf( "\nERROR: Could not stat the file %s!\n", argv[optind] );
	    return -1;
	}
	filesize = fileinfo.st_size;

	/*
	**  Dynamically allocate memory in order to read in the input file.
	*/
	if ( ( pc = malloc( filesize + 1 ) ) == NULL ) {
	    printf( "\nERROR: Could not allocate memory for file %s!\n", argv[optind] );
	    return -1;
	}

	/*
	**  Read the input file into memory.
	*/
	if ( ( fp = fopen( argv[optind], "rb" ) ) == NULL ) {
	    printf( "\nERROR: Could not open input file %s!\n", argv[optind] );
	    return -1;
	}
	for ( i = 0; i < filesize; i++ ) {
	    pc[i] = (char) fgetc( fp );
	} 
	pc[i] = '\0';
	fclose( fp );

	/*
	**  Create an output file name template.
	*/
	strcpy( wkstr, argv[optind] );
	strcpy( outfile_temp, basename( wkstr ) );
	strcat( outfile_temp, ".xbfmg.out" );

	/*
	**  Call wrdlen function to initialize BUFRLIB and determine machine endianness.
	*/
	wrdlen( );

	/*
	**  Locate each BUFR message within the input file and write each one to a separate output file.
	**
	**  Note that we can't use the intrinsic C strstr function to locate the "BUFR" and "7777"
	**  strings within the file, because the file could contain embedded NULL characters.
	*/
	noutfile = 0;
	pmsg = psb = pc;
	while ( 1 ) {
	    while (  ( ( pmsg - pc + 4 ) < filesize )  &&
		    ( ( *(pmsg)     != 'B' )  ||
		      ( *(pmsg + 1) != 'U' )  || 
		      ( *(pmsg + 2) != 'F' )  ||
		      ( *(pmsg + 3) != 'R' ) )  )  {
		if ( *pmsg == '\x01' ) psb = pmsg;
		pmsg++;
	    }
	    if  ( ( pmsg - pc + 4 ) >= filesize )  {
		free( pc );
		return 0;
	    }

	    /*
	    **  Open a new output file for this message.
	    */
	    sprintf( outfile, "%s.%06lu", outfile_temp, ++noutfile );
	    if ( ( fp = fopen( outfile, "wb" ) ) == NULL ) {
		printf( "\nERROR: Could not open output file %s!\n", outfile );
		return -1;
	    }

	    /*
	    **  If requested, write the preceding GTS bulletin information to the output file.
	    */
	    if ( save_GTSbull ) {
		while ( psb < pmsg ) {
		    fputc( *psb++, fp );
		}
	    }

	    /*
	    **  Read the BUFR message length from Section 0.
	    */
	    memcpy( &wkint, ( pmsg + 4 ), 3 );
	    msglen = iupb( &wkint, &c1, &c24 );

	    /*
	    **  Write the BUFR message to the output file.
	    */
	    if  ( ( pmsg + msglen - pc - 1 ) <= filesize ) {
		for ( i = 1; i <= msglen; i++ ) {
		    fputc( *pmsg++, fp );
		}
	    }

	    /*
	    **  Make sure that the "7777" indicator is in the expected place.
	    */
	    if ( ( *(pmsg - 4) != '7' ) || ( *(pmsg - 3) != '7' ) || 
	         ( *(pmsg - 2) != '7' ) || ( *(pmsg - 1) != '7' ) )  {
	        printf( "\nERROR: Could not find 7777 indicator in output file %s!\n",
			outfile );
	    }

	    /*
	    **  If requested, append GTS bulletin tail markers to the output file.
	    */
	    if ( save_GTSbull ) {
		fputc( '\x0d', fp );
		fputc( '\x0d', fp );
		fputc( '\x0a', fp );
		fputc( '\x03', fp );
	    }

	    fclose( fp );
	}	
}
