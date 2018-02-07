#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

/* depending on the operating system, may need to append an underscore */
#ifdef UNDERSCORE
#define wrdlen wrdlen_
#define bvers bvers_
#define iupb iupb_
#endif

/*
** In order to ensure that the C <-> FORTRAN interface works properly (and
** portably!), the default size of an "INTEGER" declared in FORTRAN must be
** identical to that of an "int" declared in C.  If this is not the case (e.g.
** some FORTRAN compilers, most notably AIX via the -qintsize= option, allow the
** sizes of INTEGERs to be definitively prescribed outside of the source code
** itself!), then the following conditional directive (or a variant of it) can
** be used to ensure that the size of an "int" in C remains identical to that
** of an "INTEGER" in FORTRAN.
*/
#ifdef F77_INTSIZE_8
    typedef long f77int;
#else
    typedef int f77int;
#endif


/* declare prototypes for ANSI C compatibility */
void wrdlen( void );
f77int iupb( f77int *, f77int *, f77int * );

/************************************************************************
**	Extracts each BUFR message from a given input file into its	*
**	own separate output file.					*
**									*
**	NOTE:  Could not use "strstr" to locate "BUFR" and "7777",	*
**	       because input file may contain embedded NULL characters.	*
************************************************************************/

main( int argc, char *argv[] )
{

	struct stat fileinfo;

	char *pc, *pmsg, *psb;
	int save_GTSbull = 0;
	char outfile[30];
	char bvstr[9] = "        ";

	int ch;

	FILE *fp;

	int ipid;

	f77int msglen, wkint;
	f77int c24 = 24, c1 = 1;

	unsigned long i, filesize, noutfile;

	unsigned short ii;

        /*
	**  Get the valid options from the command line:
	**      -v      prints version information and exits
	**      -g	saves any GTS bulletin information associated with each BUFR message      
	*/
        while ( ( ch = getopt ( argc, argv, "vg" ) ) != EOF ) {
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
		    printf( "This is xbfmg v3.0.0, built with BUFRLIB v%s\n", bvstr );
		    return 0;
                case 'g':
		    save_GTSbull = 1;
                    break;
            }
        }

	/*
	**  There should be one remaining command line argument specifying the input file.
	*/
	if ( (optind+1) != argc ) {
	    printf( "Usage: %s [options] infile\n", argv[0] );
	    printf( "  where possible options are:\n" );
	    printf( "    -v\n" );
	    printf( "    -g\n" );
	    printf( "  and infile is [path/]filename of input file containing one or more BUFR messages\n\n" );
	    return -1;
	}

	/*
	**  Get the filesize of the input file.
	*/
	if ( stat( argv[optind], &fileinfo ) != 0 ) {
	    printf( "Could not stat the file %s!\n", argv[optind] );
	    return -1;
	}
	filesize = fileinfo.st_size;

	/*
	**  Dynamically allocate memory in order to read in the input file.
	*/
	if ( ( pc = malloc( filesize + 1 ) ) == NULL ) {
	    printf( "Could not allocate memory for file %s!\n", argv[optind] );
	    return -1;
	}

	/*
	**  Read the input file into memory.
	*/
	if ( ( fp = fopen( argv[optind], "rb" ) ) == NULL ) {
	    printf( "Could not open input file %s!\n", argv[optind] );
	    return -1;
	}
	for ( i = 0; i < filesize; i++ ) {
	    pc[i] = (char) fgetc( fp );
	} 
	pc[i] = '\0';
	fclose( fp );

	/*
	**  Call wrdlen function to initialize BUFRLIB and determine machine endianness.
	*/
	wrdlen( );

	/*
	**  Locate each BUFR message within the input file and write each one to a separate output file.
	*/
	noutfile = 0;
	ipid = (int) getpid();
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
**	    Open a new output file for this message.
*/
	    sprintf( outfile, "xbfmg.out.%d.%06lu", ipid, ++noutfile );
	    if ( ( fp = fopen( outfile, "wb" ) ) == NULL ) {
		printf( "Could not open output file %s!\n", outfile );
		break;
	    }
/*
**	    If requested, write the preceding GTS bulletin information to the output file.
*/
	    if ( save_GTSbull ) {
		while ( psb < pmsg ) {
		    fputc( *psb++, fp );
		}
	    }
/*
**	    Read the BUFR message length from Section 0.
*/
	    memcpy( &wkint, ( pmsg + 4 ), 3 );
	    msglen = iupb( &wkint, &c1, &c24 );

/*
**	    Write the BUFR message to the output file.
*/
	    if  ( ( pmsg + msglen - pc - 1 ) <= filesize ) {
		for ( i = 1; i <= msglen; i++ ) {
		    fputc( *pmsg++, fp );
		}
	    }
/*
**	    Make sure that the "7777" indicator is in the expected place.
*/
	    if ( ( *(pmsg - 4) != '7' ) || ( *(pmsg - 3) != '7' ) || 
	         ( *(pmsg - 2) != '7' ) || ( *(pmsg - 1) != '7' ) )  {
	        printf( "Could not find 7777 indicator in output file %s!\n",
			outfile );
	    }

/*
**	    If requested, append GTS bulletin tail markers to the output file.
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
