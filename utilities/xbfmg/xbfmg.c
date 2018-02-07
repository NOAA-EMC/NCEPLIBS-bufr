#define	MXBUFR 950000

#ifdef UNDERSCORE
#define cobfl cobfl_
#define crbmg crbmg_
#define cwbmg cwbmg_
#define ccbfl ccbfl_
#endif

main( int argc, char *argv[] )
{
	int mxmb = MXBUFR;
	char bufr[MXBUFR];

	int nbyt, ierr, ierw, ipid;
	char outfile[30];

	unsigned long noutfile;
/*
**	Check for correct number of arguments.
*/
	if ( argc != 2 ) {
	    printf( "Usage: %s <input file>\n", argv[0] );
	    return 0;
	}
	cobfl( argv[1], "r" );
/*
**	Locate each BUFR message within the input file and then write
**	each one to a separate output file.
*/
	noutfile = 0;
	ipid = (int) getpid();
	while (1) {
	    crbmg( bufr, &mxmb, &nbyt, &ierr );
	    if ( ierr < 0 ) {
		break;
	    }
	    else if ( ierr == 1 ) {
		printf( "ierr from crbmg was %ld; "
			"message not written via cwbmg\n", (long) ierr ); 
	    }
	    else {
/*
**		Open a new output file for this message.
*/
		sprintf( outfile, "xbfmg.out.%d.%06lu", ipid, ++noutfile );
		cobfl( outfile, "w" );
/*
**		Write the message to the output file.
*/
		cwbmg( bufr, &nbyt, &ierw );
		if ( ierw != 0 )
			printf( "ierw from crbmg was %ld\n", (long) ierw );
	    }
	}	
	ccbfl( );
	return 0;
}
