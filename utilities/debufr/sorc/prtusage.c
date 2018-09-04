#ifdef UNDERSCORE
#define prtusage prtusage_
#endif

void prtusage( char *prgnam ) {
/*
**	Prints program usage information to stdout.
*/
	printf( "\nUSAGE:\n" );
	printf( "  %s [-v] [-h] [-b] [-m] [-o outfile] [-t tabledir] [-f tablefil] [-p prmstg] bufrfile\n\n", prgnam );
	printf( "WHERE:\n" );
	printf( "    -v        prints program version information and exits\n" );
	printf( "    -h        prints program help and usage information and exits\n" );
	printf( "    -b        specifies the \"basic\" option, meaning that only the\n" );
	printf( "              information in Sections 0-3 will be decoded from each\n" );
	printf( "              BUFR message in the bufrfile, and no attempt will be\n" );
	printf( "              made to decode the data in Section 4\n" );
	printf( "    -c        specifies that code and flag table meanings should not\n" );
	printf( "              be read from master tables and included in the output;\n" );
	printf( "              otherwise this feature is enabled by default\n" );
	printf( "    -m        specifies that BUFR master tables will be used to\n" );
	printf( "              decode the data messages in the file, regardless of\n" );
	printf( "              whether they contain any embedded NCEP DX dictionary\n" );
	printf( "              messages.  This option can be used to view the actual\n" );
	printf( "              contents of dictionary messages, which otherwise would\n" );
	printf( "              not be printed in the output listing.\n" );
	printf( "    outfile   [path/]name of file to contain verbose output listing.\n" );
	printf( "              The default is \"bufrfilename.debufr.out\" in the current\n" );
	printf( "              working directory, where bufrfilename is the basename of\n" );
	printf( "              the bufrfile (i.e. bufrfile with any preceding [path/]\n" );
	printf( "              removed).\n" );
	printf( "    tabledir  [path/]name of directory containing tables to be used\n" );
	printf( "              for decoding.  This directory contains the NCEP DX\n" );
	printf( "              dictionary tables file to be used (if one was specified\n" );
	printf( "              via the -f option), or it may contain all of the BUFR\n" );
	printf( "              master tables when these are being used to decode a\n" );
	printf( "              file.  The default is \"/nwprod/decoders/decod_shared/fix\"\n" );
	printf( "              if unspecified.\n" );
	printf( "    tablefil  file within tabledir containing DX dictionary tables\n" );
	printf( "              file to be used for decoding.\n" );
	printf( "     prmstg   string of comma-separated PARAMETER=VALUE pairs, up to a\n" );
	printf( "              maximum of 80 characters in length.  For each pair, the\n" );
  	printf( "              dynamic allocation PARAMETER will be set to VALUE within\n" );
	printf( "              the underlying BUFRLIB software, overriding the default\n" );
	printf( "              value that would otherwise be used.  A complete list of\n" );
	printf( "              parameters that can be dynamically sized is included\n" );
	printf( "              within the BUFRLIB documentation for function ISETPRM.\n" );
	printf( "    bufrfile  [path/]name of BUFR file to be decoded\n" );
}
