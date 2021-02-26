/** @file
 *  @brief Open a new system file for reading or writing BUFR
 *  messages via a C language interface.
 */
#include "bufrlib.h"
#define IN_COBFL
#include "cobfl.h"

#define MXFNLEN 500

/**
 *  This subroutine opens a new file for reading or writing BUFR
 *  messages via a C language interface.
 *
 *  @author J. Ator
 *  @date 2005-11-29
 *
 *  @param[in] bfl   - char*: System file to be opened.
 *                     Inclusion of directory prefixes or other
 *                     local filesystem notation is allowed, up
 *                     to 500 total characters.
 *  @param[in]  io   - char: Flag indicating how bfl is to
 *                     be opened:
 *                      - 'r' = input (for reading BUFR messages) 
 *                      - 'w' = output (for writing BUFR messages)
 *
 *  <p>This subroutine is designed to be easily callable from
 *  application program written in either C or Fortran. 
 *  It is functionally equivalent to subroutine
 *  openbf(); however, there are some important differences:
 *  - When using openbf(), the underlying file must already be
 *    associated with a Fortran logical unit number on the local
 *    system, typicially via a prior Fortran "OPEN" statement.
 *    This is not required when using this subroutine.
 *  - When using this subroutine, it is only possible to have at most
 *    one input (io = 'r') file and one output (io = 'w') file open
 *    at a time. If a successive call to this subroutine is made in
 *    either case where a file of that type is already open, then
 *    the subroutine will automatically close the previous file of
 *    that type before opening the new one.
 *  - When opening a file for input/reading using openbf(), the
 *    user can make subsequent calls to readmg() or readns() to read
 *    individual BUFR messages from that file into internal arrays,
 *    followed by subsequent calls to readsb() to read individual
 *    subsets from each such message.  However, when opening a
 *    file for input/reading using
 *    this subroutine, the user must instead make subsequent calls
 *    to crbmg() to read individual BUFR messages from that file, and
 *    each such message will be returned directly to the user within
 *    an allocated memory array.  The user may then, if desired,
 *    make subsequent calls to readerme() to store each such
 *    message into the same internal arrays, followed by subsequent
 *    calls to readsb() to read individual subsets from each such
 *    message.
 *  - When opening a file for output/writing using openbf(), the
 *    user can make subsequent successive calls to writsb() to
 *    pack each completed data subset into the BUFR message that
 *    is currently open within the internal arrays, for eventual
 *    output to that file.  However, when opening a file for
 *    output/writing using this subroutine, the user can instead,
 *    if desired, make subsequent successive calls to writsa() to
 *    pack each completed data subset into the BUFR message that
 *    is currently open within the internal arrays. The use of
 *    writsa() will cause each completed BUFR message to be
 *    returned directly to the user within an allocated memory
 *    array, which in turn can then be written directly to the file
 *    via a subsequent call to cwbmg().
 *
 *  <p>Any errors encountered when using this subroutine are
 *  automatically logged to standard output, or to an alternate
 *  location previously specified via a call to subroutine errwrt().
 *  
 *  <b>Program history log:</b>
 *  - 2005-11-29  J. Ator    -- Original author
 */
void cobfl( char *bfl, char *io )
{
    char lbf[MXFNLEN+1];
    char lio;

    char errstr[129];

    char foparg[3] = " b";  /* 3rd character will automatically
			       initialize to NULL */
    unsigned short i, j;

/*
**  Copy the input arguments into local variables and check them for validity.
**  This is especially important in case either of the arguments was passed in
**  as a string literal by the calling program or else doesn't have a trailing
**  NULL character.
*/
    for ( i = 0; ( ! isspace( bfl[i] ) && ! iscntrl( bfl[i] ) ); i++ ) {
	if ( i == MXFNLEN ) {
	    sprintf( errstr, "BUFRLIB: COBFL - INPUT FILENAME CONTAINS"
			    " MORE THAN %d CHARACTERS", MXFNLEN );
	    bort( errstr, ( f77int ) strlen( errstr ) );
	}
	lbf[i] = bfl[i];
    }
    lbf[i] = '\0';

    lio = io[0];
    if ( ( foparg[0] = (char) tolower( lio ) ) == 'r' ) {
	j = 0;
    }
    else if ( foparg[0] == 'w' ) {
	j = 1;
    }
    else {
	sprintf( errstr, "BUFRLIB: COBFL - SECOND ARGUMENT WAS (%c),"
			" WHICH IS AN ILLEGAL VALUE", lio );
	bort( errstr, ( f77int ) strlen( errstr ) );
    }

/*
**  If a file of this type is already open, then close it before
**  opening the new one.
*/
    if ( pbf[j] != NULL ) fclose( pbf[j] );

/*
**  Open the requested file.
*/
    if ( ( pbf[j] = fopen( lbf, foparg ) ) == NULL ) {
	sprintf( errstr, "BUFRLIB: COBFL - COULD NOT OPEN FILE %s", lbf );
	bort( errstr, ( f77int ) strlen( errstr ) );
    }

/*
**  Call wrdlen to initialize some important information about the
**  local machine, just in case it hasn't already been called.
*/
    wrdlen( );
    
    return;
}
