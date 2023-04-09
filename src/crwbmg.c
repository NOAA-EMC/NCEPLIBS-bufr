/** @file
 *  @brief C language interface for reading or writing BUFR messages.
 *
 *  @author J. Ator  @date 2005-11-29
 */
#include "bufrlib.h"

/** Maximum length of a filename, including any directory prefixes or other local filesystem notation. */
#define MXFNLEN 200

/** File pointers; each element will automatically initialize to NULL */
FILE *pbf[2];

/**
 * Read a specified number of bytes from the file that was opened via the
 * most recent call to function cobfl() with io = 'r'.
 *
 * @param mxmb   - Number of elements in bmg array; used by the function to ensure that
 *                 it doesn't overflow the array.
 * @param isloc  - Starting byte within bmg into which to read the next newbytes bytes
 * @param newbytes - Number of bytes to read from file most recently opened for
 *                   input/reading via function cobfl()
 * @param bmg    - Array containing the newbytes bytes that were read, beginning at byte
 *                 number isloc.
 * @returns rbytes - return code
 *                   - 0 = normal return
 *                   - 1 = overflow of bmg array
 *                   - -1 = end-of-file encountered while reading
 *                   - -2 = I/O error encountered while reading
 *
 * @author J. Ator  @date 2005-11-29
 */
int
rbytes(char *bmg, int mxmb, int isloc, int newbytes)
{
   int iret;

   if ( ( isloc + newbytes ) > mxmb ) {
       iret = 1;
   }
   else if ( fread( &bmg[isloc], 1, newbytes, pbf[0] ) != newbytes ) {
       iret = ( feof(pbf[0]) ? -1 : -2 );
   }
   else {
       iret = 0;
   }

   return iret;
}

/**
 * Open a new file for reading or writing BUFR messages via a C language interface.
 *
 * This function is designed to be easily callable from application program written in either C or
 * Fortran. It is functionally equivalent to subroutine openbf(); however, there are some important
 * differences:
 *  - When using openbf(), the underlying file must already be associated with a Fortran logical unit
 *    number on the local system, typicially via a prior Fortran "OPEN" statement. This is not
 *    required when using this function.
 *  - When using this function, it is only possible to have at most one input (io = 'r') file and
 *    one output (io = 'w') file open at a time. If a successive call to this function is made in
 *    either case where a file of that type is already open, then the function will automatically
 *    close the previous file of that type before opening the new one.
 *  - When opening a file for input/reading using openbf(), the user can make subsequent calls to any
 *    of the NCEPLIBS-bufr [message-reading subroutines](@ref hierarchy) to read individual BUFR messages
 *    from that file into internal arrays, followed by subsequent calls to any of the NCEPLIBS-bufr
 *    [subset-reading subroutines](@ref hierarchy)) to read individual data subsets from each such
 *    message.  However, when opening a file for input/reading using this function, the user must
 *    instead make subsequent calls to crbmg() to read individual BUFR messages from that file, and
 *    each such message will be returned directly to the user within an allocated memory array.
 *    The user may then, if desired, make subsequent calls to readerme() to store each such message
 *    into the same internal arrays, followed by subsequent calls to any of the NCEPLIBS-bufr
 *    [subset-reading subroutines](@ref hierarchy) to read individual data subsets from each such
 *    message.
 *  - When opening a file for output/writing using openbf(), the user can make subsequent successive
 *    calls to writsb() to pack each completed data subset into the BUFR message that is currently
 *    open within the internal arrays, for eventual output to that file.  However, when opening a
 *    file for output/writing using this function, the user can instead, if desired, make
 *    subsequent successive calls to writsa() to pack each completed data subset into the BUFR
 *    message that is currently open within the internal arrays. The use of writsa() will cause each
 *    completed BUFR message to be returned directly to the user within an allocated memory array,
 *    which in turn can then be written directly to the file via a subsequent call to cwbmg().
 *
 * Any errors encountered when using this function are automatically logged to standard output,
 * or to an alternate location previously specified via a call to subroutine errwrt().
 *
 * @param bfl - System file to be opened. Inclusion of directory prefixes or other local filesystem
 *              notation is allowed, up to 200 total characters.
 * @param io - Flag indicating how bfl is to be opened:
 *              - 'r' input (for reading BUFR messages)
 *              - 'w' output (for writing BUFR messages)
 *
 * @author J. Ator @date 2005-11-29
 */
void
cobfl(char *bfl, char io)
{
   char lbf[MXFNLEN+1];
   char lio;

   char errstr[MXFNLEN+50];

   char foparg[3] = " b";  /* 3rd character will automatically initialize to NULL */
   unsigned short i, j;

   /*
   ** Copy the input arguments into local variables and check them for validity. This is especially
   ** important in case either of the arguments was passed in as a string literal by the calling
   ** program or else doesn't have a trailing NULL character.
   */
   for ( i = 0; ( ! isspace( bfl[i] ) && ! iscntrl( bfl[i] ) ); i++ ) {
       if ( i == MXFNLEN ) {
           sprintf( errstr, "BUFRLIB: COBFL - INPUT FILENAME CONTAINS"
                            " MORE THAN %hu CHARACTERS", ( unsigned short ) MXFNLEN );
           bort( errstr, ( f77int ) strlen( errstr ) );
       }
       lbf[i] = bfl[i];
   }
   lbf[i] = '\0';

   lio = io;
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
   ** If a file of this type is already open, then close it before opening the new one.
   */
   if ( pbf[j] != NULL ) fclose( pbf[j] );

   /*
   ** Open the requested file.
   */
   if ( ( pbf[j] = fopen( lbf, foparg ) ) == NULL ) {
       sprintf( errstr, "BUFRLIB: COBFL - COULD NOT OPEN FILE %s", lbf );
       bort( errstr, ( f77int ) strlen( errstr ) );
   }

   /*
   ** Call wrdlen to initialize some important information about the local machine, just in case
   ** it hasn't already been called.
   */
   wrdlen( );

   return;
}

/**
 * Read the next BUFR message from the file that was opened via the most
 * recent call to function cobfl() with io = 'r'.
 *
 * This function is designed to be easily callable from application program written in either C
 * or Fortran.
 *
 * @param mxmb - Number of elements in bmg array;; used by the function to ensure that it
 *               doesn't overflow the array.
 * @param bmg  - BUFR message
 * @param nmb  - Size (in bytes) of BUFR message in bmg.
 * @param iret - return code
 *               - 0 = normal return
 *               - 1 = overflow of bmg array
 *               - 2 = "7777" indicator not found in expected location
 *               - -1 = end-of-file encountered while reading
 *               - -2 = I/O error encountered while reading
 *
 * @author J. Ator @date 2005-11-29
 */
void
crbmg(char *bmg, int mxmb, int *nmb, int *iret)
{
   int i4 = 4;
   int wkint[2];

   char errstr[129];
   char blanks[5] = "    ";

   /*
   ** Make sure that a file is open for reading.
   */
   if ( pbf[0] == NULL ) {
       sprintf( errstr, "BUFRLIB: CRBMG - NO FILE IS OPEN FOR READING" );
       bort( errstr, ( f77int ) strlen( errstr ) );
   }

   /*
   ** Initialize the first 4 characters of the output array to blanks.
   */
   if ( mxmb < 5 ) {
       *iret = 1;
       return;
   }
   strcpy( bmg, blanks);

   /*
   ** Look for the start of the next BUFR message.
   */
   while ( ichkstr( "BUFR", bmg, &i4, 4, 4 ) != 0 ) {
       memmove( bmg, &bmg[1], 3 );
       if ( ( *iret = rbytes( bmg, mxmb, 3, 1 ) ) != 0 ) return;
   }

   /*
   ** Read the next 4 bytes of the BUFR message and get the length of the message.
   */
   if ( ( *iret = rbytes( bmg, mxmb, 4, 4 ) ) != 0 ) return;
   memcpy( wkint, bmg, 8 );
   *nmb = iupbs01_f( wkint, "LENM" );

   /*
   ** Read the remainder of the BUFR message.
   */
   if ( ( *iret = rbytes( bmg, mxmb, 8, *nmb-8 ) ) != 0 ) return;

   /*
   ** Check that the "7777" is in the expected location.
   */
   *iret = ( ( ichkstr( "7777", &bmg[*nmb-4], &i4, 4, 4 ) == 0 ) ? 0 : 2 );

   return;
}

/**
 * Write a BUFR message to the file that was opened via the most recent
 * call to function cobfl() with io = 'w'.
 *
 * This function is designed to be easily callable from application program written in either C
 * or Fortran.
 *
 * @param bmg - BUFR message
 * @param nmb - Size (in bytes) of BUFR message in bmg
 * @param iret - return code:
 *                - 0 normal return
 *                - -1 I/O error encountered while writing
 *
 * @author J. Ator @date 2005-11-29
 */
void
cwbmg(char *bmg, int nmb, int *iret)
{
   char errstr[129];

   /*
   ** Make sure that a file is open for writing.
   */
   if ( pbf[1] == NULL ) {
       sprintf( errstr, "BUFRLIB: CWBMG - NO FILE IS OPEN FOR WRITING" );
       bort( errstr, ( f77int ) strlen( errstr ) );
   }

   /*
   ** Write the BUFR message to the file.
   */
   *iret = ( ( fwrite( bmg, 1, nmb, pbf[1] ) == nmb ) ? 0 : -1 );

   return;
}

/**
 * Close all files that were opened via previous calls to function cobfl().
 *
 * @author J. Ator  @date 2005-11-29
 */
void
ccbfl(void)
{
   unsigned short i;

   for ( i = 0; i < 2; i++ ) {
       if ( pbf[i] != NULL ) fclose( pbf[i] );
   }

   return;
}
