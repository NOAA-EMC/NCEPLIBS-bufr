/** @file
 *  @brief Write a message to a BUFR file that was
 *  previously opened for writing via a C language interface.
 */
#include "bufrlib.h"
#include "cobfl.h"

/** 
 *  This subroutine writes a BUFR message to the system
 *  file that was opened via the most recent call to subroutine
 *  cobfl() with io = 'w'.
 *  
 *  @author J. Ator
 *  @date 2005-11-29
 *
 *  @param[in] bmg    -- char*: BUFR message to be written
 *  @param[in] nmb    -- f77int*: Size (in bytes) of BUFR message
 *                        in bmg
 *  @param[out] iret  -- f77int*: return code
 *                         - 0 = normal return
 *                         - -1 = I/O error encountered while writing
 *
 * <p>This subroutine is designed to be easily callable from
 * application program written in either C or Fortran.
 *
 * <p>The file to which the message is to be written must have already
 * been opened for writing via a previous call to subroutine cobfl()
 * with io = 'w'.
 *
 *  <b>Program history log:</b>
 *  | Date | Programmer | Comments |
 *  | -----|------------|----------|
 *  | 2005-11-29 | J. Ator | Original author |
 */
void cwbmg( char *bmg, f77int *nmb, f77int *iret )
{
    char errstr[129];

/*
**  Make sure that a file is open for writing.
*/
    if ( pbf[1] == NULL ) {
	sprintf( errstr, "BUFRLIB: CWBMG - NO FILE IS OPEN FOR WRITING" );
        bort( errstr, ( f77int ) strlen( errstr ) );
    }
/*
**  Write the BUFR message to the file.
*/
    *iret = ( ( fwrite( bmg, 1, *nmb, pbf[1] ) == *nmb ) ? 0 : -1 );

    return;
}
