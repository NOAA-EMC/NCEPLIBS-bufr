/** @file
 *  @brief Read a specified number of bytes from a BUFR file
 *  that was previously opened for reading via a C language
 *  interface.
 */
#include "bufrlib.h"
#include "cobfl.h"

/**
 *  This function reads a specified number of bytes from the
 *  system file that was opened via the most recent call to
 *  subroutine cobfl() with io = 'r'.
 *
 *  @author J. Ator
 *  @date 2005-11-29
 *
 *  @param[in] mxmb    -- f77int*: Dimensioned size (in bytes) of
 *                        bmg; used by the function to ensure that
 *                        it doesn't overflow the bmg array
 *  @param[in] isloc   -- f77int: Starting byte within bmg into
 *                        which to read the next newbytes bytes
 *  @param[in] newbytes -- f77int: Number of bytes to read from
 *                         system file most recently opened for
 *                         input/reading via subroutine cobfl()
 *  @param[out] bmg    -- char*: Array containing the newbytes bytes
 *                        that were read, beginning at byte number
 *                        isloc
 *  @returns rbytes    -- f77int: return code
 *                        - 0 = normal return
 *                        - 1 = overflow of bmg array
 *                        - -1 = end-of-file encountered while
 *                               reading
 *                        - -2 = I/O error encountered while reading
 *
 * <b>Program history log:</b>
 * | Date | Programmer | Comments |
 * | -----|------------|----------|
 * | 2005-11-29 | J. Ator | Original author |
 */
f77int rbytes( char *bmg, f77int *mxmb, f77int isloc, f77int newbytes )
{
    short iret;

    if ( ( isloc + newbytes ) > *mxmb ) {
	iret = 1;
    }
    else if ( fread( &bmg[isloc], 1, newbytes, pbf[0] ) != newbytes ) {
	iret = ( feof(pbf[0]) ? -1 : -2 );
    }
    else {
	iret = 0;
    }

    return (f77int) iret;
}
