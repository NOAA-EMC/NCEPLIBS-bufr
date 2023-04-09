/** @file
 *  @brief Initialize memory for internal storage of master
 *  Code/Flag table entries.
 *  @author J. Ator  @date 2017-11-03
 */
#include "bufrlib.h"
#define IN_INITTBF
#include "cfe.h"

/**
 *  This subroutine initializes the internal memory structure
 *  for storage of master Code/Flag table entries, including
 *  dynamically allocating space for this structure if needed.
 *
 *  @author J. Ator  @date 2017-11-03
*/
void
inittbf(void)
{
    char brtstr[50] = "BUFRLIB: INITTBF FAILED ALLOCATING CFE";

    /*
    **  Has array space for the internal memory structure been
    **  allocated yet?
    */
    if ( cfe == NULL ) {

        mxmtbf = igetprm( "MXMTBF", 6 );

        if ( ( cfe = malloc( mxmtbf * sizeof(struct code_flag_entry) ) )
                        == NULL ) {
            bort( brtstr, ( f77int ) strlen( brtstr ) );
        }
    }

    nmtf = 0;
}
