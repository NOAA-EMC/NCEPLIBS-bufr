/** @file
 *  @brief Free all dynamically-allocated memory for internal
 *  storage of master Code/Flag table entries.
 * @author J. Ator @date 2017-11-03
 */
#include "bufrlib.h"
#include "cfe.h"

/**
 * This subroutine frees any memory that was dynamically allocated
 * during a previous call to subroutine inittbf().
 *
 * @author J. Ator @date 2017-11-03
 */
void
dlloctbf(void)
{
    free ( cfe );

    cfe = NULL;
}
