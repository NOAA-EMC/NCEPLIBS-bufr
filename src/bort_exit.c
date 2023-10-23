/** @file
 *  @brief Abort the application program.
 * @author J. Ator @date 2003-11-04
 */

#include "bufrlib.h"

/**
 * Terminate the application program with a non-zero status code.
 *
 * @author J. Ator @date 2003-11-04
 */

void
bort_exit(void)
{
    exit( EXIT_FAILURE );
}
