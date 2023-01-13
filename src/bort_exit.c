/** @file
 *  @brief Abort the application program.
 *
 * ### Program History Log
 * Date | Programmer | Comments |
 * -----|------------|----------|
 * 2003-11-04 | J. Ator   | Original author |
 * 2003-11-04 | D. Keyser | Unified/portable for WRF |
 * 2004-08-18 | J. Ator  | Use bufrlib.h include file |
 * 2007-01-19 | J. Ator  | Fix declaration for ANSI-C |
 *
 * @author J. Ator @date 2003-11-04
 */

#include "bufrlib.h"

/**
 * This subroutine terminates the application program with
 * a non-zero status code.
 *
 * @author J. Ator @date 2003-11-04
 */

void bort_exit( void )
{
    exit( EXIT_FAILURE );
}
