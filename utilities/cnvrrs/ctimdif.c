/*
** calculates time difference (time2 - time1) in seconds
**
** to save CPU cycles, it is assumed that time1 is the same for every call to this routine
** within the same run of this program
**
** all args are input except rltds which is output
*/

#include "cnvrrs.h"

void ctimdif( f77r8 *ryr1, f77r8 *rmo1, f77r8 *rdy1, f77r8 *rhr1, f77r8 *rmi1, f77r8 *rse1,
	      f77r8 *ryr2, f77r8 *rmo2, f77r8 *rdy2, f77r8 *rhr2, f77r8 *rmi2, f77r8 *rse2,
	      f77r8 *rltds )
{

    static char cdt1[16];
    static struct tm tm1;

    struct tm tm2;
    char cdt2[16];

    static int ifirst = 1;

    if ( ifirst ) {
      sprintf( cdt1, "%04.0f%02.0f%02.0f %02.0f%02.0f%02.0f", 
	 *ryr1, *rmo1, *rdy1, *rhr1, *rmi1, *rse1 );
      strptime( cdt1, "%Y%m%d %H%M%S", &tm1 );
      ifirst = 0;
    }

    sprintf( cdt2, "%04.0f%02.0f%02.0f %02.0f%02.0f%02.0f", 
	*ryr2, *rmo2, *rdy2, *rhr2, *rmi2, *rse2 );
    strptime( cdt2, "%Y%m%d %H%M%S", &tm2 );

    *rltds = ( f77r8 ) difftime( mktime( &tm2 ), mktime( &tm1 ) );

}
