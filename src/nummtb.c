/** @file
 * @brief Search for an entry corresponding to IDN in the bufr
 * master table.
 * @author J Ator @date 2009-03-23
*/

#include "bufrlib.h"
#include "mstabs.h"

/**
 * This routine searches for an entry corresponding to IDN in the bufr
 * master table (either 'B' or 'D', depending on the value of IDN).
 * The search uses binary search logic, so all of the entries in the
 * table must be sorted in ascending order (by FXY number) in order
 * for this routine to work properly.
 *
 * @param idn - bit-wise representation of FXY number to be searched for.
 * @param tab - table in which idn was found ('B' or 'D').
 * @param ipt - index of entry for idn in master table tab.
 *
 * @author J Ator @date 2009-03-23
*/
void nummtb( f77int *idn, char *tab, f77int *ipt )
{
        f77int *pifxyn, *pbs,  nmt;

        char adn[7], errstr[129];

        if ( *idn >= ifxy( "300000", 6 ) ) {
            *tab = 'D';
            pifxyn = &idfxyn_c[0];
            nmt = nmtd_c;
        }
        else {
            *tab = 'B';
            pifxyn = &ibfxyn_c[0];
            nmt = nmtb_c;
        }

        pbs = ( f77int * ) bsearch( idn, pifxyn, ( size_t ) nmt, sizeof( f77int ),
                                ( int (*) ( const void *, const void * ) ) cmpia );
        if ( pbs == NULL ) {
            cadn30( idn, adn, sizeof( adn ) );
            adn[6] = '\0';
            sprintf( errstr, "BUFRLIB: NUMMTB - COULD NOT FIND DESCRIPTOR "
                             "%s IN MASTER TABLE %c", adn, *tab );
            bort( errstr, ( f77int ) strlen( errstr ) );
        }
        *ipt = pbs - pifxyn;

        return;
}
