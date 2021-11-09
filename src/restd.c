/** @file
 *  @brief Standardize a local Table D descriptor.
*/

#include "bufrlib.h"

/**
 * Given the bit-wise (integer) representation of a local (not
 * WMO-standard) Table D descriptor, this subroutine returns an
 * equivalent array of WMO-standard child descriptors.
 *
 * <p>Any child descriptors which are themselves local Table D
 * descriptors are automatically resolved via a recursive call to
 * this same subroutine.  This recursive process continues until all
 * child descriptors are either WMO-standard descriptors (from Table B,
 * Table C, Table D, or replication descriptors) or else are local
 * Table B descriptors, in which case they are preceded with an
 * appropriate 2-06-YYY Table C operator in the output array.
 * The output array is then useable by any standard BUFR decoder program
 * in order to interpret the same data values as were represented by
 * the input local Table D descriptor.
 *
 * @author J. Ator
 * @date 2004-08-18
 *
 * @param[in] lun -- f77int*: Internal Fortran I/O stream index
 *                   associated with BUFR file
 * @param[in] tddesc -- f77int*: Bit-wise representation of FXY value
 *                      for local Table D descriptor
 * @param[out] nctddesc -- f77int*: Number of WMO-standard child
 *                         descriptors returned in ctddesc
 * @param[out] ctddesc -- f77int*: Array of WMO-standard child
 *                        descriptors equivalent to tddesc
 *
 * <b>Program history log:</b>
 * | Date | Programmer | Comments |
 * | -----|------------|----------|
 * | 2004-08-18 | J. Ator | Original author |
 * | 2012-04-30 | J. Ator | Use long cast for ibit in sprintf stmt |
 * | 2021-08-18 | J. Ator | Use cwork to silence superfluous GNU compiler warnings |
*/

void restd( f77int *lun, f77int *tddesc, f77int *nctddesc, f77int ctddesc[] )
{
    f77int i0 = 0;

    f77int desc, ncdesc, cdesc[MAXNC];
    f77int i, j, inum, itbd, ictbd;
    f77int iscl, iref, ibit;

    char tab, nemo[9], adn[7], cunit[25], cwork[31];

/*
**  How many child descriptors does *tddesc have?
*/
    numtbd( lun, tddesc, nemo, &tab, &itbd, 9, 1 );
    uptdd( &itbd, lun, &i0, &inum );

    *nctddesc = 0;
/*
**  Examine each child descriptor one at a time.
*/
    for ( i = 1; i <= inum; i++ ) {
	uptdd( &itbd, lun, &i, &desc ); 
	if (! istdesc( &desc ) ) {
/*
**	    desc is a local descriptor.
*/ 
	    numtbd( lun, &desc, nemo, &tab, &ictbd, 9, 1 );
	    if ( tab == 'D' ) {
/*
**		desc is itself a local Table D descriptor, so resolve
**		it now via a recursive call to this same routine.
*/ 
	        restd( lun, &desc, &ncdesc, cdesc );

	        if ( ( *nctddesc > 0 ) &&
		     ( ctddesc[(*nctddesc)-1] >  ifxy( "101000", 6 ) ) &&
		     ( ctddesc[(*nctddesc)-1] <= ifxy( "101255", 6 ) ) ) {
/*
**		    desc is replicated using fixed replication, so write
**		    the number of child descriptors into the X value of
**		    the replication descriptor ctddesc[(*nctddesc)-1]
*/
		    cadn30( &ctddesc[(*nctddesc)-1], adn, 7 );
		    sprintf( cwork, "%c%02ld%c%c%c",
			     adn[0], (long) ncdesc, adn[3], adn[4], adn[5] );
		    strncpy( adn, cwork, 6 ); adn[6] = '\0';
		    ctddesc[(*nctddesc)-1] = ifxy( adn, 7 );
		}
		else if ( ( *nctddesc > 1 ) &&
			  ( ctddesc[(*nctddesc)-2] == ifxy( "101000", 6 ) ) ) {
/*
**		    desc is replicated using delayed replication, so write
**		    the number of child descriptors into the X value of
**		    the replication descriptor ctddesc[(*nctddesc)-2]
*/
		    cadn30( &ctddesc[(*nctddesc)-2], adn, 7 );
		    sprintf( cwork, "%c%02ld%c%c%c",
			     adn[0], (long) ncdesc, adn[3], adn[4], adn[5] );
		    strncpy( adn, cwork, 6 ); adn[6] = '\0';
		    ctddesc[(*nctddesc)-2] = ifxy( adn, 7 );
		}
/*
**		Add the child descriptors to the output list.
*/
		for ( j = 0; j < ncdesc; j++ ) {
		    wrdesc( cdesc[j], ctddesc, nctddesc );
		}
		    
	    }
	    else if ( tab == 'B' ) {
/*
**		desc is a local Table B descriptor, so precede it with
**		a 206YYY operator in the output list.
*/ 
		nemtbb( lun, &ictbd, cunit, &iscl, &iref, &ibit, 25 );
		sprintf( cwork, "%c%c%c%03ld", '2', '0', '6', (long) ibit );
		strncpy( adn, cwork, 6 ); adn[6] = '\0';
		wrdesc( ifxy( adn, 7 ), ctddesc, nctddesc );
	        wrdesc( desc, ctddesc, nctddesc );
	    }
        }
	else {
/*
**	    desc is a standard Table B, Table D, operator or replicator
**	    descriptor, so append it "as is" to the output list.
*/ 
	    wrdesc( desc, ctddesc, nctddesc );
	}
    }

    return;
}
