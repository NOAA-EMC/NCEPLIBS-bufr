/** @file
 *  @brief Standardize a local Table D descriptor.
 *
 * @author J. Ator @date 2004-08-18
*/

#include "bufrlib.h"

/**
 * Standardize a local Table D descriptor.
 *
 * Given the bit-wise (integer) representation of a local (not
 * WMO-standard) Table D descriptor, this subroutine returns an
 * equivalent array of WMO-standard child descriptors.
 *
 * Any child descriptors which are themselves local Table D
 * descriptors are automatically resolved via a recursive call to this
 * same subroutine.  This recursive process continues until all child
 * descriptors are either WMO-standard descriptors (from Table B,
 * Table C, Table D, or replication descriptors) or else are local
 * Table B descriptors, in which case they are preceded with an
 * appropriate 2-06-YYY Table C operator in the output array.  The
 * output array is then useable by any standard BUFR decoder program
 * in order to interpret the same data values as were represented by
 * the input local Table D descriptor.
 *
 * @param lun - File ID.
 * @param tddesc - WMO bit-wise representation of FXY value for local Table
 * D descriptor.
 * @param nctddesc - Number of WMO-standard child descriptors returned
 * in ctddesc.
 * @param ctddesc - Array of WMO-standard child descriptors equivalent
 * to tddesc.
 *
 * @author J. Ator @date 2004-08-18
*/

void
restd(int lun, int tddesc, int *nctddesc, int *ctddesc)
{
    int desc, ncdesc, cdesc[MAXNC];
    int i, j, inum, itbd, ictbd;
    int iscl, iref, ibit;

    char tab, nemo[NEMO_STR_LEN+1], adn[FXY_STR_LEN+1], cunit[25], cwork[31];

/*
**  How many child descriptors does tddesc have?
*/
    numtbd_f(lun, tddesc, nemo, NEMO_STR_LEN+1, &tab, &itbd);
    uptdd_f(itbd, lun, 0, &inum);

    *nctddesc = 0;
/*
**  Examine each child descriptor one at a time.
*/
    for ( i = 1; i <= inum; i++ ) {
        uptdd_f(itbd, lun, i, &desc);
        if (! istdesc_f(desc)) {
/*
**          desc is a local descriptor.
*/
            numtbd_f(lun, desc, nemo, NEMO_STR_LEN+1, &tab, &ictbd);
            if ( tab == 'D' ) {
/*
**              desc is itself a local Table D descriptor, so resolve
**              it now via a recursive call to this same routine.
*/
                restd(lun, desc, &ncdesc, cdesc);

                if ( ( *nctddesc > 0 ) &&
                     ( ctddesc[(*nctddesc)-1] >  ifxy_f(MIN_FXY_REPL) ) &&
                     ( ctddesc[(*nctddesc)-1] <= ifxy_f("101255") ) ) {
/*
**                  desc is replicated using fixed replication, so write
**                  the number of child descriptors into the X value of
**                  the replication descriptor ctddesc[(*nctddesc)-1]
*/
                    cadn30_f(ctddesc[(*nctddesc)-1], adn, FXY_STR_LEN+1);
                    sprintf(cwork, "%c%02ld%c%c%c",
                             adn[0], (long) ncdesc, adn[3], adn[4], adn[5]);
                    strncpy(adn, cwork, 6); adn[6] = '\0';
                    ctddesc[(*nctddesc)-1] = ifxy_f(adn);
                }
                else if ( ( *nctddesc > 1 ) &&
                          ( ctddesc[(*nctddesc)-2] == ifxy_f(MIN_FXY_REPL) ) ) {
/*
**                  desc is replicated using delayed replication, so write
**                  the number of child descriptors into the X value of
**                  the replication descriptor ctddesc[(*nctddesc)-2]
*/
                    cadn30_f(ctddesc[(*nctddesc)-2], adn, FXY_STR_LEN+1);
                    sprintf(cwork, "%c%02ld%c%c%c",
                             adn[0], (long) ncdesc, adn[3], adn[4], adn[5]);
                    strncpy(adn, cwork, 6); adn[6] = '\0';
                    ctddesc[(*nctddesc)-2] = ifxy_f(adn);
                }
/*
**              Add the child descriptors to the output list.
*/
                for ( j = 0; j < ncdesc; j++ ) {
                    wrdesc(cdesc[j], ctddesc, nctddesc);
                }

            }
            else if ( tab == 'B' ) {
/*
**              desc is a local Table B descriptor, so precede it with
**              a 206YYY operator in the output list.
*/
                nemtbb_f(lun, ictbd, cunit, 25, &iscl, &iref, &ibit);
                sprintf(cwork, "%c%c%c%03ld", '2', '0', '6', (long) ibit);
                strncpy(adn, cwork, 6); adn[6] = '\0';
                wrdesc(ifxy_f(adn), ctddesc, nctddesc);
                wrdesc(desc, ctddesc, nctddesc);
            }
        }
        else {
/*
**          desc is a standard Table B, Table D, operator or replicator
**          descriptor, so append it "as is" to the output list.
*/
            wrdesc(desc, ctddesc, nctddesc);
        }
    }

    return;
}
