/** @file
 *  @brief Standardize a local Table D descriptor.
 *
 * @author J. Ator @date 2004-08-18
*/

#include "bufrlib.h"

/**
 * Maintain an array of descriptors.
 *
 * Given the WMO bit-wise representation of a descriptor,
 * this routine adds it to an ongoing array of descriptors, after
 * first making sure that there is enough room in the array.
 *
 * If an array overflow occurs, then an appropriate error message
 * will be written via bort().
 *
 * @param desc - WMO bit-wise representation of descriptor to be written into descary
 * @param descary - Array of descriptors
 * @param ndescary - Number of descriptors written so far into descary
 * @param mxdescary - Maximum number of descriptors that can be written into descary
 *
 * @author J. Ator @date 2004-08-18
 */
void
wrdesc(int desc, int *descary, int *ndescary, int mxdescary)
{
    char errstr[129];

    /*
    **  Is there room in descary for desc?
    */
    if ( ( *ndescary + 1 ) < mxdescary ) {
        descary[(*ndescary)++] = desc;
    }
    else {
        sprintf(errstr, "BUFRLIB: WRDESC - EXPANDED SECTION 3 CONTAINS"
                        " MORE THAN %d DESCRIPTORS", mxdescary);
        bort_f(errstr);
    }

    return;
}

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
 * @param lun - File ID
 * @param tddesc - WMO bit-wise representation of FXY value for local Table D descriptor
 * @param nctddesc - Number of WMO-standard child descriptors returned in ctddesc
 * @param ctddesc - Array of WMO-standard child descriptors equivalent to tddesc
 *
 * @author J. Ator @date 2004-08-18
*/

void
restd(int lun, int tddesc, int *nctddesc, int *ctddesc)
{
    int maxnc, desc, ncdesc, *cdesc;
    int i, j, inum, itbd, ictbd;
    int iscl, iref, ibit;

    char tab, nemo[NEMO_STR_LEN+1], adn[FXY_STR_LEN+1], cunit[UNIT_STR_LEN+1], cwork[31];
    /*
    **  How many child descriptors does tddesc have?
    */
    numtbd_f(lun, tddesc, nemo, NEMO_STR_LEN+1, &tab, &itbd);
    uptdd_f(itbd, lun, 0, &inum);

    maxnc = igetprm_f("MAXNC");

    *nctddesc = 0;
    /*
    **  Examine each child descriptor one at a time.
    */
    for ( i = 1; i <= inum; i++ ) {
        uptdd_f(itbd, lun, i, &desc);
        if (! istdesc_f(desc)) {
            /*
            **  desc is a local descriptor.
            */
            numtbd_f(lun, desc, nemo, NEMO_STR_LEN+1, &tab, &ictbd);
            if ( tab == 'D' ) {
                /*
                **  desc is itself a local Table D descriptor, so resolve
                **  it now via a recursive call to this same routine.
                */
                if (!(cdesc = malloc(maxnc * sizeof(int)))) bort_f("RESTD FAILED ALLOCATING");

                restd(lun, desc, &ncdesc, cdesc);

                if ( ( *nctddesc > 0 ) &&
                     ( ctddesc[(*nctddesc)-1] >  ifxy_f(MIN_FXY_REPL) ) &&
                     ( ctddesc[(*nctddesc)-1] <= ifxy_f("101255") ) ) {
                    /*
                    **  desc is replicated using fixed replication, so write
                    **  the number of child descriptors into the X value of
                    **  the replication descriptor ctddesc[(*nctddesc)-1]
                    */
                    cadn30_f(ctddesc[(*nctddesc)-1], adn, FXY_STR_LEN+1);
                    sprintf(cwork, "%c%02d%c%c%c",
                             adn[0], ncdesc, adn[3], adn[4], adn[5]);
                    strncpy(adn, cwork, 6); adn[6] = '\0';
                    ctddesc[(*nctddesc)-1] = ifxy_f(adn);
                }
                else if ( ( *nctddesc > 1 ) &&
                          ( ctddesc[(*nctddesc)-2] == ifxy_f(MIN_FXY_REPL) ) ) {
                    /*
                    **  desc is replicated using delayed replication, so write
                    **  the number of child descriptors into the X value of
                    **  the replication descriptor ctddesc[(*nctddesc)-2]
                    */
                    cadn30_f(ctddesc[(*nctddesc)-2], adn, FXY_STR_LEN+1);
                    sprintf(cwork, "%c%02d%c%c%c",
                             adn[0], ncdesc, adn[3], adn[4], adn[5]);
                    strncpy(adn, cwork, 6); adn[6] = '\0';
                    ctddesc[(*nctddesc)-2] = ifxy_f(adn);
                }
                /*
                **  Add the child descriptors to the output list.
                */
                for ( j = 0; j < ncdesc; j++ ) {
                    wrdesc(cdesc[j], ctddesc, nctddesc, maxnc);
                }

                free(cdesc);
            }
            else if ( imrkopr_f(nemo) ) {
                /*
                **  desc is a Table C marker operator, so append it "as is" to the
                **  output list.
                */
                wrdesc(ifxy_f(nemo), ctddesc, nctddesc, maxnc);
            }
            else {
                /*
                **  desc is a local Table B descriptor, so precede it with
                **  a 206YYY operator in the output list.
                */
                nemtbb_f(lun, ictbd, cunit, UNIT_STR_LEN+1, &iscl, &iref, &ibit);
                sprintf(cwork, "%c%c%c%03d", '2', '0', '6', ibit);
                strncpy(adn, cwork, 6); adn[6] = '\0';
                wrdesc(ifxy_f(adn), ctddesc, nctddesc, maxnc);
                wrdesc(desc, ctddesc, nctddesc, maxnc);
            }
        }
        else {
            /*
            **  desc is a standard Table B, Table D, operator or replicator
            **  descriptor, so append it "as is" to the output list.
            */
            wrdesc(desc, ctddesc, nctddesc, maxnc);
        }
    }

    return;
}
