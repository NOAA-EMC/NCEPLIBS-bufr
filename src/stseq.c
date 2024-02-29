/** @file
 *  @brief Store information about a standard Table D descriptor
 *  within internal DX BUFR tables.
 *
 * @author J. Ator @date 2009-03-23
 */

#include "bufrlib.h"
#include "mstabs.h"

/**
 * Define a comparison between two integers.
 *
 * This routine defines a comparison between two integers
 * for use by the binary search function bsearch.
 *
 * @param pf1 - first integer to be compared.
 * @param pf2 - second integer to be compared.
 *
 * @return
 * - -1 PF1 is less than PF2
 * - 0 PF1 is equal to PF2
 * - 1 PF1 is greater than PF2
 *
 * @author J. Ator @date 2009-03-23
 */
int
cmpia(const void *pf1, const void *pf2)
{
        int *mypf1 = ( int * ) pf1;
        int *mypf2 = ( int * ) pf2;

        if ( *mypf1 == *mypf2 ) return 0;

        return ( *mypf1 < *mypf2 ? -1 : 1 );
}

/**
 * Search for an entry in the BUFR master table.
 *
 * This routine searches for an entry corresponding to idn in the bufr
 * master table (either 'B' or 'D', depending on the value of idn).
 * The search uses binary search logic, so all of the entries in the
 * table must be sorted in ascending order (by FXY number) in order
 * for this routine to work properly.
 *
 * @param idn - WMO bit-wise representation of FXY number to be searched for.
 * @param tab - table in which idn was found ('B' or 'D').
 * @param ipt - index of entry for idn in master table tab.
 *
 * @author J. Ator @date 2009-03-23
*/
void
nummtb(int *idn, char *tab, int *ipt)
{
        int *pifxyn, *pbs,  nmt;

        char adn[FXY_STR_LEN+1], errstr[129];

        if ( *idn >= ifxy_f(MIN_FXY_TABLED) ) {
            *tab = 'D';
            pifxyn = &idfxyn_c[0];
            nmt = nmtd_c;
        }
        else {
            *tab = 'B';
            pifxyn = &ibfxyn_c[0];
            nmt = nmtb_c;
        }

        pbs = bsearch(idn, pifxyn, (size_t) nmt, sizeof(int),
                   (int (*) (const void *, const void *)) cmpia);
        if ( pbs == NULL ) {
            cadn30_f(*idn, adn, FXY_STR_LEN+1);
            sprintf(errstr, "BUFRLIB: NUMMTB - COULD NOT FIND DESCRIPTOR "
                             "%s IN MASTER TABLE %c", adn, *tab);
            bort_f(errstr);
        }
        *ipt = pbs - pifxyn;

        return;
}

/**
 * Store information about a standard Table D descriptor within internal DX BUFR tables.
 *
 * Given the WMO bit-wise (integer) representation of a standard
 * Table D descriptor, this subroutine uses the master BUFR tables
 * to store all of the necessary information for that descriptor
 * within the internal DX BUFR tables.  Any child descriptors which
 * are themselves Table D descriptors are automatically resolved via
 * a recursive call to this same subroutine.
 *
 * @param lun - File ID
 * @param irepct - Replication sequence counter for the current master table; used internally
 * to keep track of which sequence names have already been defined, and thereby avoid contention
 * within the internal DX BUFR Table D
 * @param idn - WMO bit-wise representation of FXY value for standard Table D descriptor
 * @param nemo - Mnemonic corresponding to idn
 * @param cseq - Description corresponding to idn
 * @param cdesc - Array of WMO-standard child descriptors equivalent to idn
 * @param ncdesc - Number of WMO-standard child descriptors in cdesc
 *
 * @author J. Ator @date 2009-03-23
*/
void
stseq(int lun, int *irepct, int idn, char *nemo,
      char *cseq, int *cdesc, int ncdesc)
{
    int i, j, nb, nd, ix, iy, iret, nbits;
    int rpidn, pkint, ilen, imxcd, imxnf, ipt, *rpdesc;

    char tab, adn[FXY_STR_LEN+1], adn2[FXY_STR_LEN+1], units[10], errstr[129];
    char nemo2[NEMO_STR_LEN+1], rpseq[56], card[80], ctmp[4], cblk = ' ', czero = '0';

/*
**  The following variable is declared as static so that it automatically initializes
**  to zero and remains unchanged between recursive calls to this subroutine.
*/
    static int naf;

/*
**  Is idn already listed as an entry in the internal Table D?
**  If so, then there's no need to proceed any further.
*/
    numtbd_f(lun, idn, nemo2, NEMO_STR_LEN+1, &tab, &iret);
    if ( ( iret > 0 ) && ( tab == 'D' ) ) return;

/*
**  Start a new Table D entry for idn.
*/
    tab = 'D';
    nd = igetntbi_f(lun, &tab);
    cadn30_f(idn, adn, FXY_STR_LEN+1);
    stntbi_f(nd, lun, adn, nemo, cseq);

/*
**  Now, go through the list of child descriptors corresponding to idn.
*/
    imxcd = igetprm_f("MAXCD");
    imxnf = igetprm_f("MXNAF");

    for ( i = 0; i < ncdesc; i++ ) {
        cadn30_f(cdesc[i], adn, FXY_STR_LEN+1); adn[6] = '\0';
        strncpy(ctmp, &adn[1], 2); ctmp[2] = '\0';
        strnum_f(ctmp, &ix, &iret);
        strncpy(ctmp, &adn[3], 4); /* trailing null will be included in this copy */
        strnum_f(ctmp, &iy, &iret);
        if ( adn[0] == '3' ) {
/*
**          cdesc[i] is itself a Table D descriptor, so locate it within the
**          master table D and then store the contents within the internal
**          Table D via a recursive call to this same routine.
*/
            nummtb(&cdesc[i], &tab, &ipt);
            if ( naf > 0 ) {
/*
**              There are associated fields in effect which will modify this
**              descriptor when storing it within the internal Table D.  So
**              create a new sequence to store the contents of this descriptor
**              along with its associated fields.
*/
                rpidn = igettdi_f(lun);

                sprintf(rpseq, "REPLICATION SEQUENCE %.3d", ++(*irepct));
                memset(&rpseq[24], (int) cblk, 31);
                sprintf(nemo2, "RPSEQ%.3d", *irepct);

                stseq(lun, irepct, rpidn, nemo2, rpseq,
                    &idefxy_c[icvidx(ipt,0,imxcd)],
                    ndelem_c[ipt]);
                pkint = rpidn;

            }
            else {
/*
**              Store cdesc[i] as is directly within the internal Table D.
*/
                stseq(lun, irepct, cdesc[i], &cdmnem_c[ipt][0],
                    &cdseq_c[ipt][0],
                    &idefxy_c[icvidx(ipt,0,imxcd)],
                    ndelem_c[ipt]);
                pkint = cdesc[i];
            }
        }
        else if ( adn[0] == '2' ) {
/*
**          cdesc[i] is an operator descriptor.
*/
            if ( ( ( ix >= 4 ) && ( ix <= 6 ) ) || ( imrkopr_f(adn) ) ) {
/*
**              This is a 204YYY, 205YYY, 206YYY operator, or else a 223255,
**              224255, 225255 or 232255 marker operator.  In any case,
**              generate a Table B mnemonic to hold the corresponding data.
*/
                strncpy(nemo2, adn, 6);
                memset(&nemo2[6], (int) cblk, 2);
                nemo2[8] = '\0';

                if ( ( ix == 4 ) && ( iy == 0 ) ) {
/*
**                  Cancel the most-recently added associated field.
*/
                    if ( naf-- <= 0 ) {
                        sprintf(errstr, "BUFRLIB: STSEQ - TOO MANY ASSOCIATED"
                            " FIELD CANCELLATION OPERATORS");
                        bort_f(errstr);
                    }
                }
                else {
/*
**                Is nemo2 already listed as an entry within the internal
**                Table B?
*/
                  nemtab_f(lun, nemo2, &pkint, &tab, &iret);
                  if ( ( iret == 0 ) || ( tab != 'B' ) ) {
/*
**                  No, so create and store a new Table B entry for nemo2.
*/
                    tab = 'B';
                    nb = igetntbi_f(lun, &tab);

                    if ( ix == 4 ) {
                        sprintf(rpseq, "Associated field of %3d bits", iy);
                        nbits = iy;
                        strcpy(units, "NUMERIC");
                    }
                    else if ( ix == 5 ) {
                        sprintf(rpseq, "Text string of %3d bytes", iy);
                        nbits = iy*8;
                        strcpy(units, "CCITT IA5");
                    }
                    else if ( ix == 6 ) {
                        sprintf(rpseq, "Local descriptor of %3d bits", iy);
                        nbits = iy;
                        if ( nbits > 32 ) {
                            strcpy(units, "CCITT IA5");
                        }
                        else {
                            strcpy(units, "NUMERIC");
                        }
                    }
                    else {   // 2-XX-255 marker operator
                        if ( ix == 23 ) {
                            sprintf(rpseq, "Substituted value");
                        }
                        else if ( ix == 24 ) {
                            sprintf(rpseq, "First-order statistical value");
                        }
                        else if ( ix == 25 ) {
                            sprintf(rpseq, "Difference statistical value");
                        }
                        else if ( ix == 32 ) {
                            sprintf(rpseq, "Replaced/retained value");
                        }
                        /* For now, set a default bit width and units. */
                        nbits = 8;
                        strcpy(units, "NUMERIC");
                    }
                    ilen = ( int ) strlen(rpseq);
                    memset(&rpseq[ilen], (int) cblk, 55 - ilen);
/*
**                  Note that 49152 = 3*(2**14), so subtracting 49152 in the
**                  following statement changes a WMO Table D bit-wise FXY value into
**                  a WMO Table B bit-wise FXY value.
*/
                    pkint = (igettdi_f(lun) - 49152);
                    cadn30_f(pkint, adn2, FXY_STR_LEN+1);

                    stntbi_f(nb, lun, adn2, nemo2, rpseq);

                    /* Initialize card to all blanks. */
                    memset(card, (int) cblk, sizeof( card ));

                    strncpy(&card[2], nemo2, 8);
                    memcpy(&card[16], &czero, 1);
                    memcpy(&card[30], &czero, 1);
                    sprintf(&card[33], "%4d", nbits);
                    strcpy(&card[40], units);
                    card[40+strlen(units)] = cblk;  /* overwrite trailing null */
                    elemdx_f(card, lun);
                  }
                  if ( ix == 4 )  {
/*
**                  Add an associated field.
*/
                    if ( naf >= imxnf ) {
                        sprintf(errstr, "BUFRLIB: STSEQ - TOO MANY ASSOCIATED"
                            " FIELDS ARE IN EFFECT AT THE SAME TIME");
                        bort_f(errstr);
                    }
                    iafpk[naf++] = pkint;
                  }
                }
                if ( ix == 6 ) {
/*
**                  Skip over the local descriptor placeholder.
*/
                    if ( ++i >= ncdesc ) {
                        sprintf(errstr, "BUFRLIB: STSEQ - COULD NOT FIND LOCAL"
                            " DESCRIPTOR PLACEHOLDER FOR %s", adn);
                        bort_f(errstr);
                    }
                }
            }
            else {
                pkint = cdesc[i];
            }
        }
        else if ( adn[0] == '1' ) {
/*
**          cdesc[i] is a replication descriptor, so create a sequence
**          consisting of the set of replicated descriptors and then immediately
**          store that sequence within the internal Table D via a recursive call
**          to this same routine.
**
**          See subroutine BFRINI and COMMON /REPTAB/ for the source of the FXY
**          values referenced in the following block.  Note we are guaranteed
**          that 0 <= iy <= 255 since adn was generated using subroutine CADN30.
*/
            if ( iy == 0 ) {        /* delayed replication */
                if ( ( i+1 ) >= ncdesc ) {
                    sprintf(errstr, "BUFRLIB: STSEQ - COULD NOT FIND DELAYED "
                             "DESCRIPTOR REPLICATION FACTOR FOR %s", adn);
                    bort_f(errstr);
                }
                else if ( cdesc[i+1] == ifxy_f("031002") ) {
                    pkint = ifxy_f("360001");
                }
                else if ( cdesc[i+1] == ifxy_f("031001") ) {
                    pkint = ifxy_f("360002");
                }
                else if ( cdesc[i+1] == ifxy_f("031000") ) {
                    pkint = ifxy_f("360004");
                }
                else {
                    sprintf(errstr, "BUFRLIB: STSEQ - UNKNOWN DELAYED "
                             "DESCRIPTOR REPLICATION FACTOR FOR %s", adn);
                    bort_f(errstr);
                }
                i += 2;
            }
            else {        /* regular replication */
                pkint = ifxy_f(MIN_FXY_REPL) + iy;
                i++;
            }
/*
**          Store this replication descriptor within the table D entry for
**          this parent.
*/
            pktdd_f(nd, lun, pkint, &iret);
            if ( iret < 0 ) {
                strncpy(nemo2, nemo, 8);
                nemo2[8] = '\0';
                sprintf(errstr, "BUFRLIB: STSEQ - BAD RETURN FROM PKTDD WHEN "
                         "STORING REPLICATOR FOR PARENT MNEMONIC %s", nemo2);
                bort_f(errstr);
            }
/*
**          Note we are guaranteed that 0 < ix <= 63 since adn was generated
**          using subroutine CADN30.
*/
            if ( ix > ( ncdesc - i ) ) {
                sprintf(errstr, "BUFRLIB: STSEQ - NOT ENOUGH REMAINING CHILD "
                         "DESCRIPTORS TO COMPLETE REPLICATION FOR %s", adn);
                bort_f(errstr);
            }
            else if ( ( ix == 1 ) && ( cdesc[i] >= ifxy_f(MIN_FXY_TABLED) ) ) {
/*
**              The only thing being replicated is a single Table D descriptor,
**              so there's no need to invent a new sequence for this replication
**              (this is a special case!)
*/
                nummtb(&cdesc[i], &tab, &ipt);
                stseq(lun, irepct, cdesc[i], &cdmnem_c[ipt][0],
                       &cdseq_c[ipt][0],
                       &idefxy_c[icvidx(ipt,0,imxcd)],
                       ndelem_c[ipt]);
                pkint = cdesc[i];
            }
            else {
/*
**              Store the ix descriptors to be replicated in a local list, then
**              get an FXY value to use with this list and generate a unique
**              mnemonic and description as well.
*/

                if ( ( rpdesc = malloc( imxcd * sizeof(int) ) ) == NULL ) {
                    sprintf(errstr, "BUFRLIB: STSEQ - UNABLE TO ALLOCATE SPACE"
                            " FOR RPDESC");
                    bort_f(errstr);
                }

                for ( j = 0; j < ix; j++ ) {
                    rpdesc[j] = cdesc[i+j];
                }

                rpidn = igettdi_f(lun);

                sprintf(rpseq, "REPLICATION SEQUENCE %.3d", ++(*irepct));
                memset(&rpseq[24], (int) cblk, 31);
                sprintf(nemo2, "RPSEQ%.3d", *irepct);

                stseq(lun, irepct, rpidn, nemo2, rpseq, rpdesc, ix);

                free(rpdesc);

                pkint = rpidn;
                i += ix - 1;
            }
        }
        else {
/*
**          cdesc[i] is a Table B descriptor.
**
**          Is cdesc[i] already listed as an entry in the internal Table B?
*/
            numtbd_f(lun, cdesc[i], nemo2, NEMO_STR_LEN+1, &tab, &iret);
            if ( ( iret == 0 ) || ( tab != 'B' ) ) {
/*
**              No, so search for it within the master table B.
*/
                nummtb(&cdesc[i], &tab, &ipt);
/*
**              Start a new Table B entry for cdesc[i].
*/
                nb = igetntbi_f(lun, &tab);
                cadn30_f(cdesc[i], adn2, FXY_STR_LEN+1);
                stntbi_f(nb, lun, adn2, &cbmnem_c[ipt][0], &cbelem_c[ipt][0]);

                /* Initialize card to all blanks. */
                memset(card, (int) cblk, sizeof( card ));

                strncpy(&card[2], &cbmnem_c[ipt][0], 8);
                strncpy(&card[13], &cbscl_c[ipt][0], 4);
                strncpy(&card[19], &cbsref_c[ipt][0], 12);
                strncpy(&card[33], &cbbw_c[ipt][0], 4);
                strncpy(&card[40], &cbunit_c[ipt][0], 24);
                elemdx_f(card, lun);
            }
            pkint = cdesc[i];
        }
        if (strncmp( adn, "204", 3) != 0 ) {
/*
**          Store this child descriptor within the table D entry for this
**          parent, preceding it with any associated fields that are currently
**          in effect.
**
**          Note that associated fields are only applied to Table B descriptors,
**          except for those in Class 31.
*/
            if ( ( naf > 0 ) && ( pkint <= ifxy_f(MAX_FXY_TABLEB) ) &&
                    ( ( pkint < ifxy_f("031000") ) ||
                      ( pkint > ifxy_f("031255") ) )  ) {
                for ( j = 0; j < naf; j++ ) {
                    pktdd_f(nd, lun, iafpk[j], &iret);
                    if ( iret < 0 ) {
                      sprintf(errstr, "BUFRLIB: STSEQ - BAD RETURN FROM PKTDD "
                             "WHEN STORING ASSOCIATED FIELDS");
                      bort_f(errstr);
                    }
                }
            }
/*
**          Store the child descriptor.
*/
            pktdd_f(nd, lun, pkint, &iret);
            if ( iret < 0 ) {
                strncpy(nemo2, nemo, 8);
                nemo2[8] = '\0';
                sprintf(errstr, "BUFRLIB: STSEQ - BAD RETURN FROM PKTDD WHEN "
                     "STORING CHILD FOR PARENT MNEMONIC %s", nemo2);
                bort_f(errstr);
            }
        }
    }
}
