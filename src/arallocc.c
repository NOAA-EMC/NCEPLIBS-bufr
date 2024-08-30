/** @file
 *  @brief Allocate or deallocate C language arrays.
 * @author J. Ator @date 2014-12-04
*/

#include "bufrlib.h"

/** Macro to control whether certain variables are explicitly declared or referenced as extern. */
#define IN_ARALLOCC

#include "cread.h"
#include "mstabs.h"
#include "rpseqs.h"

/**
 * Dynamically allocate C language arrays.
 *
 * This subroutine is called internally during the first call to
 * subroutine openbf() from an application program, in order to
 * dynamically allocate internal C language arrays based on parameter
 * values set during one or more previous calls to function isetprm().
 *
 * All memory allocated within this subroutine can be freed via a
 * subsequent call to subroutine exitbufr().
 *
 * @author J. Ator @date 2014-12-04
 */

void
arallocc(void)
{
    char brtstr[50] = "BUFRLIB: ARALLOCC FAILED ALLOCATING";

    int nfiles;

    int mxmtbb;
    int mxmtbd;
    int maxcd;
    int mxnaf;

    /* cread arrays */

    nfiles = igetprm_f("NFILES");

    if (!(pb = malloc((nfiles+1) * sizeof(FILE *)))) bort_f(brtstr);
    if (!(lstpos = malloc((nfiles+1) * sizeof(fpos_t)))) bort_f(brtstr);

    /* mstabs arrays */

    mxmtbb = igetprm_f("MXMTBB");
    mxmtbd = igetprm_f("MXMTBD");
    maxcd = igetprm_f("MAXCD");
    mxnaf = igetprm_f("MXNAF");

    if (!(ibfxyn_c = malloc(mxmtbb * sizeof(int)))) bort_f(brtstr);
    if (!(cbscl_c = malloc(mxmtbb * 4 * sizeof(char)))) bort_f(brtstr);
    if (!(cbsref_c = malloc(mxmtbb * 12 * sizeof(char)))) bort_f(brtstr);
    if (!(cbbw_c = malloc(mxmtbb * 4 * sizeof(char)))) bort_f(brtstr);
    if (!(cbunit_c = malloc(mxmtbb * 24 * sizeof(char)))) bort_f(brtstr);
    if (!(cbmnem_c = malloc(mxmtbb * 8 * sizeof(char)))) bort_f(brtstr);
    if (!(cbelem_c = malloc(mxmtbb * 120 * sizeof(char)))) bort_f(brtstr);
    if (!(idfxyn_c = malloc(mxmtbd * sizeof(int)))) bort_f(brtstr);
    if (!(cdseq_c = malloc(mxmtbd * 120 * sizeof(char)))) bort_f(brtstr);
    if (!(cdmnem_c = malloc(mxmtbd * 8 * sizeof(char)))) bort_f(brtstr);
    if (!(ndelem_c = malloc(mxmtbd * sizeof(int)))) bort_f(brtstr);
    if (!(idefxy_c = malloc(mxmtbd * maxcd * sizeof(int)))) bort_f(brtstr);
    if (!(iafpk = malloc(mxnaf * sizeof(int)))) bort_f(brtstr);

    /* rpseqs arrays */

    if (!(cdescs = malloc(MAX_RPSQ * maxcd * sizeof(int)))) bort_f(brtstr);
    if (!(iafpks = malloc(MAX_RPSQ * mxnaf * sizeof(int)))) bort_f(brtstr);
}

/**
 * Free all memory that was dynamically allocated
 * during a previous call to subroutine arallocc().
 *
 * @author J. Ator @date 2014-12-04
 */

void
ardllocc(void)
{
    /* cread arrays */

    free( pb );
    free( lstpos );

    /* mstabs arrays */

    free( ibfxyn_c );
    free( cbscl_c );
    free( cbsref_c );
    free( cbbw_c );
    free( cbunit_c );
    free( cbmnem_c );
    free( cbelem_c );
    free( idfxyn_c );
    free( cdseq_c );
    free( cdmnem_c );
    free( ndelem_c );
    free( idefxy_c );
    free( iafpk );

    /* rpseqs arrays */

    free( cdescs );
    free( iafpks );

    /* master Code/Flag table entries */

    dlloctbf();
}
