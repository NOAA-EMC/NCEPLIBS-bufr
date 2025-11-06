/** @file
 *  @brief C language interface for returning to an application program in the
 *  event of a bort error.
 *
 *  @authors J. Ator, D. O'Connor
 *  @date 2025-08-20
 */

#include "bufrlib.h"
#include <setjmp.h>

/** Context information for returning to an application program. */
jmp_buf context;

/**
 *  Return to the previously-set target location after a bort error.
 *
 *  @author J. Ator @date 2025-08-20
*/
void
bort_goto_target(void)
{
    longjmp(context, 1);
}

/**
 * Catch any bort error inside of subroutine openbf().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cio - Flag indicating how lunit is to be used by the software
 * @param lundx - Fortran logical unit number containing DX BUFR table information
 * @param cio_str_len - Length of cio string
 *
 * @author J. Ator @date 2025-09-05
*/
void
catch_bort_openbf(int lunit, char *cio, int lundx, int cio_str_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cio, for use with get_c_string_length inside of openbf_f. */
    cio[cio_str_len] = '\0';

    /* Recursively call the subroutine. */
    openbf_f(lunit, cio, lundx);
}

/**
 * Catch any bort error inside of subroutine closbf().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 *
 * @author J. Ator @date 2025-09-05
*/
void
catch_bort_closbf(int lunit)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    closbf_f(lunit);
}

/**
 * Catch any bort error inside of subroutine status().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param lun - File ID associated with lunit
 * @param il - File status
 * @param im - Message status
 *
 * @author J. Ator @date 2025-10-24
*/
void
catch_bort_status(int lunit, int *lun, int *il, int *im)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    status_f(lunit, lun, il, im);
}

/**
 * Catch any bort error inside of subroutine readmg().
 *
 * @param lunxx - Absolute value is Fortran logical unit number for BUFR file
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 * @param iret - return code:
 *   - 0 = new BUFR message was successfully read into internal arrays
 *   - -1 = there are no more messages in the BUFR file
 *
 * @author J. Ator @date 2025-08-20
*/
void
catch_bort_readmg(int lunxx, char *subset, int *jdate, int subset_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    readmg_f(lunxx, subset, jdate, subset_str_len, iret);
}

/**
 * Catch any bort error inside of subroutine openmb().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param subset - Table A mnemonic for type of BUFR message to be written
 * @param subset_str_len - Length of subset string
 * @param jdate - Date-time to be written into Section 1 of BUFR message
 *
 * @author J. Ator @date 2025-10-20
*/
void
catch_bort_openmb(int lunit, char *subset, int subset_str_len, int jdate)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to subset, for use with get_c_string_length inside of openmb_f. */
    subset[subset_str_len] = '\0';

    /* Recursively call the subroutine. */
    openmb_f(lunit, subset, jdate);
}

/**
 * Catch any bort error inside of subroutine openmg().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param subset - Table A mnemonic for type of BUFR message to be written
 * @param subset_str_len - Length of subset string
 * @param jdate - Date-time to be written into Section 1 of BUFR message
 *
 * @author J. Ator @date 2025-10-20
*/
void
catch_bort_openmg(int lunit, char *subset, int subset_str_len, int jdate)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to subset, for use with get_c_string_length inside of openmg_f. */
    subset[subset_str_len] = '\0';

    /* Recursively call the subroutine. */
    openmg_f(lunit, subset, jdate);
}

/**
 * Catch any bort error inside of subroutine readns().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 * @param iret - return code:
 *   - 0 = new BUFR data subset was successfully read into internal arrays
 *   - -1 = there are no more data subsets in the BUFR file
 *
 * @author J. Ator @date 2025-09-05
*/
void
catch_bort_readns(int lunit, char *subset, int *jdate, int subset_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    readns_f(lunit, subset, jdate, subset_str_len, iret);
}

/**
 * Catch any bort error inside of subroutine readsb().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param iret - return code:
 *   - 0 = new BUFR data subset was successfully read into internal arrays
 *   - -1 = there are no more data subsets in the BUFR file
 *
 * @author J. Ator @date 2025-09-05
*/
void
catch_bort_readsb(int lunit, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    readsb_f(lunit, iret);
}

/**
 * Catch any bort error inside of subroutine writsb().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 *
 * @author J. Ator @date 2025-10-20
*/
void
catch_bort_writsb(int lunit)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    writsb_f(lunit);
}

/**
 * Catch any bort error inside of subroutine writsa().
 *
 * @param lunxx - Absolute value is Fortran logical unit number for BUFR file
 * @param bufr_len - Allocated length of bufr array
 * @param bufr - BUFR message
 * @param nbufr - Number of integers returned in bufr array, or 0 if no message was returned
 *
 * @author J. Ator @date 2025-10-20
*/
void
catch_bort_writsa(int lunxx, int bufr_len, int *bufr, int *nbufr)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    writsa_f(lunxx, bufr_len, bufr, nbufr);
}

/**
 * Catch any bort error inside of subroutine ufbint().
 *
 * @param lunin - Absolute value is Fortran logical unit number for BUFR file
 * @param usr - Data values
 * @param i1 - First dimension of usr
 * @param i2 - Second dimension of usr
 * @param iret - Number of replications of cstr that were read/written from/to the data subset
 * @param cstr - String of mnemonics to read/write from/to the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-09-22
*/
void
catch_bort_ufbint(int lunin, double *usr, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbint_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbint_f(lunin, (void**) &usr, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine ufbrep().
 *
 * @param lunin - Absolute value is Fortran logical unit number for BUFR file
 * @param usr - Data values
 * @param i1 - First dimension of usr
 * @param i2 - Second dimension of usr
 * @param iret - Number of replications of cstr that were read/written from/to the data subset
 * @param cstr - String of mnemonics to read/write from/to the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-10-06
*/
void
catch_bort_ufbrep(int lunin, double *usr, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbrep_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbrep_f(lunin, (void**) &usr, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine ufbstp().
 *
 * @param lunin - Absolute value is Fortran logical unit number for BUFR file
 * @param usr - Data values
 * @param i1 - First dimension of usr
 * @param i2 - Second dimension of usr
 * @param iret - Number of replications of cstr that were read/written from/to the data subset
 * @param cstr - String of mnemonics to read/write from/to the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-10-24
*/
void
catch_bort_ufbstp(int lunin, double *usr, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbstp_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbstp_f(lunin, (void**) &usr, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine ufbevn().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param usr - Data values
 * @param i1 - First dimension of usr
 * @param i2 - Second dimension of usr
 * @param i3 - Third dimension of usr
 * @param iret - Number of replications of cstr that were read from the data subset
 * @param cstr - String of mnemonics to read from the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_ufbevn(int lunit, double *usr, int i1, int i2, int i3, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbevn_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbevn_f(lunit, (void**) &usr, i1, i2, i3, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine drfini().
 *
 * @param lunit - Fortran logical unit number to write to
 * @param mdrf - Array of delayed replication factors
 * @param ndrf - Number of delayed replication factors in mdrf
 * @param drftag - Table D mnemonic
 * @param drftag_len - Length of drftag
 *
 * @author Jeff Ator @date 2025-10-28
*/
void
catch_bort_drfini(int lunit, int *mdrf, int ndrf, char *drftag, int drftag_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to drftag, for use with get_c_string_length inside of drfini_f. */
    drftag[drftag_len] = '\0';

    /* Recursively call the subroutine. */
    drfini_f(lunit, mdrf, ndrf, drftag);
}

/**
 * Catch any bort error inside of subroutine ufbseq().
 *
 * @param lunin - Absolute value is Fortran logical unit number for BUFR file
 * @param usr - Data values
 * @param i1 - First dimension of usr
 * @param i2 - Second dimension of usr
 * @param iret - Number of replications of cstr that were read/written from/to the data subset
 * @param cstr - Mnemonic describing sequence to read/write from/to the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-10-06
*/
void
catch_bort_ufbseq(int lunin, double *usr, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbseq_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbseq_f(lunin, (void**) &usr, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine readlc().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cstr - Mnemonic of long character string to read from data subset
 * @param cstr_len - Length of cstr
 * @param chr - Long character string corresponding to cstr
 * @param chr_len - Allocated length of chr
 * @param nchr - Number of characters returned in chr
 *
 * @author J. Ator @date 2025-10-15
*/
void
catch_bort_readlc(int lunit, char *cstr, int cstr_len, char *chr, int chr_len, int *nchr)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of readlc_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    readlc_f(lunit, cstr, chr, chr_len);

    *nchr = (int) strlen(chr);
}

/**
 * Catch any bort error inside of subroutine writlc().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cstr - Mnemonic of long character string to write to data subset
 * @param cstr_len - Length of cstr
 * @param cchr - Long character string corresponding to cstr
 * @param cchr_len - Length of cchr
 *
 * @author J. Ator @date 2025-10-24
*/
void
catch_bort_writlc(int lunit, char *cstr, int cstr_len, char *cchr, int cchr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of writlc_f. */
    cstr[cstr_len] = '\0';

    /* Add a trailing null to cchr, for use with get_c_string_length inside of writlc_f. */
    cchr[cchr_len] = '\0';

    /* Recursively call the subroutine. */
    writlc_f(lunit, cstr, cchr);
}

/**
 * Catch any bort error inside of subroutine ufbcnt().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param kmsg - Message number
 * @param ksub - Subset number
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_ufbcnt(int lunit, int *kmsg, int *ksub)
{
    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Recursively call the subroutine. */
    ufbcnt_f(lunit, kmsg, ksub);
}

/**
 * Catch any bort error inside of subroutine ufbqcd().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cnemo - Mnemonic associated with a Category 63 Table D descriptor
 * @param iqcd - Y value of descriptor associated with mnemonic
 * @param cnemo_len - Length of cnemo
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_ufbqcd(int lunit, char *cnemo, int *iqcd, int cnemo_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Add a trailing null to cnemo, for use with get_c_string_length inside of ufbqcd_f. */
    cnemo[cnemo_len] = '\0';

    /* Recursively call the subroutine. */
    ufbqcd_f(lunit, cnemo, iqcd);
}

/**
 * Catch any bort error inside of subroutine ufbqcp().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param iqcp - Y value of a Category 63 Table D descriptor
 * @param cnemo - Mnemonic associated with iqcp
 * @param cnemo_len - Allocated length of cnemo string
 * @param ncn - Number of characters returned in cnemo
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_ufbqcp(int lunit, int iqcp, char *cnemo, int cnemo_len, int *ncn)
{
    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Recursively call the subroutine. */
    ufbqcp_f(lunit, iqcp, cnemo, cnemo_len);

    *ncn = (int) strlen(cnemo);
}
