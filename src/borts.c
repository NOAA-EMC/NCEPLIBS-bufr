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
 * Catch any bort error inside of subroutine closmg().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_closmg(int lunit)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    closmg_f(lunit);
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
    if ( setjmp(context) == 1 ) {
        dealloc_vars_f("readlc_f");
        *nchr = 1;
        return;
    }

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
    if ( setjmp(context) == 1 ) {
        dealloc_vars_f("writlc_f");
        return;
    }

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
    if ( setjmp(context) == 1 ) {
        *ncn = 1;
        return;
    }

    /* Recursively call the subroutine. */
    ufbqcp_f(lunit, iqcp, cnemo, cnemo_len);

    *ncn = (int) strlen(cnemo);
}

/**
 * Catch any bort error inside of subroutine getcfmng().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cnemoi - Mnemonic to search for
 * @param lcni - Length of cnemoi
 * @param ivali - Value associated with cnemoi
 * @param cnemod - Optional second mnemonic upon which cnemoi may depend
 * @param lcnd - Length of cnemod
 * @param ivald - Value associated with cnemod
 * @param cmeang_c - Meaning associated with cnemoi and ivali (and possibly cnemod and ivald as well)
 * @param lcmgc - Allocated length of cmeang_c
 * @param lnmng - Number of characters returned in cmeang_c
 * @param iret - Return code from call to getcfmng_f
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_getcfmng(int lunit, char *cnemoi, int lcni, int ivali, char *cnemod, int lcnd, int ivald,
                    char *cmeang_c, int lcmgc, int *lnmng, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) {
        dealloc_vars_f("getcfmng_f");
        *lnmng = 1;
        return;
    }

    /* Add trailing nulls to input strings, for use with get_c_string_length inside of getcfmng_f. */
    cnemoi[lcni] = '\0';
    cnemod[lcnd] = '\0';

    /* Recursively call the subroutine. */
    getcfmng_f(lunit, cnemoi, ivali, cnemod, ivald, cmeang_c, lcmgc, iret);

    *lnmng = (int) strlen(cmeang_c);
}

/**
 * Catch any bort error inside of subroutine upftbv().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cnemo - Mnemonic with flag table units
 * @param lcn - Length of cnemo
 * @param val - Value corresponding to cnemo
 * @param ibit - Bit numbers which were set to "On" in val
 * @param mxib - Allocated size of ibit
 * @param nib - Number of bit numbers returned in ibit
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_upftbv(int lunit, char *cnemo, int lcn, double val, int *ibit, int mxib, int *nib)
{
    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Add a trailing null to cnemo, for use with get_c_string_length inside of upftbv_f. */
    cnemo[lcn] = '\0';

    /* Recursively call the subroutine. */
    upftbv_f(lunit, cnemo, val, ibit, mxib, nib);
}

/**
 * Catch any bort error inside of function cobfl().
 *
 * @param bfl - System file to be opened
 * @param io - Flag indicating how bfl is to be opened
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_cobfl(const char *bfl, char io)
{

    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Recursively call the function. */
    cobfl(bfl, io);
}

/**
 * Catch any bort error inside of function crbmg().
 *
 * @param bmg - BUFR message
 * @param mxmb - Allocated length of bmg
 * @param nmb - Number of characters returned in bmg
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_crbmg(char *bmg, int mxmb, int *nmb, int *iret)
{

    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Recursively call the function. */
    crbmg(bmg, mxmb, nmb, iret);
}

/**
 * Catch any bort error inside of function cwbmg().
 *
 * @param bmg - BUFR message
 * @param nmb - Number of characters in bmg
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-11-05
*/
void
catch_bort_cwbmg(const char *bmg, int nmb, int *iret)
{

    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Recursively call the function. */
    cwbmg(bmg, nmb, iret);
}

/**
 * Catch any bort error inside of subroutine ufbtab().
 *
 * @param lunin - Absolute value is Fortran logical unit number for BUFR file
 * @param tab - Data values
 * @param i1 - First dimension of tab
 * @param i2 - Second dimension of tab
 * @param iret - Number of data subsets returned
 * @param cstr - String of mnemonics to read from each data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-11-13
*/
void
catch_bort_ufbtab(int lunin, double *tab, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbtab_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbtab_f(lunin, (void**) &tab, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine ufbpos().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param irec - Ordinal number of message to be read
 * @param isub - Ordinal number of subset to be read from (irec)th message
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 *
 * @author J. Ator @date 2025-11-13
*/
void
catch_bort_ufbpos(int lunit, int irec, int isub, char *subset, int *jdate, int subset_str_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufbpos_f(lunit, irec, isub, subset, jdate, subset_str_len);
}

/**
 * Catch any bort error inside of subroutine datelen().
 *
 * @param len - Length of Section 1 date-time values to be output by all future calls to
 * message-reading subroutines
 *
 * @author J. Ator @date 2025-11-14
*/
void
catch_bort_datelen(int len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    datelen_f(len);
}

/**
 * Catch any bort error inside of function iupvs01().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param s01mnem - Mnemonic for value to be read from Section 0 or Section 1 of BUFR message
 * @param s01mnem_str_len - Length of s01mnem string
 * @param iret - Value corresponding to s01mnem
 *
 * @author J. Ator @date 2025-11-14
*/
void
catch_bort_iupvs01(int lunit, char *s01mnem, int s01mnem_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to s01mnem, for use with get_c_string_length inside of iupvs01_f. */
    s01mnem[s01mnem_str_len] = '\0';

    /* Recursively call the function. */
    *iret = iupvs01_f(lunit, s01mnem);
}

/**
 * Catch any bort error inside of function nmsub().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param iret - Number of data subsets
 *
 * @author J. Ator @date 2025-11-14
*/
void
catch_bort_nmsub(int lunit, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the function. */
    *iret = nmsub_f(lunit);
}

/**
 * Catch any bort error inside of subroutine pkvs01().
 *
 * @param s01mnem - Mnemonic for value to be written into Section 0 or Section 1 of BUFR message
 * @param s01mnem_str_len - Length of s01mnem string
 * @param ival - Value corresponding to s01mnem
 *
 * @author J. Ator @date 2025-11-14
*/
void
catch_bort_pkvs01(char *s01mnem, int s01mnem_str_len, int ival)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to s01mnem, for use with get_c_string_length inside of pkvs01_f. */
    s01mnem[s01mnem_str_len] = '\0';

    /* Recursively call the subroutine. */
    pkvs01_f(s01mnem, ival);
}

/**
 * Catch any bort error inside of subroutine datebf().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param mear - Year stored within Section 1 of first data message
 * @param mmon - Month stored within Section 1 of first data message
 * @param mday - Day stored within Section 1 of first data message
 * @param mour - Hour stored within Section 1 of first data message
 * @param idate - Date-time stored within Section 1 of first data message
 *
 * @author J. Ator @date 2025-11-18
*/
void
catch_bort_datebf(int lunit, int *mear, int *mmon, int *mday, int *mour, int *idate)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    datebf_f(lunit, mear, mmon, mday, mour, idate);
}

/**
 * Catch any bort error inside of subroutine dumpbf().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param jdate - Dump center date-time stored within Section 1 of first "dummy" message
 * @param jdump - Dump initiation date-time stored within Section 1 of second "dummy" message
 *
 * @author J. Ator @date 2025-11-18
*/
void
catch_bort_dumpbf(int lunit, int *jdate, int *jdump)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    dumpbf_f(lunit, jdate, jdump);
}

/**
 * Catch any bort error inside of subroutine minimg().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param mini - Minutes value
 *
 * @author J. Ator @date 2025-11-18
*/
void
catch_bort_minimg(int lunit, int mini)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    minimg_f(lunit, mini);
}

/**
 * Catch any bort error inside of subroutine upds3().
 *
 * @param mbay - BUFR message
 * @param lcds3 - Allocated length of cds3
 * @param ccds3 - Data descriptor sequence within Section 3 of mbay
 * @param nds3 - Number of descriptors returned in cds3
 *
 * @author J. Ator @date 2025-11-18
*/
void
catch_bort_upds3(int *mbay, int lcds3, char (*ccds3)[6], int *nds3)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) {
        dealloc_vars_f("upds3_f");
        return;
    }

    /* Recursively call the subroutine. */
    upds3_f(mbay, lcds3, ccds3, nds3);
}

/**
 * Catch any bort error inside of subroutine pkbs1().
 *
 * @param ival - Value corresponding to s1mnem
 * @param mbay - BUFR message
 * @param s1mnem - Mnemonic for value to be written into Section 1 of BUFR message
 * @param s1mnem_str_len - Length of s1mnem string
 *
 * @author J. Ator @date 2025-11-18
*/
void
catch_bort_pkbs1(int ival, int *mbay, char *s1mnem, int s1mnem_str_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to s1mnem, for use with get_c_string_length inside of pkbs1_f. */
    s1mnem[s1mnem_str_len] = '\0';

    /* Recursively call the subroutine. */
    pkbs1_f(ival, mbay, s1mnem);
}

/**
 * Catch any bort error inside of subroutine strcpt().
 *
 * @param cf - Flag indicating whether future BUFR output messages should include a tank receipt time
 * @param iyr - Tank receipt year
 * @param imo - Tank receipt month
 * @param idy - Tank receipt day
 * @param ihr - Tank receipt hour
 * @param imi - Tank receipt minute
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_strcpt(char *cf, int iyr, int imo, int idy, int ihr, int imi)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    strcpt_f(cf, iyr, imo, idy, ihr, imi);
}

/**
 * Catch any bort error inside of subroutine rtrcpt().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param iyr - Tank receipt year
 * @param imo - Tank receipt month
 * @param idy - Tank receipt day
 * @param ihr - Tank receipt hour
 * @param imi - Tank receipt minute
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_rtrcpt(int lunit, int *iyr, int *imo, int *idy, int *ihr, int *imi, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    rtrcpt_f(lunit, iyr, imo, idy, ihr, imi, iret);
}

/**
 * Catch any bort error inside of subroutine atrcpt().
 *
 * @param msgin - BUFR message
 * @param lmsgot - Allocated length of msgot
 * @param msgot - Copy of msgin with a tank receipt time added to Section 1
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_atrcpt(int *msgin, int lmsgot, int *msgot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    atrcpt_f(msgin, lmsgot, msgot);
}

/**
 * Catch any bort error inside of subroutine dxdump().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param luprt - Fortran logical unit number for print output
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_dxdump(int lunit, int luprt)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    dxdump_f(lunit, luprt);
}

/**
 * Catch any bort error inside of subroutine ufbdmp().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param luprt - Fortran logical unit number for print output
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_ufbdmp(int lunit, int luprt)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufbdmp_f(lunit, luprt);
}

/**
 * Catch any bort error inside of subroutine ufdump().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param luprt - Fortran logical unit number for print output
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_ufdump(int lunit, int luprt)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufdump_f(lunit, luprt);
}

/**
 * Catch any bort error inside of subroutine copybf().
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_copybf(int lunin, int lunot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    copybf_f(lunin, lunot);
}

/**
 * Catch any bort error inside of subroutine copymg().
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_copymg(int lunin, int lunot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    copymg_f(lunin, lunot);
}

/**
 * Catch any bort error inside of subroutine copysb().
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_copysb(int lunin, int lunot, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    copysb_f(lunin, lunot, iret);
}

/**
 * Catch any bort error inside of subroutine ufbcpy().
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author J. Ator @date 2025-11-20
*/
void
catch_bort_ufbcpy(int lunin, int lunot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufbcpy_f(lunin, lunot);
}

/**
 * Catch any bort error inside of subroutine nemdefs().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cnemo - Mnemonic
 * @param lcn - Length of cnemo
 * @param ccelem - Element name
 * @param ccelem_str_len - Allocated length of ccelem string
 * @param ccunit - Units
 * @param ccunit_str_len - Allocated length of ccunit string
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-11-25
*/
void
catch_bort_nemdefs(int lunit, char *cnemo, int lcn, char *ccelem, int ccelem_str_len,
                   char *ccunit, int ccunit_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) {
        *iret = -1;
        return;
    }

    /* Add a trailing null to cnemo, for use with get_c_string_length inside of nemdefs_f. */
    cnemo[lcn] = '\0';

    /* Recursively call the subroutine. */
    nemdefs_f(lunit, cnemo, ccunit, ccunit_str_len, ccelem, ccelem_str_len, iret);
}

/**
 * Catch any bort error inside of subroutine nemspecs().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param cnemo - Mnemonic
 * @param lcn - Length of cnemo
 * @param nnemo - Ordinal indicator of specific mnemonic occurrence
 * @param nscl - Scale factor
 * @param nref - Reference value
 * @param nbts - Bit width
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-11-25
*/
void
catch_bort_nemspecs(int lunit, char *cnemo, int lcn, int nnemo, int *nscl, int *nref, int *nbts, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cnemo, for use with get_c_string_length inside of nemspecs_f. */
    cnemo[lcn] = '\0';

    /* Recursively call the subroutine. */
    nemspecs_f(lunit, cnemo, nnemo, nscl, nref, nbts, iret);
}

/**
 * Catch any bort error inside of subroutine readerme().
 *
 * @param mesg - BUFR message
 * @param lunit - Fortran logical unit number
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-11-25
*/
void
catch_bort_readerme(int *mesg, int lunit, char *subset, int *jdate, int subset_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    readerme_f(mesg, lunit, subset, jdate, subset_str_len, iret);
}

/**
 * Catch any bort error inside of subroutine rdmgsb().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param imsg - Message number
 * @param isub - Subset number
 *
 * @author J. Ator @date 2025-11-25
*/
void
catch_bort_rdmgsb(int lunit, int imsg, int isub)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    rdmgsb_f(lunit, imsg, isub);
}

/**
 * Catch any bort error inside of subroutine ufbmem().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param inew - Processing option
 * @param iret - Number of BUFR messages that were read and stored into internal arrays
 * @param iunit - File status
 *
 * @author J. Ator @date 2025-11-25
*/
void
catch_bort_ufbmem(int lunit, int inew, int *iret, int *iunit)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufbmem_f(lunit, inew, iret, iunit);
}

/**
 * Catch any bort error inside of subroutine ufbmex().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param lundx - Fortran logical unit number containing DX BUFR table information
 * @param inew - Processing option
 * @param iret - Number of BUFR messages that were read and stored into internal arrays
 * @param mesg - Types of BUFR messages that were read and stored into internal arrays
 *
 * @author J. Ator @date 2025-11-25
*/
void
catch_bort_ufbmex(int lunit, int lundx, int inew, int *iret, int *mesg)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufbmex_f(lunit, lundx, inew, iret, mesg);
}

/**
 * Catch any bort error inside of subroutine ufbmms().
 *
 * @param imsg - Number of BUFR message to be read
 * @param isub - Number of data subset to be read from imsg
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 *
 * @author J. Ator @date 2025-12-01
*/
void
catch_bort_ufbmms(int imsg, int isub,  char *subset, int *jdate, int subset_str_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufbmms_f(imsg, isub, subset, jdate, subset_str_len);
}

/**
 * Catch any bort error inside of subroutine ufbmns().
 *
 * @param irep - Number of data subset to be read
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param idate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 *
 * @author J. Ator @date 2025-12-01
*/
void
catch_bort_ufbmns(int irep, char *subset, int *idate, int subset_str_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufbmns_f(irep, subset, idate, subset_str_len);
}

/**
 * Catch any bort error inside of subroutine rdmemm().
 *
 * @param imsg - Number of BUFR message to be read
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_str_len - Allocated length of subset string
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-01
*/
void
catch_bort_rdmemm(int imsg, char *subset, int *jdate, int subset_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    rdmemm_f(imsg, subset, jdate, subset_str_len, iret);
}

/**
 * Catch any bort error inside of subroutine rdmems().
 *
 * @param isub - Number of data subset to be read
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-01
*/
void
catch_bort_rdmems(int isub, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    rdmems_f(isub, iret);
}

/**
 * Catch any bort error inside of subroutine ufbrms().
 *
 * @param imsg - Number of BUFR message to be read
 * @param isub - Number of data subset to be read from imsg
 * @param usr - Data values
 * @param i1 - First dimension of usr
 * @param i2 - Second dimension of usr
 * @param iret - Number of replications of cstr that were read from the data subset
 * @param cstr - String of mnemonics to read from the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-12-01
*/
void
catch_bort_ufbrms(int imsg, int isub, double *usr, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbrms_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbrms_f(imsg, isub, (void**) &usr, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine ufbtam().
 *
 * @param tab - Data values
 * @param i1 - First dimension of tab
 * @param i2 - Second dimension of tab
 * @param iret - Number of data subsets returned
 * @param cstr - String of mnemonics to read from each data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-12-01
*/
void
catch_bort_ufbtam(double *tab, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbtam_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbtam_f((void**) &tab, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine cpymem().
 *
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author J. Ator @date 2025-12-02
*/
void
catch_bort_cpymem(int lunot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    cpymem_f(lunot);
}

/**
 * Catch any bort error inside of subroutine ufbcup().
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author J. Ator @date 2025-12-02
*/
void
catch_bort_ufbcup(int lunin, int lunot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    ufbcup_f(lunin, lunot);
}

/**
 * Catch any bort error inside of subroutine stdmsg().
 *
 * @param cf - Flag indicating whether future BUFR output messages should be WMO-standard
 *
 * @author J. Ator @date 2025-12-02
*/
void
catch_bort_stdmsg(char *cf)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    stdmsg_f(cf);
}

/**
 * Catch any bort error inside of subroutine stndrd().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param msgin - BUFR message
 * @param lmsgot - Allocated length of msgot
 * @param msgot - Copy of msgin now fully WMO-standardized
 *
 * @author J. Ator @date 2025-12-02
*/
void
catch_bort_stndrd(int lunit, int *msgin, int lmsgot, int *msgot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    stndrd_f(lunit, msgin, lmsgot, msgot);
}

/**
 * Catch any bort error inside of subroutine cmpmsg().
 *
 * @param cf - Flag indicating whether future BUFR output messages should be compressed
 *
 * @author J. Ator @date 2025-12-02
*/
void
catch_bort_cmpmsg(char *cf)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    cmpmsg_f(cf);
}

/**
 * Catch any bort error inside of subroutine codflg().
 *
 * @param cf - Flag indicating whether code and flag table information should be included
 * when reading from master BUFR tables
 *
 * @author J. Ator @date 2025-12-02
*/
void
catch_bort_codflg(char *cf)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    codflg_f(cf);
}

/**
 * Catch any bort error inside of subroutine bvers().
 *
 * @param verstr - Version string
 * @param verstr_len - Allocated length of verstr
 *
 * @author J. Ator @date 2025-12-02
*/
void
catch_bort_bvers(char *verstr, int verstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) {
        dealloc_vars_f("bvers_f");
        return;
    }

    /* Recursively call the subroutine. */
    bvers_f(verstr, verstr_len);
}

/**
 * Catch any bort error inside of subroutine gettagpr().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tagch - Table B or Table D mnemonic
 * @param tagch_len - Length of tagch string
 * @param ntagch - Ordinal occurrence of tagch for which tagpr is to be returned
 * @param tagpr - Table D mnemonic
 * @param tagpr_len - Allocated length of tagpr
 * @param ntpchr - Number of characters returned in tagpr
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-03
*/
void
catch_bort_gettagpr(int lunit, char *tagch, int tagch_len, int ntagch,
                    char *tagpr, int tagpr_len, int *ntpchr, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) {
        *ntpchr = 1;
        return;
    }

    /* Add a trailing null to tagch, for use with get_c_string_length inside of gettagpr_f. */
    tagch[tagch_len] = '\0';

    /* Recursively call the subroutine. */
    gettagpr_f(lunit, tagch, ntagch, tagpr, tagpr_len, iret);

    *ntpchr = (int) strlen(tagpr);
}

/**
 * Catch any bort error inside of subroutine gettagre().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tagi - Table B mnemonic
 * @param tagi_len - Length of tagi string
 * @param ntagi - Ordinal occurrence of tagi for which tagre is to be returned
 * @param tagre - Table B mnemonic referenced by tagi via an internal bitmap
 * @param tagre_len - Allocated length of tagre
 * @param ntagre - Ordinal occurrence of tagre referenced by (ntagi)th occurrence of tagi
 * @param ntrchr - Number of characters returned in tagre
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-03
*/
void
catch_bort_gettagre(int lunit, char *tagi, int tagi_len, int ntagi,
                    char *tagre, int tagre_len, int *ntagre, int *ntrchr, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) {
        *ntrchr = 1;
        return;
    }

    /* Add a trailing null to tagi, for use with get_c_string_length inside of gettagre_f. */
    tagi[tagi_len] = '\0';

    /* Recursively call the subroutine. */
    gettagre_f(lunit, tagi, ntagi, tagre, tagre_len, ntagre, iret);

    *ntrchr = (int) strlen(tagre);
}

/**
 * Catch any bort error inside of subroutine cnved4().
 *
 * @param msgin - BUFR message
 * @param lmsgot - Allocated length of msgot
 * @param msgot - Copy of msgin now converted to edition 4
 *
 * @author J. Ator @date 2025-12-03
*/
void
catch_bort_cnved4(int *msgin, int lmsgot, int *msgot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    cnved4_f(msgin, lmsgot, msgot);
}

/**
 * Catch any bort error inside of function lcmgdf().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param subset - Table A mnemonic for type of BUFR message to be checked
 * @param subset_str_len - Length of subset string
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-03
*/
void
catch_bort_lcmgdf(int lunit, char *subset, int subset_str_len, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to subset, for use with get_c_string_length inside of lcmgdf_f. */
    subset[subset_str_len] = '\0';

    /* Recursively call the function. */
    *iret = lcmgdf_f(lunit, subset);
}

/**
 * Catch any bort error inside of subroutine setvalnb().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tagpv - Pivot mnemonic
 * @param tagpv_len - Length of tagpv string
 * @param ntagpv - Ordinal occurrence of tagpv to search for
 * @param tagnb - Nearby mnemonic
 * @param tagnb_len - Length of tagnb string
 * @param ntagnb - Ordinal occurrence of tagnb to search for
 * @param r8val - Value to be stored
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-05
*/
void
catch_bort_setvalnb(int lunit, char *tagpv, int tagpv_len, int ntagpv,
                    char *tagnb, int tagnb_len, int ntagnb, double r8val, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add trailing nulls to input strings, for use with get_c_string_length inside of setvalnb_f. */
    tagpv[tagpv_len] = '\0';
    tagnb[tagnb_len] = '\0';

    /* Recursively call the subroutine. */
    setvalnb_f(lunit, tagpv, ntagpv, tagnb, ntagnb, r8val, iret);
}

/**
 * Catch any bort error inside of function getvalnb().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tagpv - Pivot mnemonic
 * @param tagpv_len - Length of tagpv string
 * @param ntagpv - Ordinal occurrence of tagpv to search for
 * @param tagnb - Nearby mnemonic
 * @param tagnb_len - Length of tagnb string
 * @param ntagnb - Ordinal occurrence of tagnb to search for
 * @param r8val - Return value
 *
 * @author J. Ator @date 2025-12-03
*/
void
catch_bort_getvalnb(int lunit, char *tagpv, int tagpv_len, int ntagpv,
                    char *tagnb, int tagnb_len, int ntagnb, double *r8val)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add trailing nulls to input strings, for use with get_c_string_length inside of getvalnb_f. */
    tagpv[tagpv_len] = '\0';
    tagnb[tagnb_len] = '\0';

    /* Recursively call the function. */
    *r8val = getvalnb_f(lunit, tagpv, ntagpv, tagnb, ntagnb);
}

/**
 * Catch any bort error inside of subroutine getabdb().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param itab - Allocated length of ctabdb
 * @param ctabdb - Internal Table B and Table D information
 * @param jtab - Number of entries returned in ctabdb
 *
 * @author J. Ator @date 2025-12-05
*/
void
catch_bort_getabdb(int lunit, int itab, char (*ctabdb)[128], int *jtab)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) {
        dealloc_vars_f("getabdb_f");
        return;
    }

    /* Recursively call the subroutine. */
    getabdb_f(lunit, itab, ctabdb, jtab);
}

/**
 * Catch any bort error inside of subroutine ufbget().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tab - Data values
 * @param i1 - Allocated length of tab
 * @param iret - Return code
 * @param cstr - String of mnemonics to read from the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-12-05
*/
void
catch_bort_ufbget(int lunit, double *tab, int i1, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbget_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbget_f(lunit, tab, i1, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine ufbinx().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param imsg - Number of BUFR message to be read
 * @param isub - Number of data subset to be read from imsg
 * @param usr - Data values
 * @param i1 - First dimension of usr
 * @param i2 - Second dimension of usr
 * @param iret - Number of replications of cstr that were read from the data subset
 * @param cstr - String of mnemonics to read from the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-12-05
*/
void
catch_bort_ufbinx(int lunit, int imsg, int isub, double *usr, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbinx_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbinx_f(lunit, imsg, isub, (void**) &usr, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of subroutine ufbovr().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param usr - Data values
 * @param i1 - First dimension of usr
 * @param i2 - Second dimension of usr
 * @param iret - Number of replications of cstr that were written to the data subset
 * @param cstr - String of mnemonics to write to the data subset
 * @param cstr_len - Length of cstr
 *
 * @author J. Ator @date 2025-12-05
*/
void
catch_bort_ufbovr(int lunit, double *usr, int i1, int i2, int *iret, char *cstr, int cstr_len)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Add a trailing null to cstr, for use with get_c_string_length inside of ufbovr_f. */
    cstr[cstr_len] = '\0';

    /* Recursively call the subroutine. */
    ufbovr_f(lunit, (void**) &usr, i1, i2, iret, cstr);
}

/**
 * Catch any bort error inside of function ifbget().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_ifbget(int lunit, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the function. */
    *iret = ifbget_f(lunit);
}

/**
 * Catch any bort error inside of function igetsc().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_igetsc(int lunit, int *iret)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the function. */
    *iret = igetsc_f(lunit);
}

/**
 * Catch any bort error inside of subroutine wrdxtb().
 *
 * @param lundx - Fortran logical unit number containing DX BUFR table information
 * @param lunot - Fortran logical unit number for output file
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_wrdxtb(int lundx, int lunot)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    wrdxtb_f(lundx, lunot);
}

/**
 * Catch any bort error inside of subroutine mesgbf().
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param mesgtyp - Message type
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_mesgbf(int lunit, int *mesgtyp)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    mesgbf_f(lunit, mesgtyp);
}

/**
 * Catch any bort error inside of subroutine mesgbc().
 *
 * @param lunin - Fortran logical unit number for BUFR file
 * @param mesgtyp - Message type
 * @param icomp - Compression indicator
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_mesgbc(int lunin, int *mesgtyp, int *icomp)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    mesgbc_f(lunin, mesgtyp, icomp);
}

/**
 * Catch any bort error inside of subroutine invmrg().
 *
 * @param lubfi - Fortran logical unit number for input BUFR file
 * @param lubfj - Fortran logical unit number for output BUFR file
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_invmrg(int lubfi, int lubfj)
{
    /* Set the target location to which to return if a bort error is caught. */
    if ( setjmp(context) == 1 ) return;

    /* Recursively call the subroutine. */
    invmrg_f(lubfi, lubfj);
}

/**
 * Catch any bort error inside of function iupm().
 *
 * @param cbay - Character string
 * @param nbits - Number of bits to decode from cbay
 * @param iret - Decoded value
 * @param lcbay - Length of cbay
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_iupm(char *cbay, int nbits, int *iret, int lcbay)
{

    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Recursively call the function. */
    *iret = iupm_f(cbay, nbits, lcbay);
}

/**
 * Catch any bort error inside of subroutine ipkm().
 *
 * @param cbay - Character string
 * @param nbyt - Number of bytes of cbay within which to encode ival
 * @param ival - Value to encode
 * @param cbay_len - Allocated length of cbay
 *
 * @author J. Ator @date 2025-12-09
*/
void
catch_bort_ipkm(char *cbay, int nbyt, int ival, int cbay_len)
{

    /* Set the target location to which to return if a bort error is caught. */
    if (setjmp(context) == 1) return;

    /* Recursively call the subroutine. */
    ipkm_f(cbay, nbyt, ival, cbay_len);
}
