/** @file
 *  @brief C language interface for processing master Code/Flag table entries.
 *
 *  @author J. Ator
 *  @date 2017-11-16
 */

#include "bufrlib.h"

/** Maximum length of a meaning string for a Code/Flag table entry. */
#define MAX_MEANING_LEN 150

/**
 *  Structure to store a master Code/Flag table entry.
 */
struct code_flag_entry {
    /** Bit-wise representation of FXY number to which this entry belongs. */
    int iffxyn;
    /** Code figure or bit number. */
    int ifval;
    /** Meaning corresponding to ifval. */
    char ifmeaning[MAX_MEANING_LEN+1];
    /** Bit-wise representation of FXY number upon which this entry is
     *  dependent, if any.  Set to (-1) if no dependency.
     */
    int iffxynd;
    /** Code figure or bit number upon which this entry is dependent,
     *  if any.  Set to (-1) if no dependency.
     */
    int ifvald;
};

/** Master Code/Flag table entries. */
struct code_flag_entry *cfe;  /* will automatically initialize to NULL */

/** Maximum number of master Code/Flag table entries, counting across all
 *  individual Code/Flag tables, and counting each defined code figure
 *  (within each individual Code table) or defined bit number (within
 *  each individual Flag table) as a separate entry.
 */
int mxmtbf;

/** Number of stored master Code/Flag table entries in cfe, up to a maximum of MXMTBF. */
int nmtf;

/**
 *  Initialize memory for internal storage of master Code/Flag table entries.
 *
 *  This function initializes the internal memory structure
 *  for storage of master Code/Flag table entries, including
 *  dynamically allocating space for this structure if needed.
 *
 *  @author J. Ator  @date 2017-11-03
*/
void
inittbf(void)
{
    char brtstr[50] = "BUFRLIB: INITTBF FAILED ALLOCATING CFE";

    /*
    **  Has array space for the internal memory structure been
    **  allocated yet?
    */
    if ( cfe == NULL ) {

        mxmtbf = igetprm_f( "MXMTBF" );

        if ( ( cfe = malloc( mxmtbf * sizeof(struct code_flag_entry) ) )
                        == NULL ) {
            bort( brtstr, ( f77int ) strlen( brtstr ) );
        }
    }

    nmtf = 0;
}

/**
 * Free all dynamically-allocated memory for internal storage of master Code/Flag table entries.
 *
 * This function frees any memory that was dynamically allocated
 * during a previous call to function inittbf().
 *
 * @author J. Ator @date 2017-11-03
 */
void
dlloctbf(void)
{
    free ( cfe );

    cfe = NULL;
}

/**
 * Define a comparison between two master Code/Flag table entries.
 *
 * This function defines a comparison between two entries within the
 * internal memory structure for storage of master Code/Flag table
 * entries.  The comparison is used by the intrinsic C functions
 * qsort and bsearch, and it differs from the the comparison in
 * function cmpstia2() because it compares all of the iffxyn, ifval,
 * iffxynd and ifvald components of the structure, whereas
 * cmpstia2() only compares the iffxyn and ifval components.
 *
 * @param pe1 - First master Code/Flag table entry.
 * @param pe2 - Second master Code/Flag table entry.
 * @returns cmpstia1:
 * - -1 = pe1 is less than pe2
 * -  0 = pe1 is equal to pe2
 * -  1 = pe1 is greater than pe2
 *
 * @author J. Ator @date 2017-11-13
*/
int
cmpstia1(const void *pe1, const void *pe2)
{
    struct code_flag_entry *mype1 = ( struct code_flag_entry * ) pe1;
    struct code_flag_entry *mype2 = ( struct code_flag_entry * ) pe2;

    if ( mype1->iffxyn == mype2->iffxyn ) {
        if ( mype1->ifval == mype2->ifval ) {
            if ( mype1->iffxynd == mype2->iffxynd ) {
                if ( mype1->ifvald == mype2->ifvald ) return 0;
                return ( mype1->ifvald < mype2->ifvald ? -1 : 1 );
            }
            else {
                return ( mype1->iffxynd < mype2->iffxynd ? -1 : 1 );
            }
        }
        else {
            return ( mype1->ifval < mype2->ifval ? -1 : 1 );
        }
    }
    else {
        return ( mype1->iffxyn < mype2->iffxyn ? -1 : 1 );
    }
}

/**
 * Define a comparison between two master Code/Flag table entries.
 *
 * This function defines a comparison between two entries within the
 * internal memory structure for storage of master Code/Flag table
 * entries.  The comparison is used by the intrinsic C function
 * bsearch, and it differs from the the comparison in
 * function cmpstia1() because it only compares the iffxyn and ifval
 * components of the structure, whereas cmpstia1() compares all of
 * the iffxyn, ifval, iffxynd and ifvald components of the structure.
 *
 * @param pe1 - First master Code/Flag table entry.
 * @param pe2 - Second master Code/Flag table entry.
 * @returns cmpstia2:
 * - -1 = pe1 is less than pe2
 * -  0 = pe1 is equal to pe2
 * -  1 = pe1 is greater than pe2
 *
 * @author J. Ator @date 2017-11-13
*/
int
cmpstia2(const void *pe1, const void *pe2)
{
    struct code_flag_entry *mype1 = ( struct code_flag_entry * ) pe1;
    struct code_flag_entry *mype2 = ( struct code_flag_entry * ) pe2;

    if ( mype1->iffxyn == mype2->iffxyn ) {
        if ( mype1->ifval == mype2->ifval ) return 0;
        return ( mype1->ifval < mype2->ifval ? -1 : 1 );
    }
    else {
        return ( mype1->iffxyn < mype2->iffxyn ? -1 : 1 );
    }
}

/**
 * Store a new master Code/Flag table entry.
 *
 * This function adds a new entry to the internal memory structure for storage of
 * master Code/Flag table entries.
 *
 * @param ifxyn - Bit-wise representation of FXY number for which ival is a defined
 * code or flag table entry.
 * @param ival  - Code figure or bit number.
 * @param meaning - Meaning associated with ifxyn and ival.
 * @param lmeaning - Length (in bytes) of meaning.
 * @param idfxy - Bit-wise representation of FXY number upon which ifxyn and ival
 * depend (if any), or else set to a value of (-1).
 * @param idval - Code figure or bit number associated with idfxy and upon which ifxyn
 * and ival depend (if any), or else set to (-1) whenever idfxy is also set to (-1).
 *
 *  @author J. Ator  @date 2017-11-13
 */
void
strtbfe(int ifxyn, int ival, char *meaning, int lmeaning, int idfxy, int idval)
{
    unsigned int mnlen;

    static char brtstr[50] = "BUFRLIB: STRTBFE - MXMTBF OVERFLOW";

    /*
    **  Confirm that there's room for another entry in the structure.
    */
    if ( nmtf >= mxmtbf ) bort( brtstr, ( f77int ) strlen( brtstr ) );

    /*
    **  Store the new entry.
    */
    cfe[nmtf].iffxyn = ifxyn;
    cfe[nmtf].ifval = ival;
    mnlen = ( lmeaning > MAX_MEANING_LEN ? MAX_MEANING_LEN : lmeaning );
    strncpy( &cfe[nmtf].ifmeaning[0], meaning, mnlen );
    cfe[nmtf].ifmeaning[mnlen] = '\0';
    cfe[nmtf].iffxynd = idfxy;
    cfe[nmtf].ifvald  = idval;
    nmtf++;
}

/**
 * Sort entries within the master Code/Flag table.
 *
 * This function sorts the entries within the internal memory
 * structure for storage of master Code/Flag table entries, in
 * preparation for future searches using function srchtbf().
 *
 * @author J. Ator @date 2017-11-16
 */
void
sorttbf(void)
{
    qsort( &cfe[0], ( size_t ) nmtf, sizeof( struct code_flag_entry ),
        ( int (*) ( const void *, const void * ) ) cmpstia1 );
}

/**
 * Search for a specified master Code/Flag table entry.
 *
 * This function searches for a specified FXY number and associated
 * value (code figure or bit number) within the internal memory
 * structure for storage of master Code/Flag table entries, and if
 * found returns the associated meaning as a character string.
 *
 * The search may optionally include a specified second FXY number
 * and associated value upon which the first FXY number and its
 * associated value depend, for example when the meaning of an
 * originating sub-center value depends on the identity of the
 * originating center for which the sub-center in question is a
 * member.
 *
 * @param ifxyi - Bit-wise representation of FXY number to search for.
 * @param ivali - Value (code figure or bit number) associated with ifxyi.
 * @param ifxyd - Dependence indicator:
 * - On input, ifxyd[0] is set to the bit-wise representation of the FXY
 * number upon which ifxyi and ivali depend, or else set to (-1) if ifxyi
 * and ivali do not depend on the value associated with any other FXY number.
 * - On output, if the initial search of the master Code/Flag table was
 * unsuccessful, <b>and</b> if ifxyd[0] and ivald were both set to (-1) on
 * input, <b>and</b> if a second search of the table determines that the
 * meaning of ifxyi and ivali indeed depends on one or more other FXY numbers,
 * then the bit-wise representations of those FXY numbers are returned within
 * the first iret elements of ifxyd.
 * @param ivald - Value (code figure or bit number) associated with the FXY
 * number in ifxyd[0]; set to (-1) whenever ifxyd[0] is also set to (-1).
 * @param mxfxyd - Number of elements in ifxyd array; used by the function to
 * ensure that it doesn't overflow the array.
 * @param mxmng - Number of elements in meaning array; used by the function to
 * ensure that it doesn't overflow the string.
 * @param meaning - Meaning corresponding to ifxyi and ivali (and to ifxyd[0]
 * and ivald, if specified on input).
 * @param lnmng - Length (in bytes) of string returned in CMEANG.
 * @param iret - Return code:
 * -  0 = Meaning found and stored in meaning string.
 * - -1 = Meaning not found.
 * - >0 = Meaning not found, <b>and</b> ifxyd[0] and ivald were both set to (-1)
 * on input, <b>and</b> the meaning of ifxyi and ivali depends on the the value
 * associated with one of the FXY numbers whose bit-wise representation is
 * stored in the first iret elements of ifxyd.
 *
 * @author J. Ator  @date 2018-01-11
 */
void
srchtbf(int ifxyi, int ivali, int *ifxyd, int mxfxyd, int ivald,
        char *meaning, int mxmng, int *lnmng, int *iret)
{
    struct code_flag_entry key, *pkey, *pcfe, *pbs;

    int ipt, ii, slmng;

    *iret = -1;

    /*
    **  Initialize some values for searching the internal table.
    */
    key.iffxyn = ifxyi;
    key.ifval = ivali;
    key.iffxynd = ifxyd[0];
    key.ifvald = ivald;

    pkey = &key;
    pcfe = &cfe[0];

    /*
    **  Search for a matching entry.
    */
    pbs = ( struct code_flag_entry * ) bsearch( pkey, pcfe, ( size_t ) nmtf,
                sizeof( struct code_flag_entry ),
                ( int (*) ( const void *, const void * ) ) cmpstia1 );
    if ( pbs != NULL ) {
        /*
        **  A matching entry was found, so set the appropriate output
        **  values and return.
        */
        ipt = pbs - pcfe;
        slmng = strlen( cfe[ipt].ifmeaning );
        *lnmng = ( mxmng > slmng ? slmng : mxmng );
        strncpy( meaning, &cfe[ipt].ifmeaning[0], *lnmng );
        *iret = 0;
        return;
    }

    /*
    **  Was a particular dependency specified in the input?
    */
    if ( key.iffxynd != -1 ) {
        /*
        **  YES, so there's nothing else to do.
        */
        return;
    }

    /*
    **  NO, so check whether the given Table B descriptor and value have any
    **  dependencies, and if so then return a list of those dependencies.
    */
    pbs = ( struct code_flag_entry * ) bsearch( pkey, pcfe, ( size_t ) nmtf,
                sizeof( struct code_flag_entry ),
                ( int (*) ( const void *, const void * ) ) cmpstia2 );
    if ( pbs == NULL ) {
        /*
        **  There are no dependencies.
        */
        return;
    }

    /*
    **  Store the dependency that was returned by the secondary search.
    **  However, there may be others within the internal table, so we'll
    **  also need to check for those.
    */
    ipt = pbs - pcfe;
    *iret = 0;
    ifxyd[(*iret)++] = cfe[ipt].iffxynd;

    /*
    **  Since the internal table is sorted, check immediately before and
    **  after the returned dependency for any additional table entries which
    **  correspond to the same Table B descriptor and value, but for which the
    **  dependency is different.  If any such additional dependencies are
    **  found, return those as well.
    */
    ii = ipt - 1;
    while ( ( ii >= 0 ) &&
            ( *iret < mxfxyd ) &&
            ( cfe[ii].iffxyn == key.iffxyn ) &&
            ( cfe[ii].ifval == key.ifval ) ) {
        if ( cfe[ii].iffxynd < ifxyd[(*iret)-1] )
                ifxyd[(*iret)++] = cfe[ii].iffxynd;
        ii--;
    }
    ii = ipt + 1;
    while ( ( ii < nmtf ) &&
            ( *iret < mxfxyd ) &&
            ( cfe[ii].iffxyn == key.iffxyn ) &&
            ( cfe[ii].ifval == key.ifval ) ) {
        if ( ( cfe[ii].iffxynd > ifxyd[(*iret)-1] ) &&
             ( cfe[ii].iffxynd > cfe[ipt].iffxynd ) )
                ifxyd[(*iret)++] = cfe[ii].iffxynd;
        ii++;
    }

    return;
}
