/** @file
 *  @brief Search for a specified master Code/Flag table entry.
 */
#include "bufrlib.h"
#include "cfe.h"

/**
 *  This subroutine searches for a specified FXY number and associated
 *  value (code figure or bit number) within the internal memory
 *  structure for storage of master Code/Flag table entries, and if
 *  found returns the associated meaning as a character string.
 *
 *  <p>The search may optionally include a specified second FXY number
 *  and associated value upon which the first FXY number and its
 *  associated value depend, for example when the meaning of an
 *  originating sub-center value depends on the identity of the
 *  originating center for which the sub-center in question is a 
 *  member.
 *
 *  @author J. Ator
 *  @date 2018-01-11
 *
 *  @param[in] ifxyi - f77int*: Bitwise representation of FXY number
 *                     to search for
 *  @param[in] ivali - f77int*: Value (code figure or bit number)
 *                     associated with ifxyi
 *  @param[in,out] ifxyd - f77int(*):
 *                         - On input, the first element is set to
 *                           the bitwise representation of the FXY
 *                           number upon which ifxyi and ivali depend,
 *                           or else set to (-1) if ifxyi and ivali do
 *                           not depend on the value associated with
 *                           any other FXY number
 *                         - On output, if the initial search of the
 *                           master Code/Flag table was unsuccessful,
 *                           <b>and</b> if both ivald and the first
 *                           element of this array were both set to (-1)
 *                           on input, <b>and</b> if a second search of
 *                           the table determines that the meaning of
 *                           ifxyi and ivali indeed depends on one or
 *                           more other FXY numbers, then the bitwise
 *                           representations of those FXY numbers are
 *                           returned within the first iret elements
 *  @param[in] ivald - f77int*: Value (code figure or bit number)
 *                     associated with the FXY number in the first
 *                     element of ifxyd; set to (-1) whenever the
 *                     first element of ifxyd is also set to (-1)
 *  @param[in] mxfxyd - f77int*: Dimensioned size (in f77ints) of
 *                      ifxyd; used by the subroutine to ensure that
 *                      it doesn't overflow the ifxyd array
 *  @param[in] mxmng - f77int*: Dimensioned size (in bytes) of meaning
 *                     string; used by the subroutine to ensure that
 *                     it doesn't overflow the meaning string
 *  @param[out] meaning - char*: Meaning corresponding to ifxyi and
 *                        ivali (and to ivald and the first element
 *                        of ifxyd, if specified on input)
 *  @param[out] lnmng - f77int*: Length (in bytes) of string returned
 *                      in CMEANG
 *  @param[out] iret - f77int*: return code
 *                     -  0 = meaning found and stored in meaning string
 *                     - -1 = meaning not found
 *                     - >0 = meaning not found, <b>and</b> ivald and
 *                            the first element of ifxyd were both set
 *                            to (-1) on input, <b>and</b> the meaning
 *                            of ifxyi and ivali depends on the the
 *                            value associated with one of the FXY
 *                            numbers whose bitwise representation is
 *                            stored in the first iret elements of ifxyd
 *
 * <b>Program history log:</b>
 * - 2018-01-11  J. Ator    -- Original author
 */
void srchtbf( f77int *ifxyi, f77int *ivali, f77int *ifxyd, f77int *mxfxyd, f77int *ivald,
	      char *meaning, f77int *mxmng, f77int *lnmng, f77int *iret )
{
	struct code_flag_entry key, *pkey, *pcfe, *pbs;

	int ipt, ii, slmng;

	*iret = -1;

	/*
	**  Initialize some values for searching the internal table.
	*/

	key.iffxyn = *ifxyi;
	key.ifval = *ivali;
	key.iffxynd = ifxyd[0];
	key.ifvald = *ivald;

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
	    *lnmng = ( *mxmng > slmng ? slmng : *mxmng );
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
		( *iret < *mxfxyd ) &&
		( cfe[ii].iffxyn == key.iffxyn ) &&
		( cfe[ii].ifval == key.ifval ) ) {
	    if ( cfe[ii].iffxynd < ifxyd[(*iret)-1] )
		    ifxyd[(*iret)++] = cfe[ii].iffxynd;
	    ii--;
	}
	ii = ipt + 1;
	while ( ( ii < nmtf ) &&
		( *iret < *mxfxyd ) &&
		( cfe[ii].iffxyn == key.iffxyn ) &&
		( cfe[ii].ifval == key.ifval ) ) {
	    if ( ( cfe[ii].iffxynd > ifxyd[(*iret)-1] ) &&
		 ( cfe[ii].iffxynd > cfe[ipt].iffxynd ) )
		    ifxyd[(*iret)++] = cfe[ii].iffxynd;
	    ii++;
	}

	return;
}
