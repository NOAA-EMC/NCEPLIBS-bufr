/** @file
 *  @brief Copy master Table B and Table D information from
 *  Fortran arrays to C arrays within internal memory.
 *  @author J. Ator @date 2014-12-04
 */
#include "bufrlib.h"
#include "mstabs.h"

/**
 *  Copy relevant information from the Fortran
 *  module @ref moda_mstabs arrays to new arrays within C, for use
 *  whenever arrays are dynamically allocated at run time, and in
 *  which case we can't directly access those Fortran module
 *  arrays from within C.
 *
 *  All arguments to this subroutine are input.
 *
 *  @param nmtb - Number of master Table B entries
 *  @param ibfxyn - WMO bit-wise representations of master Table B FXY numbers
 *  @param cbscl - Master Table B scale factors
 *  @param cbsref - Master Table B reference values
 *  @param cbbw - Master Table B bit widths
 *  @param cbunit - Master Table B units
 *  @param cbmnem - Master Table B mnemonics
 *  @param cbelem - Master Table B element names
 *  @param nmtd - Number of master Table D entries
 *  @param idfxyn - WMO bit-wise representations of master Table D FXY numbers
 *  @param cdseq - Master Table D sequence names
 *  @param cdmnem - Master Table D mnemonics
 *  @param ndelem - Number of child descriptors for master Table D sequence
 *  @param idefxy - WMO bit-wise representations of child descriptors for master Table D sequence
 *  @param maxcd - Maximum number of child descriptors for a master Table D sequence
 *
 *  @author J. Ator @date 2014-12-04
*/
void
cpmstabs(int nmtb, int *ibfxyn, char (*cbscl)[4], char (*cbsref)[12], char (*cbbw)[4],
         char (*cbunit)[24], char (*cbmnem)[8], char (*cbelem)[120], int nmtd,
         int *idfxyn, char (*cdseq)[120], char (*cdmnem)[8], int *ndelem,
         int *idefxy, int maxcd)
{

    int ii, jj, idx;

    nmtb_c = nmtb;
    for ( ii = 0; ii < nmtb; ii++ ) {
        ibfxyn_c[ii] = ibfxyn[ii];
        for ( jj = 0; jj < 4; jj++ ) {
            cbscl_c[ii][jj] = cbscl[ii][jj];
            cbbw_c[ii][jj] = cbbw[ii][jj];
        }
        for ( jj = 0; jj < 8; jj++ ) {
            cbmnem_c[ii][jj] = cbmnem[ii][jj];
        }
        for ( jj = 0; jj < 12; jj++ ) {
            cbsref_c[ii][jj] = cbsref[ii][jj];
        }
        for ( jj = 0; jj < 24; jj++ ) {
            cbunit_c[ii][jj] = cbunit[ii][jj];
        }
        for ( jj = 0; jj < 120; jj++ ) {
            cbelem_c[ii][jj] = cbelem[ii][jj];
        }
    }

    nmtd_c = nmtd;
    for ( ii = 0; ii < nmtd; ii++ ) {
        idfxyn_c[ii] = idfxyn[ii];
        ndelem_c[ii] = ndelem[ii];
        for ( jj = 0; jj < ndelem[ii]; jj++ ) {
            idx = icvidx( ii, jj, maxcd );
            idefxy_c[idx] = idefxy[idx];
        }
        for ( jj = 0; jj < 8; jj++ ) {
            cdmnem_c[ii][jj] = cdmnem[ii][jj];
        }
        for ( jj = 0; jj < 120; jj++ ) {
            cdseq_c[ii][jj] = cdseq[ii][jj];
        }
    }

}
