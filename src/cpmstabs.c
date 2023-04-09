/** @file
 *  @brief Copy master Table B and Table D information from
 *  Fortran arrays to C arrays within internal memory.
 *  @author J. Ator @date 2014-12-04
 */
#include "bufrlib.h"
#include "mstabs.h"

/**
 *  This subroutine copies relevant information from the Fortran
 *  module MODA_MSTABS arrays to new arrays within C, for use
 *  whenever arrays are dynamically allocated at run time, and in
 *  which case we can't directly access the Fortran module
 *  MODA_MSTABS arrays from within C.
 *
 *  All arguments to this subroutine are input.
 *
 *  @param pnmtb - Number of master Table B entries
 *  @param pibfxyn - Bit-wise representations of master Table B FXY numbers
 *  @param pcbscl - Master Table B scale factors
 *  @param pcbsref - Master Table B reference values
 *  @param pcbbw - Master Table B bit widths
 *  @param pcbunit - Master Table B units
 *  @param pcbmnem - Master Table B mnemonics
 *  @param pcbelem - Master Table B element names
 *  @param pnmtd - Number of master Table D entries
 *  @param pidfxyn - Bit-wise representations of master Table D FXY numbers
 *  @param pcdseq - Master Table D sequence names
 *  @param pcdmnem - Master Table D mnemonics
 *  @param pndelem - Number of child descriptors for master Table D sequence
 *  @param pidefxy - Bit-wise representations of child descriptors for master Table D sequence
 *  @param maxcd - Maximum number of child descriptors for a master Table D sequence
 *
 *  @author J. Ator @date 2014-12-04
*/
void
cpmstabs(f77int *pnmtb, f77int *pibfxyn, char (*pcbscl)[4], char (*pcbsref)[12], char (*pcbbw)[4],
         char (*pcbunit)[24], char (*pcbmnem)[8], char (*pcbelem)[120], f77int *pnmtd,
         f77int *pidfxyn, char (*pcdseq)[120], char (*pcdmnem)[8], f77int *pndelem,
         f77int *pidefxy, f77int *maxcd)
{

    int ii, jj, idx;

    nmtb_c = *pnmtb;
    for ( ii = 0; ii < *pnmtb; ii++ ) {
        ibfxyn_c[ii] = pibfxyn[ii];
        for ( jj = 0; jj < 4; jj++ ) {
            cbscl_c[ii][jj] = pcbscl[ii][jj];
            cbbw_c[ii][jj] = pcbbw[ii][jj];
        }
        for ( jj = 0; jj < 8; jj++ ) {
            cbmnem_c[ii][jj] = pcbmnem[ii][jj];
        }
        for ( jj = 0; jj < 12; jj++ ) {
            cbsref_c[ii][jj] = pcbsref[ii][jj];
        }
        for ( jj = 0; jj < 24; jj++ ) {
            cbunit_c[ii][jj] = pcbunit[ii][jj];
        }
        for ( jj = 0; jj < 120; jj++ ) {
            cbelem_c[ii][jj] = pcbelem[ii][jj];
        }
    }

    nmtd_c = *pnmtd;
    for ( ii = 0; ii < *pnmtd; ii++ ) {
        idfxyn_c[ii] = pidfxyn[ii];
        ndelem_c[ii] = pndelem[ii];
        for ( jj = 0; jj < pndelem[ii]; jj++ ) {
            idx = icvidx( ii, jj, (int) *maxcd );
            idefxy_c[idx] = pidefxy[idx];
        }
        for ( jj = 0; jj < 8; jj++ ) {
            cdmnem_c[ii][jj] = pcdmnem[ii][jj];
        }
        for ( jj = 0; jj < 120; jj++ ) {
            cdseq_c[ii][jj] = pcdseq[ii][jj];
        }
    }

}
