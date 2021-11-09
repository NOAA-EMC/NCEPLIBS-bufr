/** @file
 *  @brief Copy master Table B and Table D information from
 *  Fortran arrays to C arrays within internal memory.
 */
#include "bufrlib.h"
#include "mstabs.h"

/**
 *  This subroutine copies relevant information from the Fortran
 *  module MODA_MSTABS arrays to new arrays within C, for use
 *  whenever arrays are dynamically allocated at run time and in
 *  which case we can't directly access the Fortran module
 *  MODA_MSTABS arrays from within C.
 *
 *  @author J. Ator
 *  @date 2014-12-04
 *
 *  @param[in] pnmtb -- f77int*: Number of master Table B entries
 *  @param[in] pibfxyn -- f77int*: Bit-wise representations of
 *                        master Table B FXY numbers
 *  @param[in] pcbscl -- char(*)[4]: Master Table B scale factors
 *  @param[in] pcbsref -- char(*)[12]: Master Table B reference
 *                        values
 *  @param[in] pcbbw -- char(*)[4]: Master Table B bit widths
 *  @param[in] pcbunit -- char(*)[24]: Master Table B units
 *  @param[in] pcbmnem -- char(*)[8]: Master Table B mnemonics
 *  @param[in] pcbelem -- char(*)[120]: Master Table B element names
 *  @param[in] pnmtd -- f77int*: Number of master Table D entries
 *  @param[in] pidfxyn -- f77int*: Bit-wise representations of
 *                        master Table D FXY numbers
 *  @param[in] pcdseq -- char(*)[120]: Master Table D sequence names
 *  @param[in] pcdmnem -- char(*)[8]: Master Table D mnemonics
 *  @param[in] pndelem -- f77int*: Number of child descriptors for
 *                        master Table D sequence
 *  @param[in] pidefxy -- f77int*: Bit-wise representations of
 *                        child descriptors for master Table D
 *                        sequence
 *  @param[in] maxcd -- f77int*: Maximum number of child descriptors
 *                      for a master Table D sequence
 *
 * <b>Program history log:</b>
 * | Date | Programmer | Comments |
 * | -----|------------|----------|
 * | 2014-12-04 | J. Ator | Original author |
 * | 2021-05-17 | J. Ator | Allow up to 24 characters in cbunit |
*/
void cpmstabs(  f77int *pnmtb,
		f77int *pibfxyn, char (*pcbscl)[4],
		char (*pcbsref)[12], char (*pcbbw)[4],
		char (*pcbunit)[24], char (*pcbmnem)[8],
		char (*pcbelem)[120],
		f77int *pnmtd,
		f77int *pidfxyn, char (*pcdseq)[120],
		char (*pcdmnem)[8], f77int *pndelem,
		f77int *pidefxy, f77int *maxcd )
{

    f77int ii, jj, idx;

    MSTABS_BASE(nmtb) = *pnmtb;
    for ( ii = 0; ii < *pnmtb; ii++ ) {
	MSTABS_BASE(ibfxyn)[ii] = pibfxyn[ii];
	for ( jj = 0; jj < 4; jj++ ) {
	    MSTABS_BASE(cbscl)[ii][jj] = pcbscl[ii][jj];
	    MSTABS_BASE(cbbw)[ii][jj] = pcbbw[ii][jj];
	}
	for ( jj = 0; jj < 8; jj++ ) {
	    MSTABS_BASE(cbmnem)[ii][jj] = pcbmnem[ii][jj];
	}
	for ( jj = 0; jj < 12; jj++ ) {
	    MSTABS_BASE(cbsref)[ii][jj] = pcbsref[ii][jj];
	}
	for ( jj = 0; jj < 24; jj++ ) {
	    MSTABS_BASE(cbunit)[ii][jj] = pcbunit[ii][jj];
	}
	for ( jj = 0; jj < 120; jj++ ) {
	    MSTABS_BASE(cbelem)[ii][jj] = pcbelem[ii][jj];
	}
    }

    MSTABS_BASE(nmtd) = *pnmtd;
    for ( ii = 0; ii < *pnmtd; ii++ ) {
	MSTABS_BASE(idfxyn)[ii] = pidfxyn[ii];
	MSTABS_BASE(ndelem)[ii] = pndelem[ii];
	for ( jj = 0; jj < pndelem[ii]; jj++ ) {
	    idx = icvidx( &ii, &jj, maxcd );
	    MSTABS_BASE(idefxy)[idx] = pidefxy[idx];
	}
	for ( jj = 0; jj < 8; jj++ ) {
	    MSTABS_BASE(cdmnem)[ii][jj] = pcdmnem[ii][jj];
	}
	for ( jj = 0; jj < 120; jj++ ) {
	    MSTABS_BASE(cdseq)[ii][jj] = pcdseq[ii][jj];
	}
    }

}

