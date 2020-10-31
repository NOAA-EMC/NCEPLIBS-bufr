/** @file
    @author ATOR @date 2014-12-04
*/


#ifdef DYNAMIC_ALLOCATION

#include "bufrlib.h"
#include "mstabs.h"

/**

If dynamic memory allocation is being used, then we can't
directly access the fortran module mstabs arrays from within c, so
this routine is called within bufr archive library subroutine
ireadmt to copy the relevant information from these fortran arrays
to new arrays for use within c.

@author ATOR #date 2014-12-04

 PROGRAM HISTORY LOG:
- 2014-12-04  J. ATOR    -- ORIGINAL AUTHOR

   INPUT ARGUMENT LIST:
@param[in] PMTBB      - INTEGER: NUMBER OF ENTRIES IN MASTER TABLE B ARRAYS
@param[in] PIBFXYN(*) - INTEGER: BIT-WISE REPRESENTATIONS OF FXY NUMBERS
@param[in] PCBSCL(*)  - CHARACTER*4: SCALE FACTORS
@param[in] PCBCSREF(*)- CHARACTER*12: REFERENCE VALUES
@param[in] PCBBW(*)   - CHARACTER*4: BIT WIDTHS
@param[in] PCBUNIT(*) - CHARACTER*14: UNITS
@param[in] PCBMNEM(*) - CHARACTER*8: MNEMONICS
@param[in] PCBELEM(*) - CHARACTER*120: ELEMENT NAMES
@param[in] PMTBD      - INTEGER: NUMBER OF ENTRIES IN MASTER TABLE D ARRAYS
@param[in] PIDFXYN(*) - INTEGER: BIT-WISE REPRESENTATIONS OF FXY NUMBERS
@param[in] PCDSEQ(*)  - CHARACTER*120: SEQUENCE NAMES
@param[in] PCDMNEM(*) - CHARACTER*8: MNEMONICS
@param[in] PNDELEM(*) - INTEGER: NUMBER OF ELEMENTS STORED FOR PCDSEQ
@param[in] PIDEFXY(*,*)- INTEGER: BIT-WISE REPRESENTATIONS OF FXY NUMBERS
                  FOR ELEMENTS IN PNDELEM
@param[in] MAXCD      - INTEGER: MAXIMUM NUMBER OF ELEMENTS PER PCDSEQ;
                  USED BY THE SUBROUTINE WHEN CALLING FUNCTION ICVIDX

 REMARKS:
    THIS ROUTINE CALLS:        icvidx()
    THIS ROUTINE IS CALLED BY: ireadmt()
*/
void cpmstabs(  f77int *pnmtb,
		f77int *pibfxyn, char (*pcbscl)[4],
		char (*pcbsref)[12], char (*pcbbw)[4],
		char (*pcbunit)[14], char (*pcbmnem)[8],
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
	for ( jj = 0; jj < 14; jj++ ) {
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

#endif
