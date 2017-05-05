/*$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CPMSTABS
C   PRGMMR: ATOR             ORG: NP12       DATE: 2014-12-04
C
C ABSTRACT:  IF DYNAMIC MEMORY ALLOCATION IS BEING USED, THEN WE CAN'T
C   DIRECTLY ACCESS THE FORTRAN MODULE MSTABS ARRAYS FROM WITHIN C, SO
C   THIS ROUTINE IS CALLED WITHIN BUFR ARCHIVE LIBRARY SUBROUTINE READMT
C   TO COPY THE RELEVANT INFORMATION FROM THESE FORTRAN ARRAYS TO
C   NEW ARRAYS FOR USE WITHIN C.
C
C PROGRAM HISTORY LOG:
C 2014-12-04  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL CPMSTABS ( PNMTB, PIBFXYN, PCBSCL, PCBSREF, PCBBW,
C                           PCBUNIT, PCBMNEM, PCBELEM,
C                           PNMTD, PIDFXYN, PCDSEQ, PCDMNEM, PNDELEM,
C                           PIDEFXY, MAXCD)
C   INPUT ARGUMENT LIST:
C     PMTBB      - INTEGER: NUMBER OF ENTRIES IN MASTER TABLE B ARRAYS
C     PIBFXYN(*) - INTEGER: BIT-WISE REPRESENTATIONS OF FXY NUMBERS
C     PCBSCL(*)  - CHARACTER*4: SCALE FACTORS
C     PCBCSREF(*)- CHARACTER*12: REFERENCE VALUES
C     PCBBW(*)   - CHARACTER*4: BIT WIDTHS
C     PCBUNIT(*) - CHARACTER*14: UNITS
C     PCBMNEM(*) - CHARACTER*8: MNEMONICS
C     PCBELEM(*) - CHARACTER*120: ELEMENT NAMES
C     PMTBD      - INTEGER: NUMBER OF ENTRIES IN MASTER TABLE D ARRAYS
C     PIDFXYN(*) - INTEGER: BIT-WISE REPRESENTATIONS OF FXY NUMBERS
C     PCDSEQ(*)  - CHARACTER*120: SEQUENCE NAMES
C     PCDMNEM(*) - CHARACTER*8: MNEMONICS
C     PNDELEM(*) - INTEGER: NUMBER OF ELEMENTS STORED FOR PCDSEQ
C    PIDEFXY(*,*)- INTEGER: BIT-WISE REPRESENTATIONS OF FXY NUMBERS
C                  FOR ELEMENTS IN PNDELEM
C     MAXCD      - INTEGER: MAXIMUM NUMBER OF ELEMENTS PER PCDSEQ;
C                  USED BY THE SUBROUTINE WHEN CALLING FUNCTION ICVIDX
C
C REMARKS:
C    THIS ROUTINE CALLS:        ICVIDX
C    THIS ROUTINE IS CALLED BY: READMT
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: C
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$*/

#ifdef DYNAMIC_ALLOCATION

#include "bufrlib.h"
#include "mstabs.h"

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
