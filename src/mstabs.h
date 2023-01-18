/** @file
 *  @brief Define signatures and declare variables for internal storage
 *  of master Table B and Table D entries.
 *
 *  Since the arrays in Fortran module MODA_MSTABS are dynamically
 *  allocated and their size isn't known at compile time, then we can't
 *  directly access them from within C.  Instead, we need to allocate
 *  separate array space in C and then use subroutine cpmstabs() to copy
 *  the relevant information from the Fortran module MODA_MSTABS arrays
 *  to these C arrays at run time, in order to be able to access this
 *  information from within C.
 *
 *  @author J. Ator @date 2014-12-04
 */

#ifdef UNDERSCORE
#define cpmstabs   cpmstabs_
#endif

/** Function prototype. */
void cpmstabs( f77int *pnmtb, f77int *pibfxyn, char (*pcbscl)[4], char (*pcbsref)[12], char (*pcbbw)[4],
        char (*pcbunit)[24], char (*pcbmnem)[8], char (*pcbelem)[120], f77int *pnmtd, f77int *pidfxyn,
        char (*pcdseq)[120], char (*pcdmnem)[8], f77int *pndelem, f77int *pidefxy, f77int *maxcd );

#ifdef IN_ARALLOCC
        /** Number of master Table B entries; copied from Fortran nmtb variable. */
	f77int nmtb_c; 
        /** Bit-wise representations of master Table B FXY numbers; copied from Fortran ibfxyn array. */
	f77int *ibfxyn_c;
        /** Master Table B scale factors; copied from Fortran cbscl array. */
	char   (*cbscl_c)[4];
        /** Master Table B reference value; copied from Fortran cbsref array. */
	char   (*cbsref_c)[12];
        /** Master Table B bit widths; copied from Fortran cbbw array. */
	char   (*cbbw_c)[4];
        /** Master Table B units; copied from Fortran cbunit array. */
	char   (*cbunit_c)[24];
        /** Master Table B mnemonics; copied from Fortran cbmnem array. */
	char   (*cbmnem_c)[8];
        /** Master Table B element names; copied from Fortran cbelem array. */
	char   (*cbelem_c)[120];
        /** Number of master Table D entries; copied from Fortran nmtd variable. */
	f77int nmtd_c;
        /** Bit-wise representations of master Table D FXY numbers; copied from Fortran idfxyn array. */
	f77int *idfxyn_c;
        /** Master Table D sequence names; copied from Fortran cdseq array. */
	char   (*cdseq_c)[120];
        /** Master Table D mnemonics; copied from Fortran cdmnem array. */
	char   (*cdmnem_c)[8];
        /** Number of child descriptors for master Table D sequence; copied from Fortran ndelem array. */
	f77int *ndelem_c;
        /** Bit-wise representations of child descriptors for master Table D sequence; copied from Fortran idefxy array. */
	f77int *idefxy_c;
#else
	extern f77int nmtb_c;
	extern f77int *ibfxyn_c;
	extern char   (*cbscl_c)[4];
	extern char   (*cbsref_c)[12];
	extern char   (*cbbw_c)[4];
	extern char   (*cbunit_c)[24];
	extern char   (*cbmnem_c)[8];
	extern char   (*cbelem_c)[120];
	extern f77int nmtd_c;
	extern f77int *idfxyn_c;
	extern char   (*cdseq_c)[120];
	extern char   (*cdmnem_c)[8];
	extern f77int *ndelem_c;
	extern f77int *idefxy_c;
#endif
