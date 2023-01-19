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

void cpmstabs( f77int *, f77int *, char (*)[4], char (*)[12], char (*)[4],
        char (*)[24], char (*)[8], char (*)[120], f77int *, f77int *,
        char (*)[120], char (*)[8], f77int *, f77int *, f77int * );

/** ??? @return ??? */
#define MSTABS_BASE(var) mstabs_newCarr_ ## var 

#ifdef IN_ARALLOCC
/**
 * ???
 *
 * @return ???
 */
	f77int MSTABS_BASE(nmtb); 
	f77int *MSTABS_BASE(ibfxyn); /**< ??? */
	char   (*MSTABS_BASE(cbscl))[4]; /**< ??? */
	char   (*MSTABS_BASE(cbsref))[12]; /**< ??? */
	char   (*MSTABS_BASE(cbbw))[4]; /**< ??? */
	char   (*MSTABS_BASE(cbunit))[24]; /**< ??? */
	char   (*MSTABS_BASE(cbmnem))[8]; /**< ??? */
	char   (*MSTABS_BASE(cbelem))[120]; /**< ??? */
	f77int MSTABS_BASE(nmtd); /**< ??? */
	f77int *MSTABS_BASE(idfxyn); /**< ??? */
	char   (*MSTABS_BASE(cdseq))[120]; /**< ??? */
	char   (*MSTABS_BASE(cdmnem))[8]; /**< ??? */
	f77int *MSTABS_BASE(ndelem); /**< ??? */
	f77int *MSTABS_BASE(idefxy); /**< ??? */
#else
/**
 * ???
 *
 * @return ???
 */
	extern f77int MSTABS_BASE(nmtb); /**< ??? */
	extern f77int *MSTABS_BASE(ibfxyn); /**< ???  @return ???*/
	extern char   (*MSTABS_BASE(cbscl))[4]; /**< ???  @return ???*/
	extern char   (*MSTABS_BASE(cbsref))[12]; /**< ???  @return ???*/
	extern char   (*MSTABS_BASE(cbbw))[4]; /**< ??? @return ??? */
	extern char   (*MSTABS_BASE(cbunit))[24]; /**< ???  @return ???*/
	extern char   (*MSTABS_BASE(cbmnem))[8]; /**< ???  @return ???*/
	extern char   (*MSTABS_BASE(cbelem))[120]; /**< ???  @return ???*/
	extern f77int MSTABS_BASE(nmtd); /**< ???  @return ???*/
	extern f77int *MSTABS_BASE(idfxyn); /**< ???  @return ???*/
	extern char   (*MSTABS_BASE(cdseq))[120]; /**< ???  @return ???*/
	extern char   (*MSTABS_BASE(cdmnem))[8]; /**< ???  @return ???*/
	extern f77int *MSTABS_BASE(ndelem); /**< ???  @return ???*/
	extern f77int *MSTABS_BASE(idefxy); /**< ???  @return ???*/
#endif
