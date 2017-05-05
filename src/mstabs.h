/*
** If array sizes are statically allocated, then we can directly access the
** arrays in FORTRAN MODULE MSTABS from within C.  However, if these arrays
** are dynamically allocated, meaning that their size isn't known at compile
** time, then we can't directly access these arrays from within C.  Instead,
** we'll need to allocate separate array space in C and copy the relevant
** information from the FORTRAN MODULE MSTABS arrays to these new C arrays
** at run time in order to access the information from within C.
*/

#ifdef STATIC_ALLOCATION

#   define MSTABS_BASE(var) var

    extern f77int MSTABS_BASE(nmtb);
    extern f77int MSTABS_BASE(ibfxyn)[];
    extern char   MSTABS_BASE(cbscl)[][4];
    extern char   MSTABS_BASE(cbsref)[][12];
    extern char   MSTABS_BASE(cbbw)[][4];
    extern char   MSTABS_BASE(cbunit)[][14];
    extern char   MSTABS_BASE(cbmnem)[][8];
    extern char   MSTABS_BASE(cbelem)[][120];
    extern f77int MSTABS_BASE(nmtd);
    extern f77int MSTABS_BASE(idfxyn)[];
    extern char   MSTABS_BASE(cdseq)[][120];
    extern char   MSTABS_BASE(cdmnem)[][8];
    extern f77int MSTABS_BASE(ndelem)[];
    extern f77int MSTABS_BASE(idefxy)[];

#else

#   define MSTABS_BASE(var) mstabs_newCarr_ ## var

#   ifdef IN_ARALLOCC
	f77int MSTABS_BASE(nmtb);
	f77int *MSTABS_BASE(ibfxyn);
	char   (*MSTABS_BASE(cbscl))[4];
	char   (*MSTABS_BASE(cbsref))[12];
	char   (*MSTABS_BASE(cbbw))[4];
	char   (*MSTABS_BASE(cbunit))[14];
	char   (*MSTABS_BASE(cbmnem))[8];
	char   (*MSTABS_BASE(cbelem))[120];
	f77int MSTABS_BASE(nmtd);
	f77int *MSTABS_BASE(idfxyn);
	char   (*MSTABS_BASE(cdseq))[120];
	char   (*MSTABS_BASE(cdmnem))[8];
	f77int *MSTABS_BASE(ndelem);
	f77int *MSTABS_BASE(idefxy);
#   else
	extern f77int MSTABS_BASE(nmtb);
	extern f77int *MSTABS_BASE(ibfxyn);
	extern char   (*MSTABS_BASE(cbscl))[4];
	extern char   (*MSTABS_BASE(cbsref))[12];
	extern char   (*MSTABS_BASE(cbbw))[4];
	extern char   (*MSTABS_BASE(cbunit))[14];
	extern char   (*MSTABS_BASE(cbmnem))[8];
	extern char   (*MSTABS_BASE(cbelem))[120];
	extern f77int MSTABS_BASE(nmtd);
	extern f77int *MSTABS_BASE(idfxyn);
	extern char   (*MSTABS_BASE(cdseq))[120];
	extern char   (*MSTABS_BASE(cdmnem))[8];
	extern f77int *MSTABS_BASE(ndelem);
	extern f77int *MSTABS_BASE(idefxy);
#   endif

#endif
