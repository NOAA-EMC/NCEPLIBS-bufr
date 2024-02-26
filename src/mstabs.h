/** @file
 *  @brief Declare variables for internal storage of master Table B and Table D entries.
 *
 *  Since the arrays in Fortran module @ref moda_mstabs are dynamically
 *  allocated and their size isn't known at compile time, then we can't
 *  directly access them from within C.  Instead, we need to allocate
 *  separate array space in C and then use subroutine cpmstabs() to copy
 *  the relevant information from the module arrays
 *  to these C arrays at run time, in order to be able to access this
 *  information from within C.
 *
 *  @author J. Ator @date 2014-12-04
 */

/** @var nmtb_c
 *  Number of master Table B entries; copied from Fortran nmtb variable
 *
 *  @var ibfxyn_c
 *  WMO bit-wise representations of master Table B FXY numbers; copied from Fortran ibfxyn array
 *
 *  @var cbscl_c
 *  Master Table B scale factors; copied from Fortran cbscl array
 *
 *  @var cbsref_c
 *  Master Table B reference values; copied from Fortran cbsref array
 *
 *  @var cbbw_c
 *  Master Table B bit widths; copied from Fortran cbbw array
 *
 *  @var cbunit_c
 *  Master Table B units; copied from Fortran cbunit array
 *
 *  @var cbmnem_c
 *  Master Table B mnemonics; copied from Fortran cbmnem array
 *
 *  @var cbelem_c
 *  Master Table B element names; copied from Fortran cbelem array
 *
 *  @var nmtd_c
 *  Number of master Table D entries; copied from Fortran nmtd variable
 *
 *  @var idfxyn_c
 *  WMO bit-wise representations of master Table D FXY numbers; copied from Fortran idfxyn array
 *
 *  @var cdseq_c
 *  Master Table D sequence names; copied from Fortran cdseq array
 *
 *  @var cdmnem_c
 *  Master Table D mnemonics; copied from Fortran cdmnem array
 *
 *  @var ndelem_c
 *  Number of child descriptors for master Table D sequences; copied from Fortran ndelem array
 *
 *  @var idefxy_c
 *  WMO bit-wise representations of child descriptors for master Table D sequences; copied from Fortran idefxy array
 */
#ifdef IN_ARALLOCC
    int nmtb_c;
    int *ibfxyn_c;
    char   (*cbscl_c)[4];
    char   (*cbsref_c)[12];
    char   (*cbbw_c)[4];
    char   (*cbunit_c)[24];
    char   (*cbmnem_c)[8];
    char   (*cbelem_c)[120];
    int nmtd_c;
    int *idfxyn_c;
    char   (*cdseq_c)[120];
    char   (*cdmnem_c)[8];
    int *ndelem_c;
    int *idefxy_c;
#else
    extern int nmtb_c;
    extern int *ibfxyn_c;
    extern char   (*cbscl_c)[4];
    extern char   (*cbsref_c)[12];
    extern char   (*cbbw_c)[4];
    extern char   (*cbunit_c)[24];
    extern char   (*cbmnem_c)[8];
    extern char   (*cbelem_c)[120];
    extern int nmtd_c;
    extern int *idfxyn_c;
    extern char   (*cdseq_c)[120];
    extern char   (*cdmnem_c)[8];
    extern int *ndelem_c;
    extern int *idefxy_c;
#endif
