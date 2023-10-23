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

#ifdef IN_ARALLOCC
        /** Number of master Table B entries; copied from Fortran nmtb variable. */
        int nmtb_c;
        /** WMO bit-wise representations of master Table B FXY numbers; copied from Fortran ibfxyn array. */
        int *ibfxyn_c;
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
        int nmtd_c;
        /** WMO bit-wise representations of master Table D FXY numbers; copied from Fortran idfxyn array. */
        int *idfxyn_c;
        /** Master Table D sequence names; copied from Fortran cdseq array. */
        char   (*cdseq_c)[120];
        /** Master Table D mnemonics; copied from Fortran cdmnem array. */
        char   (*cdmnem_c)[8];
        /** Number of child descriptors for master Table D sequence; copied from Fortran ndelem array. */
        int *ndelem_c;
        /** WMO bit-wise representations of child descriptors for master Table D sequence; copied from Fortran idefxy array. */
        int *idefxy_c;
#else
        /** Number of master Table B entries; copied from Fortran nmtb variable. */
        extern int nmtb_c;
        /** WMO bit-wise representations of master Table B FXY numbers; copied from Fortran ibfxyn array. */
        extern int *ibfxyn_c;
        /** Master Table B scale factors; copied from Fortran cbscl array. */
        extern char   (*cbscl_c)[4];
        /** Master Table B reference value; copied from Fortran cbsref array. */
        extern char   (*cbsref_c)[12];
        /** Master Table B bit widths; copied from Fortran cbbw array. */
        extern char   (*cbbw_c)[4];
        /** Master Table B units; copied from Fortran cbunit array. */
        extern char   (*cbunit_c)[24];
        /** Master Table B mnemonics; copied from Fortran cbmnem array. */
        extern char   (*cbmnem_c)[8];
        /** Master Table B element names; copied from Fortran cbelem array. */
        extern char   (*cbelem_c)[120];
        /** Number of master Table D entries; copied from Fortran nmtd variable. */
        extern int nmtd_c;
        /** WMO bit-wise representations of master Table D FXY numbers; copied from Fortran idfxyn array. */
        extern int *idfxyn_c;
        /** Master Table D sequence names; copied from Fortran cdseq array. */
        extern char   (*cdseq_c)[120];
        /** Master Table D mnemonics; copied from Fortran cdmnem array. */
        extern char   (*cdmnem_c)[8];
        /** Number of child descriptors for master Table D sequence; copied from Fortran ndelem array. */
        extern int *ndelem_c;
        /** WMO bit-wise representations of child descriptors for master Table D sequence; copied from Fortran idefxy array. */
        extern int *idefxy_c;
#endif
