/**
 * @file
 * @brief Enable a number of NCEPLIBS-bufr subprograms to be called from within C and C++
 * application programs.
 *
 * This header file defines the signatures which wrap a number of native Fortran subprograms
 * in the library.  It also contains prototypes for native C functions in the library which
 * are expected to be called from C and C++ application programs.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */

#pragma once

void cobfl(char *bfl, char io);
void crbmg(char *bmg, int mxmb, int *nmb, int *iret);
void cwbmg(char *bmg, int nmb, int *iret);
void ccbfl(void);

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Open a Fortran file from a C program.
 *
 * @param unit - the integer to use as the Fortran logical unit.
 * @param filepath - path to the file we want to open.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void open_f(int unit, const char* filepath);

/**
 * Close a Fortran file from a C program.
 *
 * @param unit - the integer to use as the Fortran logical unit.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void close_f(int unit);

/**
 * Connect a new file to the library, or initialize the
 * library, or change verbosity associated with already-connected file.
 *
 * Wraps openbf() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number.
 * @param cio - cio string (ex "IN", "SEC3", and "OUT").
 * @param table_file_id - table_file unit number.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void openbf_f(int bufr_unit, const char* cio, int table_file_id);

/**
 * Close a previously opened file and disconnect it from the library.
 *
 * Wraps closbf() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to close.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void closbf_f(int bufr_unit);

/**
 * Reset the library.
 *
 * Wraps exitbufr() subroutine.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void exitbufr_f();

/**
 * Read the next message from a BUFR file.
 *
 * Wraps ireadmg() function.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 * @param subset - the subset string.
 * @param iddate - datetime of message.
 * @param subset_len - length of the subset string.
 *
 * @return
 * - 0 new BUFR message was successfully read into internal arrays.
 * - -1 there are no more BUFR messages in bufr_unit.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  int ireadmg_f(int bufr_unit, char* subset, int* iddate, int subset_len);

/**
 * Read the next data subset from a BUFR message.
 *
 * Wraps ireadsb() function.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 *
 * @return
 * - 0 new BUFR data subset was successfully read into internal arrays.
 * - -1 there are no more BUFR data subsets in the BUFR message associated with bufr_unit
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  int ireadsb_f(int bufr_unit);

/**
 * Read/write one or more data values from/to a data subset.
 *
 * Wraps ufbint() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read or write.
 * @param dim_2 - dimensionality of data to read or write.
 * @param iret - return value, length of data read.
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void ufbint_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_b_mnemonic);

/**
 * Read/write one or more data values from/to a data subset.
 *
 * Wraps ufbrep() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read or write.
 * @param dim_2 - dimensionality of data to read or write.
 * @param iret - length of data read.
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void ufbrep_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_b_mnemonic);

/**
 * Specify location of master BUFR tables on local file system.
 *
 * Wraps mtinfo() subroutine.
 *
 * @param path - the path where the WMO tables are stored.
 * @param file_unit_1 - number to use for first logical unit.
 * @param file_unit_2 - number to use for second logical unit.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void mtinfo_f(const char* path, int file_unit_1, int file_unit_2);

/**
 * Check whether a file is connected to the library.
 *
 * Wraps status() subroutine.
 *
 * @param file_unit - Fortran logical unit number of file.
 * @param lun - File ID.
 * @param il - File status.
 * @param im - Message status.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void status_f(int file_unit, int* lun, int* il, int* im);

/**
 * Get the element name and units associated with a Table B mnemonic.
 *
 * Wraps nemdefs() subroutine.
 *
 * @param file_unit - Fortran logical unit for the open file.
 * @param mnemonic - Mnemonic.
 * @param unit_c - Unit string.
 * @param unit_str_len - Unit string length.
 * @param desc_c - Description string.
 * @param desc_str_len - Description string length.
 * @param iret - 0 indicates success -1 indicates failure.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
  void nemdefs_f(int file_unit,
                 const char* mnemonic,
                 char* unit_c,
                 int unit_str_len,
                 char* desc_c,
                 int desc_str_len,
                 int* iret);

/**
 * Get the scale factor, reference value and bit width associated with a
 * specified occurrence of a Table B mnemonic.
 *
 * Wraps nemspecs() subroutine.
 *
 * @param file_unit - Fortran logical unit for the open file.
 * @param mnemonic: - Mnemonic.
 * @param mnemonic_idx - Ordinal indicator of specific mnemonic element (if repeated).
 * @param scale - Scale of element.
 * @param reference - Reference of element.
 * @param bits - Number of bits representing the element.
 * @param iret - 0 indicates success -1 indicates failure.
 *
 * @author Ronald Mclaren @date 2022-08-08
 */
  void nemspecs_f(int file_unit,
                  const char* mnemonic,
                  int mnemonic_idx,
                  int* scale,
                  int* reference,
                  int* bits,
                  int* iret);

/**
 * Get information about a descriptor.
 *
 * Wraps nemtab() subroutine.
 *
 * @param lun - File ID.
 * @param mnemonic - Mnemonic.
 * @param descriptor - The binary descriptor for the mnemonic.
 * @param table_type - Type of internal DX BUFR table ('B', 'C', or 'D').
 * @param table_idx - The table index, or 0 if not found.
 *
 * @author Ronald Mclaren @date 2022-08-16
 */
  void nemtab_f(int lun,
                const char* mnemonic,
                int* descriptor,
                char* table_type,
                int* table_idx);

/**
 * Get information about a Table B descriptor.
 *
 * Wraps nemtbb() subroutine.
 *
 * @param lun - File ID.
 * @param table_idx - Table B index.
 * @param unit_str - Unit string.
 * @param unit_str_len - Unit string length.
 * @param scale - Scale of element.
 * @param reference - Reference value of element.
 * @param bits - Number of bits representing theelement.
 *
 * @author Ronald McLaren @date 2022-08-16
 */
  void nemtbb_f(int lun,
                int table_idx,
                char* unit_str,
                int unit_str_len,
                int* scale,
                int* reference,
                int* bits);

/**
 * Get copy of the moda_tables ISC array.
 *
 * @param isc_ptr - pointer to a pointer to the ISC array.
 * @param isc_size - size of the ISC array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_isc_f(int** isc_ptr, int* isc_size);

/**
 * Get copy of the moda_tables LINK array.
 *
 * @param link_ptr - pointer to a pointer to the LINK array.
 * @param link_size - size of the LINK array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_link_f(int** link_ptr, int* link_size);

/**
 * Get copy of the moda_tables ITP array.
 *
 * @param itp_ptr - pointer to a pointer to the ITP array.
 * @param itp_size - size of the ITP array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_itp_f(int** itp_ptr, int* itp_size);

/**
 * Get copy of the moda_tables TYP array.
 *
 * @param typ_ptr - pointer to a pointer to the TYP array.
 * @param typ_len - size of each string within the TYP array.
 * @param mem_size - size of the TYP array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_typ_f(char** typ_ptr, int* typ_len, int* mem_size);

/**
 * Get copy of the moda_tables TAG array.
 *
 * @param tag_ptr - pointer to a pointer to the TAG array.
 * @param tag_len - size of each string within the TAG array.
 * @param mem_size - size of the TAG array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_tag_f(char** tag_ptr, int* tag_len, int* mem_size);

/**
 * Get copy of the moda_tables JMPB array.
 *
 * @param jmpb_ptr - pointer to a pointer to the JMPB array.
 * @param jmpb_size - size of the JMPB array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_jmpb_f(int** jmpb_ptr, int* jmpb_size);


/**
 * Get copy of the moda_tables IRF array.
 *
 * @param irf_ptr - pointer to a pointer to the IRF array.
 * @param irf_size - size of the IRF array.
 *
 * @author Ronald McLaren @date 2023-04-05
 */
  void get_irf_f(int** irf_ptr, int* irf_size);

/**
 * Get the bufr node idx for the start node of the subset.
 *
 * @param lun - File ID.
 * @param start_node - the start node of the subset.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_inode_f(int lun, int* start_node);

/**
 * Get the number of values in the current subset
 *
 * @param lun - File ID.
 * @param num_nodes - number of values in the subset.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_nval_f(int lun, int* num_nodes);

/**
 * Get pointer to the moda_usrint VAL array.
 *
 * @param lun - File ID.
 * @param val_ptr - pointer to a pointer to the VAL array.
 * @param val_size - size of the VAL array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_val_f(int lun, double** val_ptr, int* val_size);

/**
 * Get pointer to the moda_usrint INV array.
 *
 * @param lun - File ID.
 * @param inv_ptr - pointer to a pointer to the INV array.
 * @param inv_size - size of the INV array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_inv_f(int lun, int** inv_ptr, int* inv_size);

/**
 *  Function used to get long strings from the BUFR file.
 *
 * @param lunit - Fortran logical unit.
 * @param str_id - Mnemonic for the string for the source field plus the index number
 *                  (ex: 'IDMN#2')
 * @param output_str - The pre-allocated result string
 * @param output_str_len - The length of the result string
 *
 * @author Ronald McLaren @date 2023-07-03
 */

  void readlc_f(int lun, const char* str_id, char* output_str, int* output_str_len);

/**
 * Deletes the copies of the moda_tables arrays.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void delete_table_data_f();

/**
 * Read a data value from Section 0 or Section 1 of a BUFR message.
 *
 * Wraps iupbs01() function.
 *
 * @param bufr - BUFR message.
 * @param mnemonic - Value to be read from Section 0 or Section 1.
 *
 * @return - Value corresponding to mnemonic, or -1 if not found or error occurred.
 *
 *  @author J. Ator @date 2023-04-07
 */
  int iupbs01_f(int *bufr, char* mnemonic);

/**
 * Get the current value of a parameter.
 *
 * @param cprmnm - Parameter.
 *
 * @return Value of cprmnm.
 *
 * @author J. Ator @date 2023-04-07
 */
  int igetprm_f(char *cprmnm);

/**
 * Define a customized parameter value for dynamic allocation.
 *
 * @param cprmnm - Parameter.
 * @param ipval - Value to be set for cprmnm.
 *
 * @return 0 if successful, or -1 if cprmnm unknown.
 *
 * @author J. Ator @date 2023-04-07
 */
  int isetprm_f(char *cprmnm, int ipval);

/**
 * Define a customized maximum length for output BUFR messages.
 *
 * Wraps maxout() subroutine.
 *
 * @param max0 - New maximum length (in bytes) for all BUFR messages
 * written to all output files.
 *
 * @author J. Ator @date 2023-04-07
 */
  void maxout_f(int max0);

/**
 * Get the maximum length of a BUFR message that can be written to an
 * output file.
 *
 * @return Maximum length of a BUFR message that can be written to an
 * output file.
 *
 * @author J. Ator @date 2023-04-07
 */
  int igetmxby_f(void);

/**
 * Read/write an entire sequence of data values from/to a data subset.
 *
 * Wraps ufbseq() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read or write.
 * @param dim_2 - dimensionality of data to read or write.
 * @param iret - return value, length of data read.
 * @param table_d_mnemonic - Table A or Table D mnemonic.
 *
 * @author J. Ator @date 2023-04-07
 */
  void ufbseq_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_d_mnemonic);

/**
 * Read the next data subset from a BUFR file.
 *
 * Wraps ireadns() function.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 * @param subset - the subset string.
 * @param iddate - datetime of message.
 * @param subset_len - length of the subset string.
 *
 * @return
 * - 0 new BUFR data subset was successfully read into internal arrays.
 * - -1 there are no more BUFR data subsets in bufr_unit.
 *
 * @author J. Ator @date 2023-04-07
 */
  int ireadns_f(int bufr_unit, char* subset, int* iddate, int subset_len);

/**
 * Test whether a data value is "missing".
 *
 * Wraps ibfms() function.
 *
 * @param r8val - Data value.
 *
 * @return - 1 if r8val is "missing", or 0 otherwise.
 *
 *  @author J. Ator @date 2023-04-07
 */
  int ibfms_f(double r8val);

/**
 * Open a new message for output in a BUFR file that was previously
 * opened for writing.
 *
 * Wraps openmb() subroutine.
 *
 * @param bufr_unit - Fortran logical unit number to write to.
 * @param c_subset - Table A mnemonic of message.
 * @param iddate - Date-time to be stored within Section 1 of message.
 *
 * @author J. Ator @date 2023-04-07
 */
  void openmb_f(int bufr_unit, char *c_subset, int iddate);

/**
 * Get the version number of the NCEPLIBS-bufr software.
 *
 * Wraps bvers() subroutine.
 *
 * @param cverstr - Version string.
 * @param cverstr_len - Length of version string.
 *
 * @author J. Ator @date 2023-04-07
 */
  void bvers_f(char *cverstr, int cverstr_len);

/**
 * Specify the use of compression when writing BUFR messages.
 *
 * Wraps cmpmsg() subroutine.
 *
 * @param cf - Flag indicating whether future BUFR output messages are to be
 * compressed ('Y' = Yes, 'N' = No).
 *
 * @author J. Ator @date 2023-04-07
 */
  void cmpmsg_f(char *cf);

#ifdef __cplusplus
}
#endif
