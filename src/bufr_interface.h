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

void cobfl(const char *bfl, char io);
void crbmg(char *bmg, int mxmb, int *nmb, int *iret);
void cwbmg(const char *bmg, int nmb, int *iret);
void ccbfl(void);

/** Size of a character string needed to store a library version number. */
#define VERS_STR_LEN 8

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
void open_f(int unit, const char *filepath);

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
void openbf_f(int bufr_unit, const char *cio, int table_file_id);

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
int ireadmg_f(int bufr_unit, char *subset, int *iddate, int subset_len);

/**
 * Read the next message from a BUFR file.
 *
 * Wraps readmg() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from
 * @param subset - the subset string
 * @param iddate - datetime of message
 * @param subset_len - length of the subset string
 * @param ires - return code:
 * - 0 new BUFR message was successfully read into internal arrays.
 * - -1 there are no more BUFR messages in bufr_unit.
 *
 * @author Jeff Ator @date 2025-08-25
 */
void readmg_f(int bufr_unit, char *subset, int *iddate, int subset_len, int *ires);

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
 * Read the next data subset from a BUFR message.
 *
 * Wraps readsb() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 * @param ires - return code:
 * - 0 new BUFR data subset was successfully read into internal arrays.
 * - -1 there are no more BUFR data subsets in the BUFR message associated with bufr_unit
 *
 * @author Jeff Ator @date 2025-09-05
 */
void readsb_f(int bufr_unit, int *ires);

/**
 * Write the next data subset to a BUFR message.
 *
 * Wraps writsb() subroutine.
 *
 * @param bufr_unit - Fortran logical unit number to write to
 *
 * @author Jeff Ator @date 2025-10-20
 */
void writsb_f(int bufr_unit);

/**
 * Write the next data subset to a BUFR message, and return a copy of any completed message.
 *
 * Wraps writsa() subroutine.
 *
 * @param bufr_unit - Fortran logical unit number to write to
 * @param bufr_len - Allocated length of bufr array
 * @param bufr - BUFR message
 * @param nbufr - Number of integers returned in bufr array, or 0 if no message was returned
 *
 * @author Jeff Ator @date 2025-10-20
 */
void writsa_f(int bufr_unit, int bufr_len, int *bufr, int *nbufr);

/**
 * Read/write one or more data values from/to a data subset.
 *
 * Wraps ufbint() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from or write to.
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read or write.
 * @param dim_2 - dimensionality of data to read or write.
 * @param iret - return value, length of data read or written.
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
void ufbint_f(int bufr_unit, void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_b_mnemonic);

/**
 * Read/write one or more data values from/to a data subset.
 *
 * Wraps ufbrep() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from or write to.
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read or write.
 * @param dim_2 - dimensionality of data to read or write.
 * @param iret - length of data read or written.
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author Ronald Mclaren @date 2020-07-29
 */
void ufbrep_f(int bufr_unit, void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_b_mnemonic);

/**
 * Read/write one or more data values from/to a data subset.
 *
 * Wraps ufbstp() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from or write to.
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read or write.
 * @param dim_2 - dimensionality of data to read or write.
 * @param iret - length of data read or written.
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author Jeff Ator @date 2025-10-24
 */
void ufbstp_f(int bufr_unit, void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_b_mnemonic);

/**
 * Read one or more data values from a data subset.
 *
 * Wraps ufbevn() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read
 * @param dim_2 - dimensionality of data to read
 * @param dim_3 - dimensionality of data to read
 * @param iret - return value, length of data read.
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author J. Ator @date 2025-11-05
 */
void ufbevn_f(int bufr_unit, void **c_data, int dim_1, int dim_2, int dim_3,
              int *iret, const char *table_b_mnemonic);

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
void mtinfo_f(const char *path, int file_unit_1, int file_unit_2);

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
void status_f(int file_unit, int *lun, int *il, int *im);

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
               const char *mnemonic,
               char *unit_c,
               int unit_str_len,
               char *desc_c,
               int desc_str_len,
               int *iret);

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
                const char *mnemonic,
                int mnemonic_idx,
                int *scale,
                int *reference,
                int *bits,
                int *iret);

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
              const char *mnemonic,
              int *descriptor,
              char *table_type,
              int *table_idx);

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
                char *unit_str,
                int unit_str_len,
                int *scale,
                int *reference,
                int *bits);

/**
 * Get copy of the moda_tables ISC array.
 *
 * @param isc_ptr - pointer to a pointer to the ISC array.
 * @param isc_size - size of the ISC array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_isc_f(int **isc_ptr, int *isc_size);

/**
 * Get copy of the moda_tables LINK array.
 *
 * @param link_ptr - pointer to a pointer to the LINK array.
 * @param link_size - size of the LINK array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_link_f(int **link_ptr, int *link_size);

/**
 * Get copy of the moda_tables ITP array.
 *
 * @param itp_ptr - pointer to a pointer to the ITP array.
 * @param itp_size - size of the ITP array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_itp_f(int **itp_ptr, int *itp_size);

/**
 * Get copy of the moda_tables TYP array.
 *
 * @param typ_ptr - pointer to a pointer to the TYP array.
 * @param typ_len - size of each string within the TYP array.
 * @param mem_size - size of the TYP array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_typ_f(char **typ_ptr, int *typ_len, int *mem_size);

/**
 * Get copy of the moda_tables TAG array.
 *
 * @param tag_ptr - pointer to a pointer to the TAG array.
 * @param tag_len - size of each string within the TAG array.
 * @param mem_size - size of the TAG array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_tag_f(char **tag_ptr, int *tag_len, int *mem_size);

/**
 * Get copy of the moda_tables JMPB array.
 *
 * @param jmpb_ptr - pointer to a pointer to the JMPB array.
 * @param jmpb_size - size of the JMPB array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_jmpb_f(int **jmpb_ptr, int *jmpb_size);


/**
 * Get copy of the moda_tables IRF array.
 *
 * @param irf_ptr - pointer to a pointer to the IRF array.
 * @param irf_size - size of the IRF array.
 *
 * @author Ronald McLaren @date 2023-04-05
 */
void get_irf_f(int **irf_ptr, int *irf_size);

/**
 * Get the bufr node idx for the start node of the subset.
 *
 * @param lun - File ID.
 * @param start_node - the start node of the subset.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_inode_f(int lun, int *start_node);

/**
 * Get the number of values in the current subset
 *
 * @param lun - File ID.
 * @param num_nodes - number of values in the subset.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_nval_f(int lun, int *num_nodes);

/**
 * Get pointer to the moda_usrint VAL array.
 *
 * @param lun - File ID.
 * @param val_ptr - pointer to a pointer to the VAL array.
 * @param val_size - size of the VAL array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_val_f(int lun, double **val_ptr, int *val_size);

/**
 * Get pointer to the moda_usrint INV array.
 *
 * @param lun - File ID.
 * @param inv_ptr - pointer to a pointer to the INV array.
 * @param inv_size - size of the INV array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
void get_inv_f(int lun, int **inv_ptr, int *inv_size);

/**
 * Read a long string from the BUFR file.
 *
 * @param lunit - Fortran logical unit.
 * @param str_id - Mnemonic for the string for the source field plus the index number
 *                  (ex: 'IDMN#2')
 * @param output_str - The pre-allocated result string
 * @param output_str_len - Size of the result string buffer
 *
 * @author Ronald McLaren @date 2023-07-03
 */
void readlc_f(int lunit, const char *str_id, char *output_str, int output_str_len);

/**
 * Write a long string to the BUFR file.
 *
 * @param lunit - Fortran logical unit.
 * @param str - Mnemonic for the string for the source field plus the index number
 *                  (ex: 'IDMN#2')
 * @param chr - Value corresponding to str
 *
 * @author Jeff Ator @date 2025-10-24
 */
void writlc_f(int lunit, const char *str, const char *chr);

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
int iupbs01_f(int *bufr, const char *mnemonic);

/**
 * Get the current value of a parameter.
 *
 * @param cprmnm - Parameter.
 *
 * @return Value of cprmnm.
 *
 * @author J. Ator @date 2023-04-07
 */
int igetprm_f(const char *cprmnm);

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
int isetprm_f(const char *cprmnm, int ipval);

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
 * Explicitly initialize delayed replication factors for writing to a data subset
 *
 * Wraps drfini() subroutine.
 *
 * @param bufr_unit - Fortran logical unit number to write to
 * @param mdrf - Array of delayed replication factors
 * @param ndrf - Number of delayed replication factors in mdrf
 * @param table_d_mnemonic - Table D mnemonic
 *
 * @author Jeff Ator @date 2025-10-28
*/
void drfini_f(int bufr_unit, int *mdrf, int ndrf, const char *table_d_mnemonic);

/**
 * Read/write an entire sequence of data values from/to a data subset.
 *
 * Wraps ufbseq() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from or write to.
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read or write.
 * @param dim_2 - dimensionality of data to read or write.
 * @param iret - return value, length of data read or written.
 * @param table_d_mnemonic - Table A or Table D mnemonic.
 *
 * @author J. Ator @date 2023-04-07
 */
void ufbseq_f(int bufr_unit, void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_d_mnemonic);

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
int ireadns_f(int bufr_unit, char *subset, int *iddate, int subset_len);

/**
 * Read the next data subset from a BUFR file.
 *
 * Wraps readns() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from.
 * @param subset - the subset string.
 * @param iddate - datetime of message.
 * @param subset_len - length of the subset string.
 *
 * @param ires - return code:
 * - 0 new BUFR data subset was successfully read into internal arrays.
 * - -1 there are no more BUFR data subsets in bufr_unit.
 *
 * @author J. Ator @date 2025-09-05
 */
void readns_f(int bufr_unit, char *subset, int *iddate, int subset_len, int *ires);

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
void openmb_f(int bufr_unit, const char *c_subset, int iddate);

/**
 * Open a new message for output in a BUFR file that was previously
 * opened for writing.
 *
 * Wraps openmg() subroutine.
 *
 * @param bufr_unit - Fortran logical unit number to write to.
 * @param c_subset - Table A mnemonic of message.
 * @param iddate - Date-time to be stored within Section 1 of message.
 *
 * @author J. Ator @date 2025-10-20
 */
void openmg_f(int bufr_unit, const char *c_subset, int iddate);

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
void cmpmsg_f(const char *cf);

/**
 * Specify the use of compression when writing BUFR messages.
 *
 * Wraps catch_borts() function.
 *
 * @param cf - Flag indicating whether subsequent bort errors should be caught
 * and returned to the application program ('Y' = Yes, 'N' = No).
 *
 * @return - -1 if cf contained an illegal value, otherwise 0
 *
 * @author J. Ator @date 2025-10-15
 */
int catch_borts_f(const char *cf);

/**
 * Check whether a bort error was caught during a previous call to a library
 * function or subroutine
 *
 * Wraps check_for_bort() subroutine.
 *
 * @param error_str - Error string if a bort error occurred; otherwise empty
 * @param error_str_len - Allocated size of error_str
 *
 * @author J. Ator @date 2025-10-15
 */
void check_for_bort_f(char *error_str, int error_str_len);

/**
 * Get the current location of the file pointer within a BUFR file.
 *
 * Wraps ufbcnt() subroutine.
 *
 * @param lunit - Fortran logical unit
 * @param kmsg - Message number
 * @param ksub - Subset number
 *
 * @author J. Ator @date 2025-11-05
*/
void ufbcnt_f(int lunit, int *kmsg, int *ksub);

/**
 * Return a prepbufr program code corresponding to a mnemonic.
 *
 * Wraps ufbqcd() subroutine.
 *
 * @param lunit - Fortran logical unit
 * @param cnemo - Mnemonic
 * @param iqcd - Y value of descriptor associated with mnemonic
 *
 * @author J. Ator @date 2025-11-05
*/
void ufbqcd_f(int lunit, const char *cnemo, int *iqcd);

/**
 * Return a mnemonic corresponding to a prepbufr program code.
 *
 * Wraps ufbqcp() subroutine.
 *
 * @param lunit - Fortran logical unit
 * @param iqcp - Y value of a Category 63 Table D descriptor
 * @param cnemo - Mnemonic associated with iqcp
 * @param cnemo_len - Allocated length of cnemo string
 *
 * @author J. Ator @date 2025-11-05
*/
void ufbqcp_f(int lunit, int iqcp, char *cnemo, int cnemo_len);

/**
 * Get the meaning of a numerical value from a code or flag table.
 *
 * Wraps getcfmng() subroutine.
 *
 * @param lunit - Fortran logical unit
 * @param cnemoi - Mnemonic to search for
 * @param ivali - Value associated with cnemoi
 * @param cnemod - Optional second mnemonic upon which cnemoi may depend
 * @param ivald - Value associated with cnemod
 * @param cmeang_c - Meaning associated with cnemoi and ivali (and possibly cnemod and ivald as well)
 * @param lcmgc - Allocated length of cmeang_c
 * @param iret - Return code from call to getcfmng
 *
 * @author J. Ator @date 2025-11-05
*/
void getcfmng_f(int lunit, const char *cnemoi, int ivali, const char *cnemod, int ivald,
                char *cmeang_c, int lcmgc, int *iret);

/**
 * Get the bit settings equivalent to a given numerical value for a flag table mnemonic.
 *
 * Wraps upftbv() subroutine.
 *
 * @param lunit - Fortran logical unit
 * @param cnemo - Mnemonic with flag table units
 * @param val - Value corresponding to cnemo
 * @param ibit - Bit numbers which were set to "On" in val
 * @param mxib - Allocated size of ibit
 * @param nib - Number of bit numbers returned in ibit
 *
 * @author J. Ator @date 2025-11-05
*/
void upftbv_f(int lunit, const char *cnemo, double val, int *ibit, int mxib, int *nib);

/**
 * Read one or more data values from every data subset in a BUFR file.
 *
 * Wraps ufbtab() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read
 * @param dim_2 - dimensionality of data to read
 * @param iret - return value, number of data subsets read
 * @param table_b_mnemonic - String of mnemonics to read from each data subset
 *
 * @author J. Ator @date 2025-11-13
 */
void ufbtab_f(int bufr_unit, void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_b_mnemonic);

/**
 * Jump forwards or backwards to a specified data subset within a BUFR file.
 *
 * Wraps ufbpos() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from
 * @param irec - Ordinal number of message to be read
 * @param isub - Ordinal number of subset to be read from (irec)th message
 * @param subset - the subset string
 * @param iddate - datetime of message
 * @param subset_len - length of the subset string
 *
 * @author Jeff Ator @date 2025-11-13
 */
void ufbpos_f(int bufr_unit, int irec, int isub, char *subset, int *iddate, int subset_len);

/**
 * Specify the format of Section 1 date-time values that will be output by future calls to
 * message-reading subroutines.
 *
 * Wraps datelen() subroutine.
 *
 * @param len - Length of Section 1 date-time values to be output by all future calls to
 * message-reading subroutines.
 *
 * @author Jeff Ator @date 2025-11-14
 */
void datelen_f(int len);

/**
 * Read a specified value from within Section 0 or 1 of a BUFR message.
 *
 * Wraps iupvs01() function.
 *
 * @param bufr_unit - Fortran logical unit number to read from
 * @param c_s01m - Mnemonic
 *
 * @returns Value corresponding to mnemonic
 *
 * @author Jeff Ator @date 2025-11-14
 */
int iupvs01_f(int bufr_unit, const char *c_s01m);

/**
 * Get the total number of data subsets available within a BUFR message
 *
 * Wraps nmsub() function.
 *
 * @param bufr_unit - Fortran logical unit number to read from
 *
 * @returns Number of data subsets
 *
 * @author Jeff Ator @date 2025-11-14
 */
int nmsub_f(int bufr_unit);

/**
 * Specify a value to be written into Section 0 or 1 of all future BUFR messages
 *
 * Wraps pkvs01() subroutine.
 *
 * @param c_s01m - Mnemonic
 * @param ival - Value corresponding to mnemonic
 *
 * @author Jeff Ator @date 2025-11-14
 */
void pkvs01_f(const char *c_s01m, int ival);

/**
 * Get the Section 1 date-time from the first data message of a BUFR file.
 *
 * Wraps datebf() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from
 * @param mear - Year stored within Section 1 of first data message
 * @param mmon - Month stored within Section 1 of first data message
 * @param mday - Day stored within Section 1 of first data message
 * @param mour - Hour stored within Section 1 of first data message
 * @param idate - Date-time stored within Section 1 of first data message
 *
 * @author Jeff Ator @date 2025-11-18
 */
void datebf_f(int bufr_unit, int *mear, int *mmon, int *mday, int *mour, int *idate);

/**
 * Get the Section 1 date-time from the first two "dummy" messages of an NCEP dump file.
 *
 * Wraps dumpbf() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from
 * @param jdate - Dump center date-time stored within Section 1 of first "dummy" message
 * @param jdump - Dump initiation date-time stored within Section 1 of second "dummy" message
 *
 * @author Jeff Ator @date 2025-11-18
 */
void dumpbf_f(int bufr_unit, int *jdate, int *jdump);

/**
 * Write a minutes value into Section 1 of a BUFR message.
 *
 * Wraps minimg() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from
 * @param mini - Minutes value
 *
 * @author Jeff Ator @date 2025-11-18
 */
void minimg_f(int bufr_unit, int mini);

/**
 * Get the sequence of data descriptors contained within Section 3 of a BUFR message.
 *
 * Wraps upds3() subroutine.
 *
 * @param mbay - BUFR message
 * @param lcds3 - Allocated length of cds3
 * @param ccds3 - Data descriptor sequence within Section 3 of mbay
 * @param nds3 - Number of descriptors returned in cds3
 *
 * @author Jeff Ator @date 2025-11-18
 */
void upds3_f(int *mbay, int lcds3, char (*ccds3)[6], int *nds3);

/**
 * Specify a value to be written into Section 1 of a BUFR message
 *
 * Wraps pkbs1() subroutine.
 *
 * @param ival - Value corresponding to mnemonic
 * @param mbay - BUFR message
 * @param c_s1m - Mnemonic
 *
 * @author Jeff Ator @date 2025-11-18
 */
void pkbs1_f(int ival, int *mbay, const char *c_s1m);

/**
 * Specify a tank receipt time to be written into Section 1 of all future BUFR messages
 *
 * Wraps strcpt() subroutine.
 *
 * @param cf - Flag indicating whether future BUFR output messages should include a tank receipt time
 * @param iyr - Tank receipt year
 * @param imo - Tank receipt month
 * @param idy - Tank receipt day
 * @param ihr - Tank receipt hour
 * @param imi - Tank receipt minute
 *
 * @author Jeff Ator @date 2025-11-20
 */
void strcpt_f(const char *cf, int iyr, int imo, int idy, int ihr, int imi);

/**
 * Get the tank receipt time from Section 1 of a BUFR message
 *
 * Wraps rtrcpt() subroutine.
 *
 * @param lunit - Fortran logical unit
 * @param iyr - Tank receipt year
 * @param imo - Tank receipt month
 * @param idy - Tank receipt day
 * @param ihr - Tank receipt hour
 * @param imi - Tank receipt minute
 * @param iret - Return code
 *
 * @author Jeff Ator @date 2025-11-20
 */
void rtrcpt_f(int lunit, int *iyr, int *imo, int *idy, int *ihr, int *imi, int *iret);

/**
 * Read a BUFR message and output an equivalent message with a tank receipt time added to Section 1
 *
 * Wraps atrcpt() subroutine.
 *
 * @param msgin - BUFR message
 * @param lmsgot - Allocated length of msgot
 * @param msgot - Copy of msgin with a tank receipt time added to Section 1
 *
 * @author Jeff Ator @date 2025-11-20
 */
void atrcpt_f(int *msgin, int lmsgot, int *msgot);

/**
 * Print a copy of the DX BUFR table associated with a specified Fortran logical unit
 *
 * Wraps dxdump() subroutine.
 *
 * @param lunit - Fortran logical unit for BUFR file
 * @param luprt - Fortran logical unit number for print output
 *
 * @author Jeff Ator @date 2025-11-20
 */
void dxdump_f(int lunit, int luprt);

/**
 * Print a verbose listing of the contents of a data subset
 *
 * Wraps ufbdmp() subroutine.
 *
 * @param lunit - Fortran logical unit for BUFR file
 * @param luprt - Fortran logical unit number for print output
 *
 * @author Jeff Ator @date 2025-11-20
 */
void ufbdmp_f(int lunit, int luprt);

/**
 * Print a verbose listing of the contents of a data subset
 *
 * Wraps ufdump() subroutine.
 *
 * @param lunit - Fortran logical unit for BUFR file
 * @param luprt - Fortran logical unit number for print output
 *
 * @author Jeff Ator @date 2025-11-20
 */
void ufdump_f(int lunit, int luprt);

/**
 * Copy an entire BUFR file from one Fortran logical unit to another
 *
 * Wraps copybf() subroutine.
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author Jeff Ator @date 2025-11-20
 */
void copybf_f(int lunin, int lunot);

/**
 * Copy a BUFR message from one Fortran logical unit to another
 *
 * Wraps copymg() subroutine.
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author Jeff Ator @date 2025-11-20
 */
void copymg_f(int lunin, int lunot);

/**
 * Copy a BUFR data subset from one Fortran logical unit to another
 *
 * Wraps copysb() subroutine.
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 * @param iret - Return code
 *
 * @author Jeff Ator @date 2025-11-20
 */
void copysb_f(int lunin, int lunot, int *iret);

/**
 * Copy a BUFR data subset from one Fortran logical unit to another
 *
 * Wraps ufbcpy() subroutine.
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author Jeff Ator @date 2025-11-20
 */
void ufbcpy_f(int lunin, int lunot);

/**
 * Read a BUFR message from a memory array.
 *
 * Wraps readerme() subroutine.
 *
 * @param mesg - BUFR message
 * @param bufr_unit - Fortran logical unit number
 * @param subset - Subset string
 * @param iddate - Datetime of message
 * @param subset_len - Length of the subset string
 * @param ires - Return code
 *
 * @author Jeff Ator @date 2025-11-25
 */
void readerme_f(int *mesg, int bufr_unit, char *subset, int *iddate, int subset_len, int *ires);

/**
 * Read a specified data subset from a BUFR file.
 *
 * Wraps rdmgsb() subroutine.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param imsg - Message number
 * @param isub - Subset number
 *
 * @author Jeff Ator @date 2025-11-25
 */
void rdmgsb_f(int lunit, int imsg, int isub);

/**
 * Read an entire BUFR file into internal arrays.
 *
 * Wraps ufbmem() subroutine.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param inew - Processing option
 * @param iret - Number of BUFR messages that were read and stored into internal arrays
 * @param iunit - File status
 *
 * @author Jeff Ator @date 2025-11-25
 */
void ufbmem_f(int lunit, int inew, int *iret, int *iunit);

/**
 * Read an entire BUFR file into internal arrays.
 *
 * Wraps ufbmex() subroutine.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param lundx - Fortran logical unit number containing DX BUFR table information
 * @param inew - Processing option
 * @param iret - Number of BUFR messages that were read and stored into internal arrays
 * @param mesg - Types of BUFR messages that were read and stored into internal arrays
 *
 * @author Jeff Ator @date 2025-11-25
 */
void ufbmex_f(int lunit, int lundx, int inew, int *iret, int *mesg);

/**
 * Read a specified data subset from internal arrays.
 *
 * Wraps ufbmms() subroutine.
 *
 * @param imsg - Number of BUFR message to be read
 * @param isub - Number of data subset to be read from imsg
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_len - Allocated length of subset string
 *
 * @author Jeff Ator @date 2025-12-01
 */
void ufbmms_f(int imsg, int isub, char *subset, int *jdate, int subset_len);

/**
 * Read a specified data subset from internal arrays.
 *
 * Wraps ufbmns() subroutine.
 *
 * @param irep - Number of data subset to be read
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param idate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_len - Allocated length of subset string
 *
 * @author Jeff Ator @date 2025-12-01
 */
void ufbmns_f(int irep, char *subset, int *idate, int subset_len);

/**
 * Read a specified message from internal arrays.
 *
 * Wraps rdmemm() subroutine.
 *
 * @param imsg - Number of BUFR message to be read
 * @param subset - Table A mnemonic for type of BUFR message that was read
 * @param jdate - Date-time stored within Section 1 of BUFR message that was read
 * @param subset_len - Allocated length of subset string
 * @param iret - Return code
 *
 * @author Jeff Ator @date 2025-12-01
 */
void rdmemm_f(int imsg, char *subset, int *jdate, int subset_len, int *iret);

/**
 * Read a specified data subset from internal arrays.
 *
 * Wraps rdmems() subroutine.
 *
 * @param isub - Number of data subset to be read
 * @param iret - Return code
 *
 * @author Jeff Ator @date 2025-12-01
 */
void rdmems_f(int isub, int *iret);

/**
 * Read one or more data values from internal arrays.
 *
 * Wraps ufbrms() subroutine.
 *
 * @param imsg - Number of BUFR message to be read
 * @param isub - Number of data subset to be read from imsg
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read or write.
 * @param dim_2 - dimensionality of data to read or write.
 * @param iret - return value, length of data read or written.
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author Jeff Ator @date 2025-12-01
 */
void ufbrms_f(int imsg, int isub, void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_b_mnemonic);

/**
 * Read one or more data values from every data subset in internal arrays.
 *
 * Wraps ufbtam() subroutine.
 *
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read
 * @param dim_2 - dimensionality of data to read
 * @param iret - return value, number of data subsets read
 * @param table_b_mnemonic - String of mnemonics to read from each data subset
 *
 * @author J. Ator @date 2025-12-01
 */
void ufbtam_f(void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_b_mnemonic);

/**
 * Copy a message from internal arrays to a file
 *
 * Wraps cpymem() subroutine.
 *
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author Jeff Ator @date 2025-12-02
 */
void cpymem_f(int lunot);

/**
 * Copy unique elements of a data subset from one file to another
 *
 * Wraps ufbcup() subroutine.
 *
 * @param lunin - Fortran logical unit number for source BUFR file
 * @param lunot - Fortran logical unit number for target BUFR file
 *
 * @author Jeff Ator @date 2025-12-02
 */
void ufbcup_f(int lunin, int lunot);

/**
 * Specify whether to standardize future output BUFR messages
 *
 * Wraps stdmsg() subroutine.
 *
 * @param cf - Flag indicating whether future BUFR output messages should be WMO-standard
 *
 * @author Jeff Ator @date 2025-12-02
 */
void stdmsg_f(const char *cf);

/**
 * Standardize a copy of a BUFR message
 *
 * Wraps stndrd() subroutine.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param msgin - BUFR message
 * @param lmsgot - Allocated length of msgot
 * @param msgot - Copy of msgin now fully WMO-standardized
 *
 * @author Jeff Ator @date 2025-12-02
 */
void stndrd_f(int lunit, int *msgin, int lmsgot, int *msgot);

/**
 * Specify whether to read code and flag table information from master BUFR tables
 *
 * Wraps codflg() subroutine.
 *
 * @param cf - Flag indicating whether code and flag table information should be included
 * when reading from master BUFR tables
 *
 * @author Jeff Ator @date 2025-12-02
 */
void codflg_f(const char *cf);

/**
 * Get the parent for a specified occurrence of a Table B or Table D mnemonic
 *
 * Wraps gettagpr() subroutine.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tagch - Table B or Table D mnemonic
 * @param ntagch - Ordinal occurrence of tagch for which tagpr is to be returned
 * @param tagpr - Table D mnemonic
 * @param tagpr_len - Allocated length of tagpr
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-03
 */
void gettagpr_f(int lunit, const char *tagch, int ntagch, char *tagpr, int tagpr_len, int *iret);

/**
 * Get the parent for a specified occurrence of a Table B or Table D mnemonic
 *
 * Wraps gettagre() subroutine.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tagi - Table B mnemonic
 * @param ntagi - Ordinal occurrence of tagi for which tagre is to be returned
 * @param tagre - Table B mnemonic referenced by tagi via an internal bitmap
 * @param tagre_len - Allocated length of tagre
 * @param ntagre - Ordinal occurrence of tagre referenced by (ntagi)th occurrence of tagi
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-03
 */
void gettagre_f(int lunit, const char *tagi, int ntagi, char *tagre, int tagre_len, int *ntagre, int *iret);

/**
 * Convert a BUFR message to edition 4
 *
 * Wraps cnved4() subroutine.
 *
 * @param msgin - BUFR message
 * @param lmsgot - Allocated length of msgot
 * @param msgot - Copy of msgin now converted to edition 4
 *
 * @author Jeff Ator @date 2025-12-03
 */
void cnved4_f(int *msgin, int lmsgot, int *msgot);

/**
 * Check if a subset definition contains any long character strings
 *
 * Wraps lcmgdf() function.
 *
 * @param bufr_unit - Fortran logical unit number
 * @param c_subset - Table A mnemonic
 *
 * @returns Return code
 *
 * @author Jeff Ator @date 2025-12-03
 */
int lcmgdf_f(int bufr_unit, const char *c_subset);

/**
 * Write a data value corresponding to a specific occurrence of a mnemonic
 *
 * Wraps setvalnb() subroutine.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tagpv - Pivot mnemonic
 * @param ntagpv - Ordinal occurrence of tagpv to search for
 * @param tagnb - Nearby mnemonic
 * @param ntagnb - Ordinal occurrence of tagnb to search for
 * @param r8val - Value to be stored
 * @param iret - Return code
 *
 * @author J. Ator @date 2025-12-05
 */
void setvalnb_f(int lunit, const char *tagpv, int ntagpv, const char *tagnb, int ntagnb, double r8val, int *iret);

/**
 * Read a data value corresponding to a specific occurrence of a mnemonic
 *
 * Wraps getvalnb() function.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param tagpv - Pivot mnemonic
 * @param ntagpv - Ordinal occurrence of tagpv to search for
 * @param tagnb - Nearby mnemonic
 * @param ntagnb - Ordinal occurrence of tagnb to search for
 *
 * @returns Return value
 *
 * @author J. Ator @date 2025-12-05
 */
double getvalnb_f(int lunit, const char *tagpv, int ntagpv, const char *tagnb, int ntagnb);

/**
 * Get Table B and Table D information from the internal DX tables
 *
 * Wraps getabdb() subroutine.
 *
 * @param lunit - Fortran logical unit number for BUFR file
 * @param itab - Allocated length of ctabdb
 * @param ctabdb - Internal Table B and Table D information
 * @param jtab - Number of entries returned in ctabdb
 *
 * @author J. Ator @date 2025-12-05
 */
void getabdb_f(int lunit, int itab, char (*ctabdb)[128], int *jtab);

/**
 * Read one or more data values from a data subset without advancing the subset pointer
 *
 * Wraps ufbget() subroutine.
 *
 * @param bufr_unit - Fortran logical unit number to read from
 * @param tab - Data values
 * @param i1 - Allocated length of tab
 * @param iret - Return code
 * @param table_b_mnemonic - String of mnemonics to read from the data subset
 *
 * @author J. Ator @date 2025-12-05
 */
void ufbget_f(int bufr_unit, double *tab, int i1, int *iret, const char *table_b_mnemonic);

/**
 * Read one or more data values from a specified data subset
 *
 * Wraps ufbinx() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to read from
 * @param imsg - Number of BUFR message to be read
 * @param isub - Number of data subset to be read from imsg
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to read
 * @param dim_2 - dimensionality of data to read
 * @param iret - return value, length of data read
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author J. Ator @date 2025-12-05
 */
void ufbinx_f(int bufr_unit, int imsg, int isub, void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_b_mnemonic);

/**
 * Overwrite one or more data values within a data subset
 *
 * Wraps ufbovr() subroutine.
 *
 * @param bufr_unit - the Fortran logical unit number to write to
 * @param c_data - pointer to a pointer to a pre-allocated buffer.
 * @param dim_1 - dimensionality of data to write
 * @param dim_2 - dimensionality of data to write
 * @param iret - return value, length of data written
 * @param table_b_mnemonic - string of mnemonics.
 *
 * @author J. Ator @date 2025-12-05
 */
void ufbovr_f(int bufr_unit, void **c_data, int dim_1, int dim_2,
              int *iret, const char *table_b_mnemonic);

#ifdef __cplusplus
}
#endif
