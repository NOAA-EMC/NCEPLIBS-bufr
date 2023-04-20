/**
 * @file
 * @brief Define signatures to enable a number of NCEPLIBS-bufr subprograms to be called via wrapper
 * functions from C and C++ application programs.
 *
 * This header file defines the signatures for the subprograms in bufr_c2f_interface.F90 which wrap
 * a number of native Fortran subroutines in the library.  It also contains prototypes for native C
 * functions in the library which are expected to be called from C and C++ application programs.
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
 * @param lun - file ID.
 * @param il - file status.
 * @param im - message status.
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
 * @param mnemonic - mnemonic.
 * @param unit_c - unit str.
 * @param unit_str_len - unit str length.
 * @param desc_c - description string.
 * @param desc_str_len - description str length.
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
 * @param mnemonic: - mnemonic.
 * @param mnemonic_idx - indicates specific mnemonic element (if repeated).
 * @param scale - scale of element.
 * @param reference - reference of element.
 * @param bits - number of bits representing the element.
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
 * @param bufr_unit - the bufr file pointer.
 * @param mnemonic - mnemonic.
 * @param descriptor - the binary descriptor for the mnemonic.
 * @param table_type - 'A', 'B', 'C', or 'D', depending on table type.
 * @param table_idx - the table index, or 0 if not found.
 *
 * @author Ronald Mclaren @date 2022-08-16
 */
    void nemtab_f(int bufr_unit,
                  const char* mnemonic,
                  int* descriptor,
                  char* table_type,
                  int* table_idx);

/**
 * Get information about a Table B descriptor.
 *
 * Wraps nemtbb() subroutine.
 *
 * @param bufr_unit - the bufr file pointer.
 * @param table_idx - Table B index.
 * @param unit_str - unit str.
 * @param unit_str_len - unit str length.
 * @param scale - scale of element.
 * @param reference - reference of element.
 * @param bits - bits of element.
 *
 * @author Ronald McLaren @date 2022-08-16
 */
    void nemtbb_f(int bufr_unit,
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
 * @param lun - pointer for the file stream.
 * @param start_node - the start node of the subset.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_inode_f(int lun, int* start_node);

/**
 * Get the number of values in the current subset
 *
 * @param lun - pointer for the file stream.
 * @param num_nodes - number of values in the subset.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_nval_f(int lun, int* num_nodes);

/**
 * Get pointer to the moda_usrint VAL array.
 *
 * @param lun - pointer for the file stream.
 * @param val_ptr - pointer to a pointer to the VAL array.
 * @param val_size - size of the VAL array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_val_f(int lun, double** val_ptr, int* val_size);

/**
 * Get pointer to the moda_usrint INV array.
 *
 * @param lun - pointer for the file stream.
 * @param inv_ptr - pointer to a pointer to the INV array.
 * @param inv_size - size of the INV array.
 *
 * @author Ronald McLaren @date 2022-03-23
 */
  void get_inv_f(int lun, int** inv_ptr, int* inv_size);

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
 * @author J.Ator @date 2014-12-04
 */
  int igetprm_f(char *cprmnm);

#ifdef __cplusplus
}
#endif
