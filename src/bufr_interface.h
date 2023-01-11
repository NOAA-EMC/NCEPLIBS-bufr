/**
   @file
   @brief Define signatures to enable a number of BUFRLIB 
   subprograms to be called via wrapper functions from C and C++
   application programs.
   
   This header file defines the signatures for the functions
   in bufr_interface.f90 which wrap a number of native Fortran
   subroutines in the BUFRLIB.
   
   @author Ronald Mclaren @date 2020-07-29
*/

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

/**
   Wraps fortran "open" statement so we can open a Fortran file
   from a C program.

   @param unit - int: the integer to use as the fortran file unit
   @param filepath - const char*: path to the file we want to open.
   
   @author Ronald Mclaren @date 2020-07-29
*/
  void open_f(int unit, const char* filepath);


/**
   Wraps fortran "close" statement so we can close a Fortran file
   from a C program.

   @param[in] unit - int: the integer to use as the fortran file unit

   @author Ronald Mclaren @date 2020-07-29
*/
  void close_f(int unit);


/**
   Wraps BUFRLIB "openbf" subroutine.

   @param[in] bufr_unit - int: the fortran file unit number
   @param[in] cio - const char*: cio string (ex "IN", "SEC3", and "OUT")
   @param[in] table_file_id - int: table_file unit number

   @author Ronald Mclaren @date 2020-07-29
*/
  void openbf_f(int bufr_unit, const char* cio, int table_file_id);


/**
   Wraps BUFRLIB "closbf" subroutine.

   @param[in] bufr_unit - int: the fortran file unit number to close

   @author Ronald Mclaren @date 2020-07-29
*/
  void closbf_f(int bufr_unit);


/**
   Wraps BUFRLIB "exitbufr" subroutine. Closes all open file units
   used by BUFRLIB.

   @author Ronald Mclaren @date 2020-07-29
*/
  void exitbufr_f();


/**
   Wraps BUFRLIB "ireadmg" subroutine.

   @param[in] bufr_unit - int: the fortran file unit number to read from
   @param[out] subset - char*: the subset string
   @param[out] iddate - int*: datetime of message
   @param[in] subset_len - int: length of the subset string

   @return ???

   @author Ronald Mclaren @date 2020-07-29
*/
  int ireadmg_f(int bufr_unit, char* subset, int* iddate, int subset_len);


/**
   Wraps BUFRLIB "ireadsb" function.

   @param[in] bufr_unit - int: the fortran file unit number to read from

   @return ???

   @author Ronald Mclaren @date 2020-07-29
*/
  int ireadsb_f(int bufr_unit);


/**
   Wraps BUFRLIB "ufbint" function.

   @param[in] bufr_unit - int: the fortran file unit number to read from
   @param[inout] c_data - void**: c style pointer to a pre-allocated buffer
   @param[in] dim_1, dim_2 - int: dimensionality of data to read or write
   @param[out] iret -  int: return value, length of data read
   @param[in] table_b_mnemonic - const char*: string of mnemonics

   @author Ronald Mclaren @date 2020-07-29
*/
  void ufbint_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_b_mnemonic);


/**
   Wraps BUFRLIB "ufbrep" function.

   @param[in] bufr_unit - int: the fortran file unit number to read from
   @param[inout] c_data - void**: c style pointer to a pre-allocated buffer
   @param[in] dim_1, dim_2 - int: dimensionality of data to read or write
   @param[out] iret -  int: return value, length of data read
   @param[in] table_b_mnemonic - const char*: string of mnemonics

   @author Ronald Mclaren @date 2020-07-29
*/
  void ufbrep_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_b_mnemonic);


/**
   Wraps BUFRLIB "mtinfo" function.

   @param[in] path - const char*: the path where the WMO tables are stored
   @param[in] file_unit_1 - int: number to use for first file unit
   @param[in] file_unit_2 - int: number to use for second file unit

   @author Ronald Mclaren @date 2020-07-29
*/
  void mtinfo_f(const char* path, int file_unit_1, int file_unit_2);


  // Table Data
/**
   Wraps BUFRLIB "status" function.

   @param[in] file_unit - int: the fortran file unit number to read from
   @param[out] lun - int: pointer for the file stream
   @param[out] il - int: file status
   @param[out] im - int: message status

   @author Ronald Mclaren @date 2020-07-29
*/
  void status_f(int file_unit, int* lun, int* il, int* im);


/**
   Gets Table B Unit and Description strings for a mnemonic. Wraps BUFRLIB "nemdefs".

   @param[in] file_unit - int: Fortran file unit for the open file
   @param[in] mnemonic - const char*: mnemonic
   @param[out] unit_c - char*: unit str
   @param[in] unit_str_len - int: unit str length
   @param[out] desc_c - char*: description string
   @param[in] desc_str_len - int: description str length
   @param[out] iret - int*: return value. 0 indicates success -1 indicates failure.

   @author Ronald Mclaren @date 2020-07-29
*/
    void nemdefs_f(int file_unit,
                   const char* mnemonic,
                   char* unit_c,
                   int unit_str_len,
                   char* desc_c,
                   int desc_str_len,
                   int* iret);


/**
   Gets Table B scale, reference, and bits values. Wraps BUFRLIB "nemspecs".

   @param[in] file_unit - int: Fortran file unit for the open file
   @param[in] mnemonic - const char*: mnemonic
   @param[in] mnemonic_idx - int: indicates specific mnemonic element (if repeated)
   @param[out] scale - int*: scale of element
   @param[out] reference - int*: reference of element
   @param[out] bits - int*: number of bits representing the element
   @param[out] iret - int*: return value. 0 indicates success -1 indicates failure.
   
   @author Ronald Mclaren @date 2022-08-08
*/
    void nemspecs_f(int file_unit,
                    const char* mnemonic,
                    int mnemonic_idx,
                    int* scale,
                    int* reference,
                    int* bits,
                    int* iret);


/**
   This subroutine returns information about a descriptor from the
   internal DX BUFR tables, based on the mnemonic associated with that
   descriptor.
   
   @param[in] bufr_unit - int: the bufr file pointer
   @param[in] mnemonic - const char*: mnemonic
   @param[out] descriptor - int*: the binary descriptor for the mnemonic
   @param[out] table_type char*: 'A', 'B', 'C', or 'D', depending on table type
   @param[out] table_idx - int*: the table index, or 0 if not found
   
   @author Ronald Mclaren @date 2022-08-16
*/
    void nemtab_f(int bufr_unit,
                  const char* mnemonic,
                  int* descriptor,
                  char* table_type,
                  int* table_idx);


/**  
     Get information about a Table B descriptor.
     
     @param[in] bufr_unit - int: the bufr file pointer
     @param[in] table_idx - int: Table B index
     @param[out] unit_str - char*: unit str
     @param[in] unit_str_len - int: unit str length
     @param[out] scale - int*: scale of element
     @param[out] reference - int*: reference of element
     @param[out] bits - int*: bits of element
     
     @author Ronald McLaren @date 2022-08-16
     
*/
    void nemtbb_f(int bufr_unit,
                  int table_idx,
                  char* unit_str,
                  int unit_str_len,
                  int* scale,
                  int* reference,
                  int* bits);


/**
   Get copy of the moda_tables ISC array.
   
   @param[out] isc_ptr - int**: c style pointer to the ISC array
   @param[out] isc_size - int*: size of the ISC array
   
   @author Ronald McLaren @date 2022-03-23    
*/
  void get_isc_f(int** isc_ptr, int* isc_size);


/** 
    Get copy of the moda_tables LINK array.
    
    @param[out] link_ptr - int**: c style pointer to the LINK array
    @param[out] link_size - int*: size of the LINK array
    
    @author Ronald McLaren @date 2022-03-23    
*/
  void get_link_f(int** link_ptr, int* link_size);


/** 
    Get copy of the moda_tables ITP array.

    @param[out] itp_ptr - int**: c style pointer to the ITP array
    @param[out] itp_size - int*: size of the ITP array
    
    @author Ronald McLaren @date 2022-03-23
    
*/
  void get_itp_f(int** itp_ptr, int* itp_size);


/** 
    Get copy of the moda_tables TYP array.

    @param[out] typ_ptr - char**: c style pointer to the TYP array
    @param[out] typ_len - int*: size of each string within the TYP array
    @param[out] mem_size - int*: size of the TYP array
    
    @author Ronald McLaren @date 2022-03-23
*/
  void get_typ_f(char** typ_ptr, int* typ_len, int* mem_size);


/** 
    Get copy of the moda_tables TAG array.

    @param[out] tag_ptr - char**: c style pointer to the TAG array
    @param[out] tag_len - int*: size of each string within the TAG array
    @param[out] mem_size - int*: size of the TAG array
    
    @author Ronald McLaren @date 2022-03-23
  
*/
  void get_tag_f(char** tag_ptr, int* tag_len, int* mem_size);


/** 
    Get copy of the moda_tables JMPB array.

    @param[out] jmpb_ptr - int**: c style pointer to the JMPB array
    @param[out] jmpb_size - int: size of the JMPB array
    
    @author Ronald McLaren @date 2022-03-23
*/
  void get_jmpb_f(int** jmpb_ptr, int* jmpb_size);


  // Data
/** 
    Get the bufr node idx for the start node of the subset.

    @param[in] lun - int: pointer for the file stream
    @param[out] start_node - int*: the start node of the subset

    @author Ronald McLaren @date 2022-03-23
*/
  void get_inode_f(int lun, int* start_node);


/** 
    Get the number of values in the current subset

    @param[in] lun - int: pointer for the file stream
    @param[out] num_nodes - int*: number of values in the subset

    @author Ronald McLaren @date 2022-03-23
*/
  void get_nval_f(int lun, int* num_nodes);


/** 
    Get pointer to the moda_usrint VAL array.

    @param[in] lun - int: pointer for the file stream
    @param[out] val_ptr - double**: c style pointer to the VAL array
    @param[out] val_size - int*: size of the VAL array

    @author Ronald McLaren @date 2022-03-23
*/
  void get_val_f(int lun, double** val_ptr, int* val_size);


/** 
    Get pointer to the moda_usrint INV array.

    @param[in] lun - int: pointer for the file stream
    @param[out] inv_ptr - int**: c style pointer to the INV array
    @param[out] inv_size - int*: size of the INV array
    
    @author Ronald McLaren @date 2022-03-23
*/
  void get_inv_f(int lun, int** inv_ptr, int* inv_size);


/** 
    Deletes the copies of the moda_tables arrays.

    @author Ronald McLaren @date 2022-03-23
*/
  void delete_table_data_f();

#ifdef __cplusplus
}
#endif
