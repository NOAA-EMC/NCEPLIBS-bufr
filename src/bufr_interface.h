/** @file
    @brief Define signatures for C functions which wrap native Fortran
    BUFRLIB functions. 

    @author Ronald Mclaren
    @date 2020-07-29

    <p>This header file defines the C function signatures for the Fortran
       functions exposed in bufr_interface.f90. It is intended for use by
       C/C++ application programs that wish to call BUFRLIB functions
       written in Fortran.
*/

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

/** @author Ronald McLaren
    @date 2020-07-29

    @brief Wraps fortran "open" statement so we can open a Fortran file
           from a C program.

    @param unit - int: the integer to use as the fortran file unit
    @param filepath - const char*: path to the file we want to open.
*/
  void open_f(int unit, const char* filepath);


/** @author Ronald McLaren
    @date 2020-07-29

    @brief Wraps fortran "close" statement so we can close a Fortran file
           from a C program.

    @param[in] unit - int: the integer to use as the fortran file unit
*/
  void close_f(int unit);


/** @author Ronald McLaren
    @date 2020-07-29

    @brief Wraps BUFRLIB "openbf" subroutine.

    @param[in] bufr_unit - int: the fortran file unit number
    @param[in] cio - const char*: cio string (ex "IN", "SEC3", and "OUT")
    @param[in] table_file_id - int: table_file unit number
*/
  void openbf_f(int bufr_unit, const char* cio, int table_file_id);


/** @author Ronald McLaren
    @date 2020-07-29

    @brief Wraps BUFRLIB "closbf" subroutine.

    @param[in] bufr_unit - int: the fortran file unit number to close
*/
  void closbf_f(int bufr_unit);


/**  @author Ronald McLaren
     @date 2020-07-29

     @brief Wraps BUFRLIB "exitbufr" subroutine. Closes
            all open file units used by BUFRLIB.
*/
  void exitbufr_f();


/** @author Ronald McLaren
    @date 2020-07-29

    @brief Wraps BUFRLIB "ireadmg" subroutine.

    @param[in] bufr_unit - int: the fortran file unit number to read from
    @param[inout] subset - char*: the subset string
    @param[out] iddate - int*: datetime of message
    @param[in] subset_len - int: length of the subset string
*/
  int ireadmg_f(int bufr_unit, char* subset, int* iddate, int subset_len);


/** @author Ronald McLaren
    @date 2020-07-29

    @brief Wraps BUFRLIB "ireadsb" function.

    @param[in] bufr_unit - int: the fortran file unit number to read from
*/
  int ireadsb_f(int bufr_unit);


/** @author Ronald McLaren
    @date 2020-07-29

    @brief Wraps BUFRLIB "ufbint" function.

    @param[in] bufr_unit - int: the fortran file unit number to read from
    @param[inout] c_data - void**: c style pointer to a pre-allocated buffer
    @param[in] dim_1, dim_2 - int: dimensionality of data to read or write
    @param[out] iret -  int: return value, length of data read
    @param[in] table_b_mnemonic - const char*: string of mnemonics
*/
  void ufbint_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_b_mnemonic);


/** @author Ronald McLaren
    @date 2020-07-29

    @brief Wraps BUFRLIB "ufbrep" function.

    @param[in] bufr_unit - int: the fortran file unit number to read from
    @param[inout] c_data - void**: c style pointer to a pre-allocated buffer
    @param[in] dim_1, dim_2 - int: dimensionality of data to read or write
    @param[out] iret -  int: return value, length of data read
    @param[in] table_b_mnemonic - const char*: string of mnemonics
*/
  void ufbrep_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_b_mnemonic);


/** @author Ronald McLaren
    @date 2021-02-24

    @brief Wraps BUFRLIB "mtinfo" function.

    @param[in] path - const char*: the path where the WMO tables are stored
    @param[in] file_unit_1 - int: number to use for first file unit
    @param[in] file_unit_2 - int: number to use for second file unit
*/
  void mtinfo_f(const char* path, int file_unit_1, int file_unit_2);

#ifdef __cplusplus
}
#endif
