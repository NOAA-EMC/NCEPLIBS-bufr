/** @file
    @author Ronald Mclaren
    @date 2020-07-29

    @brief This header file defines the c function signatures for the functions
           exposed in bufr.interface.f90. It is included by c/c++ source files
           that wish to use NCEPLIB-bufr functions.
*/

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

  void open_f(int unit, const char* filepath);

  void close_f(int unit);

  void openbf_f(int bufr_unit, const char* cio, int table_file_d);

  void closbf_f(int bufr_unit);

  void exitbufr_f();

  int ireadmg_f(int bufr_unit, char* subset, int* iddate, int subset_len);

  int ireadsb_f(int bufr_unit);

  void ufbint_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_b_mnemonic);

  void ufbrep_f(int bufr_unit, void** c_data, int dim_1, int dim_2,
                int* iret, const char* table_b_mnemonic);

  void dxdump_f(int bufr_unit, int table_unit);

  void mtinfo_f(const char* path, int file_unit_1, int file_unit_2);

#ifdef __cplusplus
}
#endif
