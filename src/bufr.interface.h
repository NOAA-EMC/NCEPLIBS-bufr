#pragma once

#ifdef __cplusplus
extern "C" {
#endif

  void open_f(int unit, const char* filepath);
  void close_f(int unit);
  void openbf_f(int bufr_unit, const char* cio, int table_file_d);
  void closbf_f(int bufr_unit);
  int ireadmg_f(int bufr_unit, char* subset, int* iddate);
  int ireadsb_f(int bufr_unit);
  void ufbint_f(int bufr_unit, void** c_data, int dim_1, int dim_2, int* iret, const char* table_b_mnemonic);
  void ufbrep_f(int bufr_unit, void** c_data, int dim_1, int dim_2, int* iret, const char* table_b_mnemonic);

#ifdef __cplusplus
}
#endif

