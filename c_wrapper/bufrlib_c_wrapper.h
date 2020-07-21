#ifndef bufrlib_c_wrapper_h
#define bufrlib_c_wrapper_h

#ifdef __cplusplus
extern "C" {
#endif

  void open_fortran_file(int unit, const char* filepath);
  void close_fortran_file(int unit);
  void open_bufr(int bufr_unit, const char* cio, int table_file_d);
  void close_bufr(int bufr_unit);
  int read_next_msg(int bufr_unit, char* subset, int* iddate);
  int read_next_subset(int bufr_unit);
  void ufbint(int bufr_unit, double** c_data, int* dim_1, int* dim_2, int* iret, const char* table_b_mnemonic);
  void ufbrep(int bufr_unit, double** c_data, int* dim_1, int* dim_2, int* iret, const char* table_b_mnemonic);

#ifdef __cplusplus
}
#endif

#endif /* bufrlib_c_wrapper_h */