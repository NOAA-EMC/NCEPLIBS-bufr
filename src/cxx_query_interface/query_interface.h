//
// Created by rmclaren on 7/2/21.
//

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

    double* result_set__get_f(void* cls,
                              const char* field,
                              const char* for_field,
                              double** data,
                              std::size_t* data_len);

#ifdef __cplusplus
}
#endif