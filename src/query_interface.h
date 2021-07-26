//
// Created by rmclaren on 7/2/21.
//

#pragma once

extern int __modq_query_set_MOD___vtab_modq_query_set_Queryset;
extern int __modq_result_set_MOD___vtab_modq_result_set_Resultset;


#ifdef __cplusplus
extern "C" {
#endif

    // QuerySet Methods
    void query_set__allocate_f(void** query_set);
    void query_set__add_f(void* cls, const char* query_str, const char* query_name);
    void query_set__print_f(void* cls);
    void query_set__deallocate_f(void* query_set);

    // Execute Methods
    void execute_f(std::size_t file_unit, void** query_set, std::size_t next, void** result_set);

    // ResultSet Methods
    void result_set__allocate_f(void** result_set);

    void result_set__get_raw_f(void* cls,
                                const char* field,
                                const char* for_field,
                                double** data,
                                std::size_t* dim_rows,
                                std::size_t* dim_cols,
                                std::size_t* dim_z);

    bool result_set__is_string_f(void* cls, const char* field);

    void result_set__deallocate_f(void* result_set);

#ifdef __cplusplus
}
#endif
