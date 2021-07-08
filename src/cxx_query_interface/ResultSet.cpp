//
// Created by rmclaren on 6/30/21.
//

#include "ResultSet.h"
#include "query_interface.h"

struct FortranRealArray
{
    double* data;
    std::size_t size;
};

extern int __modq_result_set_MOD___vtab_modq_result_set_Resultset;

extern "C"
{
    void __cxx_query_interface_MOD_allocate__result_set(bufr::Address *caddr);
    void __cxx_query_interface_MOD_deallocate__result_set(bufr::Address caddr);
}

namespace bufr
{
    ResultSet::ResultSet()
    {
        Address data_ptr;
        __cxx_query_interface_MOD_allocate__result_set(&data_ptr);
        initialize(data_ptr, true);
    }

    ResultSet::~ResultSet()
    {
        if (is_owned_)
        {
            __cxx_query_interface_MOD_deallocate__result_set(data_ptr_);
        }
    }

    std::vector<double> ResultSet::get(const std::string& field_name,
                                       const std::string& for_field)
    {
        double* data_ptr = nullptr;
        std::size_t data_len;
        result_set__get_f(class_data_ptr_,
                          field_name.c_str(),
                          for_field.c_str(),
                          &data_ptr,
                          &data_len);

        auto data =  std::vector<double>();
        data.assign(data_ptr, data_ptr + data_len);
        return data;
    }

    Address ResultSet::get_v_ptr()
    {
        return &__modq_result_set_MOD___vtab_modq_result_set_Resultset;
    }
}
