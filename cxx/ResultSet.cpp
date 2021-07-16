//
// Created by rmclaren on 6/30/21.
//

#include <iostream>

#include "ResultSet.h"
#include "query_interface.h"


namespace bufr
{
    ResultSet::ResultSet()
    {
        Address data_ptr;
        result_set__allocate_f(&data_ptr);
        initialize(data_ptr, true);
    }

    ResultSet::~ResultSet()
    {
        if (is_owned_)
        {
            result_set__deallocate_f(data_ptr_);
        }
    }

    Result ResultSet::get(const std::string& field_name,
                          const std::string& for_field) const
    {
        double* data_ptr = nullptr;
        std::size_t dimRows = 0;
        std::size_t dimCols = 0;
        std::size_t dimZ = 0;
        result_set__get_f(class_data_ptr_,
                          field_name.c_str(),
                          for_field.c_str(),
                          &data_ptr,
                          &dimRows,
                          &dimCols,
                          &dimZ);

        auto total_size = dimRows * dimCols * dimZ;
        auto data = std::vector<float>();

        data.resize(total_size);

        for (int data_idx = 0; data_idx < total_size; data_idx++)
        {
            data[data_idx] = static_cast<float>(data_ptr[data_idx]);
        }

        Result result;
        result.data = data;
        result.dimRows = dimRows;
        result.dimCols = dimCols;
        result.dimZ = dimZ;

        return result;
    }

    Address ResultSet::get_v_ptr()
    {
        return &__modq_result_set_MOD___vtab_modq_result_set_Resultset;
    }
}
