//
// Created by rmclaren on 6/30/21.
//


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

    Result<float> ResultSet::get(const std::string& field_name,
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

        Result<float> result;
        result.data = data;
        result.dims.push_back(dimRows);
        result.dims.push_back(dimCols);
        result.dims.push_back(dimZ);

        return result;
    }

    Result<std::string> ResultSet::get_as_strs(const std::string& field_name,
                                               const std::string& for_field) const
    {
        char* char_ptr = nullptr;
        std::size_t num_strs = 0;

        result_set__get_as_chars_f(class_data_ptr_,
                                   field_name.c_str(),
                                   for_field.c_str(),
                                   &char_ptr,
                                   &num_strs);

        Result<std::string> result;
        result.data.resize(num_strs);
        result.dims.push_back(num_strs);

        for (int str_idx = 0; str_idx < num_strs; str_idx++)
        {
            result.data[str_idx] = std::string(&char_ptr[str_idx]);
        }

        return result;
    }

    bool ResultSet::is_string(const std::string& fieldName) const
    {
        return result_set__is_string_f(class_data_ptr_, fieldName.c_str());
    }

    Address ResultSet::get_v_ptr()
    {
        return &__modq_result_set_MOD___vtab_modq_result_set_Resultset;
    }
}
