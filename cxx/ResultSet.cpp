//
// Created by rmclaren on 6/30/21.
//


#include "ResultSet.h"
#include "query_interface.h"

#include <algorithm> 
#include <iostream>
#include <sstream>


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

    std::shared_ptr<ResultBase> ResultSet::get(const std::string& field_name,
                                               const std::string& group_by_field) const
    {
        double* data_ptr = nullptr;
        std::size_t dimRows = 0;
        std::size_t dimCols = 0;
        std::size_t dimZ = 0;
        result_set__get_raw_f(class_data_ptr_,
                              field_name.c_str(),
                              group_by_field.c_str(),
                              &data_ptr,
                              &dimRows,
                              &dimCols,
                              &dimZ);

        bool isString = result_set__is_string_f(class_data_ptr_, field_name.c_str());
        std::shared_ptr<ResultBase> result;

        if (isString)
        {
            auto data = std::vector<std::string>();

            const char* char_ptr = (char*) data_ptr;
            for (int row_idx = 0; row_idx < dimRows; row_idx++)
            {
                std::string str = std::string(char_ptr + row_idx * dimCols * sizeof(double),
                                              dimCols * sizeof(double));

                // trim trailing whitespace from str
                str.erase(std::find_if(str.rbegin(), str.rend(),
                                       [](char c) { return !std::isspace(c); }).base(), str.end());

                data.push_back(str);
            }

            auto strResult = std::make_shared<Result<std::string>>();
            strResult->data = data;
            strResult->dims.push_back(dimRows);
            result = strResult;
        }
        else
        {
            auto data = std::vector<float>();

            data.resize(dimRows * dimCols);

            // Raw fortran data arranged in column major ordering
            // but we want row major ordering instead. So convert.
            unsigned int pos = 0;
            for (int rowIdx = 0; rowIdx < dimRows; rowIdx++)
            {
                for (int colIdx = 0; colIdx < dimCols; colIdx++)
                {
                    data[pos] = static_cast<float>(data_ptr[rowIdx + dimRows * colIdx]);
                    pos++;
                }
            }

            auto floatResult = std::make_shared<Result<float>>();
            floatResult->data = data;
            floatResult->dims.push_back(dimRows);
            floatResult->dims.push_back(dimCols);
            floatResult->dims.push_back(dimZ);
            result = floatResult;
        }

        return result;
    }

    Address ResultSet::get_v_ptr()
    {
        return &__modq_result_set_MOD___vtab_modq_result_set_Resultset;
    }
}
