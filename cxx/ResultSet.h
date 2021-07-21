//
// Created by rmclaren on 6/30/21.
//

#pragma once

#include "FortranObject.h"

#include <string>
#include <vector>

namespace bufr
{
    template <typename T>
    struct Result
    {
        std::vector<T> data;
        std::vector<std::size_t> dims;
    };


    class ResultSet : public FortranObject
    {
     public:
        ResultSet();
        ~ResultSet();

        Result<float> get(const std::string& field_name, const std::string& for_field = "") const;
        Result<std::string> get_as_strs(const std::string& field_name, const std::string& for_field = "") const;
        bool is_string(const std::string& field_name) const;

     protected:
        Address get_v_ptr() override;
    };
}  // namespace bufr
