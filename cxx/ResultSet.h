//
// Created by rmclaren on 6/30/21.
//

#pragma once

#include "FortranObject.h"

#include <string>
#include <vector>

namespace bufr
{
    struct Result
    {
        std::vector<float> data;
        int dimRows;
        int dimCols;
        int dimZ;
    };

    class ResultSet : public FortranObject
    {
     public:
        ResultSet();
        ~ResultSet();

        Result get(const std::string& field_name, const std::string& for_field = "") const;

     protected:
        Address get_v_ptr() override;
    };
}  // namespace bufr
