//
// Created by rmclaren on 6/30/21.
//

#pragma once

#include "FortranObject.h"

#include <string>
#include <vector>

namespace bufr
{
    class ResultSet : public FortranObject
    {
     public:
        ResultSet();
        ~ResultSet();

        std::vector<float> get(const std::string& field_name, const std::string& for_field = "") const;

     protected:
        Address get_v_ptr() override;
    };
}  // namespace bufr
