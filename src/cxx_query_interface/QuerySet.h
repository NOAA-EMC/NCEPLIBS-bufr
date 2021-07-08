//
// Created by rmclaren on 6/30/21.
//

#pragma once

#include "FortranObject.h"

#include <string>


namespace bufr
{
    class QuerySet : public FortranObject
    {
     public:
        QuerySet();
        ~QuerySet();

        void add(const std::string& query_str, const std::string& query_name);
        void print();

     protected:
        Address get_v_ptr() final;
    };
}  // namespace bufr

