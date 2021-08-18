//
// Created by rmclaren on 6/30/21.
//

#pragma once

#include "FortranObject.h"

#include <iostream>
#include <memory>
#include <string>
#include <vector>


namespace bufr
{
    struct ResultBase {
        virtual ~ResultBase() {}
        virtual void print() = 0;
    };

    template <typename T>
    struct Result : ResultBase
    {
        std::vector<T> data;
        std::vector<std::size_t> dims;

        void print() final
        {
            for (auto val : data)
            {
                std::cout << val << ", ";
            }
        }
    };

    class ResultSet : public FortranObject
    {
     public:
        ResultSet();
        ~ResultSet();

        std::shared_ptr<ResultBase> get(const std::string& field_name, 
                                        const std::string& group_by_field = "") const;

     protected:
        Address get_v_ptr() override;
    };
}  // namespace bufr
