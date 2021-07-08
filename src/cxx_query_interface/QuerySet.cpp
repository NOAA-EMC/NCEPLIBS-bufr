//
// Created by rmclaren on 6/30/21.
//

#include "QuerySet.h"

#include "query_interface.h"


namespace bufr
{
    QuerySet::QuerySet()
    {
        Address data_ptr = nullptr;
        query_set__allocate_f(&data_ptr);
        initialize(data_ptr, true);
    }

    QuerySet::~QuerySet()
    {
        if (is_owned_)
        {
            query_set__deallocate_f(data_ptr_);
        }
    }

    void QuerySet::add(const std::string& query_str, const std::string& query_name)
    {
        query_set__add_f(class_data_ptr_, query_str.c_str(), query_name.c_str());
    }

    void QuerySet::print()
    {
        query_set__print_f(class_data_ptr_);
    }

    Address QuerySet::get_v_ptr()
    {
        return &__modq_query_set_MOD___vtab_modq_query_set_Queryset;
    }
}  // namespace bufr



 
