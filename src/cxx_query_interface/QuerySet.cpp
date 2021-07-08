//
// Created by rmclaren on 6/30/21.
//

#include "QuerySet.h"

extern int __modq_query_set_MOD___vtab_modq_query_set_Queryset;

extern "C"
{
    void __cxx_query_interface_MOD_allocate__query_set(bufr::Address *caddr);
    void __cxx_query_interface_MOD_deallocate__query_set(bufr::Address caddr);

    void __modq_query_set_MOD_query_set__add(bufr::Address self,
                                             const char* query,
                                             const char* name,
                                             std::size_t query_len,
                                             std::size_t name_len);

    void __modq_query_set_MOD_query_set__print(bufr::Address self);
};

namespace bufr
{

    QuerySet::QuerySet()
    {
        Address data_ptr = nullptr;
        __cxx_query_interface_MOD_allocate__query_set(&data_ptr);
        initialize(data_ptr, true);
    }

    QuerySet::~QuerySet()
    {
        if (is_owned_)
        {
            __cxx_query_interface_MOD_deallocate__query_set(data_ptr_);
        }
    }

    void QuerySet::add(const std::string& query_str, const std::string& query_name)
    {
        __modq_query_set_MOD_query_set__add(class_data_ptr_,
                                            query_str.c_str(),
                                            query_name.c_str(),
                                            query_str.size(),
                                            query_name.size());
    }

    void QuerySet::print()
    {
        __modq_query_set_MOD_query_set__print(class_data_ptr_);
    }

    Address QuerySet::get_v_ptr()
    {
        return &__modq_query_set_MOD___vtab_modq_query_set_Queryset;
    }
}  // namespace bufr



 
