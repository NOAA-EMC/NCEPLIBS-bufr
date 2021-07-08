//
// Created by rmclaren on 6/30/21.
//

#pragma once


namespace bufr
{
    typedef void(*generic_fpointer)(void);
    typedef void* Address;

    struct FortranClassContainer
    {
        Address data;
        Address vptr;
    };

    class FortranObject
    {
     public:
        Address get_fortran_obj() const { return data_ptr_; }

     protected:
        Address data_ptr_;
        bool is_owned_;
        FortranClassContainer class_data_;
        FortranClassContainer* class_data_ptr_;

        void initialize(Address p, bool memOwn);

        virtual Address get_v_ptr() = 0;
    };
}  // namespace bufr

