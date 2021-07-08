//
// Created by rmclaren on 6/30/21.
//

#include "FortranObject.h"

namespace bufr
{
    void FortranObject::initialize(Address p, bool is_owned_by_me)
    {
        data_ptr_ = p;
        is_owned_ = is_owned_by_me;
        class_data_.vptr = get_v_ptr();
        class_data_.data = data_ptr_;
        class_data_ptr_ = &class_data_;
    }
}  // namespace bufr
