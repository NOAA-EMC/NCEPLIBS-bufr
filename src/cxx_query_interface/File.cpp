//
// Created by rmclaren on 7/1/21.
//

#include "File.h"

#include "FortranObject.h"
#include "bufr_interface.h"

extern "C"
{
    void __cxx_query_interface_MOD_execute_c(int, bufr::Address, int, bufr::Address);
}

namespace bufr
{
    File::File(const std::string& filename, bool isWmoFormat, const std::string& wmoTablePath) :
      fileUnit_(nextFileUnit()),
      fileUnitTable1_(0),
      fileUnitTable2_(0),
      filename_(filename),
      isWmoFormat_(isWmoFormat),
      wmoTablePath_(wmoTablePath)
    {
        if (isWmoFormat && !wmoTablePath_.empty())
        {
            fileUnitTable1_ = nextFileUnit();
            fileUnitTable2_ = nextFileUnit();
        }

        open();
    }

    ResultSet File::execute(const QuerySet& query_set, int next)
    {
        auto result_set = ResultSet();
        __cxx_query_interface_MOD_execute_c(fileUnit_,
                                            query_set.get_fortran_obj(),
                                            next,
                                            result_set.get_fortran_obj());
        return result_set;
    }

    void File::open()
    {
        open_f(fileUnit_, filename_.c_str());

        if (!isWmoFormat_)
        {
            openbf_f(fileUnit_, "IN", fileUnit_);
        }
        else
        {
            openbf_f(fileUnit_, "SEC3", fileUnit_);

            if (!wmoTablePath_.empty())
            {
                mtinfo_f(wmoTablePath_.c_str(), fileUnitTable1_, fileUnitTable2_);
            }
        }
    }

    void File::close()
    {
        closbf_f(fileUnit_);
        close_f(fileUnit_);
    }

    void File::rewind()
    {
        close();
        open();
    }

    int File::nextFileUnit()
    {
        static int lastFileUnit = 11; //  Numbers 12 and above are valid.
        return ++lastFileUnit;
    }
}  // namespace bufr
