//
// Created by rmclaren on 7/1/21.
//

#pragma once

#include <string>

#include "QuerySet.h"
#include "ResultSet.h"

namespace bufr
{
    class File
    {
     public:
        File() = delete;
        File(const std::string& filename,
             bool isWmoFormat=false,
             const std::string& wmoTablePath="");

        ResultSet execute(const QuerySet& query_set, int next=0);
        void open();
        void close();
        void rewind();

     private:
        bool isOpen_;
        bool isWmoFormat_;
        int fileUnit_;
        int fileUnitTable1_;
        int fileUnitTable2_;
        std::string filename_;
        std::string wmoTablePath_;

        int nextFileUnit();
    };
}  // namespace bufr
