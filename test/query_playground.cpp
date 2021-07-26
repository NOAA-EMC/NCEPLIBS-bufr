//
// Created by rmclaren on 6/30/21.
//

#include <string>
#include <iostream>
#include <vector>

#include "QuerySet.h"
#include "ResultSet.h"
#include "File.h"

void print(const std::string& name, const std::shared_ptr<bufr::ResultBase>& result)
{
    std::cout << name << ": ";
    result->print();
    std::cout << std::endl;
}

void test_get_data_mhs()
{
//    auto file = bufr::File(
//        "/home/rmclaren/Work/ioda-bundle/iodaconv/test/testinput/gnssro_kompsat5_20180415_00Z.bufr");
//
//    auto query_set = bufr::QuerySet();
//    query_set.add("*/CLATH", "Latitude");
//    query_set.add("*/CLONH", "Longitude");
//    query_set.print();

    auto file = bufr::File(
        "/home/rmclaren/Work/ioda-bundle/iodaconv/test/testinput/gdas.t18z.1bmhs.tm00.bufr_d");

    auto query_set = bufr::QuerySet();
//    query_set.add("*/FOVN", "fovn");
//    query_set.add("*/HOLS", "height");
    query_set.add("*/CLAT", "latitude");
    query_set.add("*/CLON", "longitude");
//    query_set.add("*/LSQL", "lsql");
//    query_set.add("*/SOLAZI", "saz");
//    query_set.add("*/SOZA", "sza");
//    query_set.add("*/BEARAZ", "vaz");
//    query_set.add("*/SAZA", "vza");
//
    auto result_set = file.execute(query_set, 15);
//
    auto latitude = result_set.get("latitude");
    auto longitude = result_set.get("longitude");

    print("Latitude", latitude);
    print("Longitude", longitude);

    file.close();
}

void test_get_data_chars()
{
    auto file = bufr::File(
        "/home/rmclaren/Work/ioda-bundle/iodaconv/test/testinput/bufr_satwnd_old_format.bufr");

    auto query_set = bufr::QuerySet();
    query_set.add("*/BORG", "borg");

    auto result_set = file.execute(query_set, 15);

    auto borg = result_set.get("borg");

    print("BORG", borg);

    file.close();
}


int main()
{
    test_get_data_chars();
}
