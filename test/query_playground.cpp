//
// Created by rmclaren on 6/30/21.
//

#include <string>
#include <iostream>
#include <vector>

#include "cxx_query_interface/QuerySet.h"
#include "cxx_query_interface/ResultSet.h"
#include "cxx_query_interface/File.h"

template<typename T>
void print(const std::string& name, const std::vector<T>& vec)
{
    std::cout << name << ": ";
    for (auto val : vec)
    {
        std::cout << val << ", ";
    }
    std::cout << std::endl;
}

int main()
{
    auto file = bufr::File(
        "/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gnssro_kompsat5_20180415_00Z.bufr");

    auto query_set = bufr::QuerySet();
    query_set.add("*/CLATH", "Latitude");
    query_set.add("*/CLONH", "Longitude");
    query_set.print();

    auto result_set = file.execute(query_set, 5);

    auto latitude = result_set.get("Latitude");
    auto longitude = result_set.get("Longitude");

    print("Latitude", latitude);
    print("Longitude", longitude);

    file.close();
}
