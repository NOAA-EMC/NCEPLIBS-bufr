#include <iostream>
#include "bufrlib_c_wrapper.h"

using namespace std;

static const int FORTRAN_FILE_UNIT = 10;

void count_messages(string filepath)
{
    open_fortran_file(FORTRAN_FILE_UNIT, filepath.c_str());
    open_bufr(FORTRAN_FILE_UNIT, "IN", FORTRAN_FILE_UNIT);

    int num_msgs = 0;
    int num_reports = 0;

    char subset[20];
    int iddate, result;
    while (read_next_msg(FORTRAN_FILE_UNIT, subset, &iddate) == 0)
    {
        num_msgs++;

        while (read_next_subset(FORTRAN_FILE_UNIT) == 0)
        {
            num_reports++;
        }
    } 

    close_bufr(FORTRAN_FILE_UNIT);
    close_fortran_file(FORTRAN_FILE_UNIT);

    cout << filepath << endl;
    cout << "contains " << num_msgs << " messages and " << num_reports << " reports." << endl;
}

int main(int argc, const char** argv) 
{
    string filepath = "/Users/rmclaren/Work/sample-bufr-data/gdas/gdas.20200704/12/gdas.t12z.1bhrs4.tm00.bufr_d";
    count_messages(filepath);

    return 0;
}
