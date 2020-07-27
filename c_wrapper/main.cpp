#include <time.h>
#include <math.h>
#include <iostream>
#include <iomanip>
#include <vector>
#include "NCEPLIB-bufr.interface.h"

using namespace std;

static const int FORTRAN_FILE_UNIT = 10;
static const char* HEADER_1_MNEMONIC = "SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS";
static const char* HEADER_2_MNEMONIC = "SAZA SOZA BEARAZ SOLAZI";

typedef struct
{
  double satid;
  double ifov;
  double year;
  double month;
  double day;
  double hour;
  double minute;
  double second;
  double clat;
  double clon;
  double clath;
  double clonh;
  double terrain;
}  Header_1;

typedef struct
{
  double lza;
  double sza;
  double sat_aziang;
  double sol_aziang;
}  Header_2;

typedef struct
{
	int nchanl;
  int satid;
  int ifov;
  int dtime[6];
  double olat, olon;
  double terrain;
  double lza, sza;
  double sat_aziang, sol_aziang;
  shared_ptr<double> bufr_data;
}  BufrData;

typedef vector<BufrData> BufrDataList;

int count_messages(const string filepath)
{
  open_f90(FORTRAN_FILE_UNIT, filepath.c_str());
  openbf_f90(FORTRAN_FILE_UNIT, "IN", FORTRAN_FILE_UNIT);

  int num_msgs = 0;
  int num_reports = 0;

  char* subset;
  int iddate, result;
  while (ireadmg_f90(FORTRAN_FILE_UNIT, subset, &iddate) == 0)
  {
      num_msgs++;

      while (ireadsb_f90(FORTRAN_FILE_UNIT) == 0)
      {
          num_reports++;
      }
  } 

  closbf_f90(FORTRAN_FILE_UNIT);
  close_f90(FORTRAN_FILE_UNIT);

  cout << filepath << endl;
  cout << "contains " << num_msgs << " messages and " << num_reports << " reports." << endl;

  return num_reports;
}

BufrDataList read_bufrdata(const string filepath, int num_channels)
{
  BufrDataList bufrDataList;

  const int HEADER_1_SIZE = sizeof(Header_1)/sizeof(double);
  const int HEADER_2_SIZE = sizeof(Header_2)/sizeof(double);

  open_f90(FORTRAN_FILE_UNIT, filepath.c_str());
  openbf_f90(FORTRAN_FILE_UNIT, "IN", FORTRAN_FILE_UNIT);

  char* subset;
  int iddate;
  int result;

  while (ireadmg_f90(FORTRAN_FILE_UNIT, subset, &iddate) == 0)
  {
    while (ireadsb_f90(FORTRAN_FILE_UNIT) == 0)
    {
      BufrData bufrData;

      //Read header 1 data
      Header_1* header1 = new Header_1;
      ufbint_f90(FORTRAN_FILE_UNIT, (void**)&header1, HEADER_1_SIZE, 1, &result, HEADER_1_MNEMONIC);

      bufrData.nchanl = num_channels;
      bufrData.satid = header1->satid;
      bufrData.ifov = header1->ifov;

      bufrData.dtime[0] = header1->year; //year
      bufrData.dtime[1] = header1->month; //month
      bufrData.dtime[2] = header1->day; //day
      bufrData.dtime[3] = header1->hour; //hour
      bufrData.dtime[4] = header1->minute; //minute
      bufrData.dtime[5] = header1->second; //second

      double lat;
      double lon;
      if (abs(header1->clath) <= 90 && abs(header1->clonh) <= 360)
      {
        lat = header1->clath;
        lon = header1->clon;
      }
      else if (abs(header1->clat) <= 90 && abs(header1->clon) <= 360)
      {
        lat = header1->clat;
        lon = header1->clon;
      }

      if (lon < 0) lon = lon + 360;
      if (lon >= 360) lon = lon - 360;

      bufrData.olat = lat;
      bufrData.olon = lon;

      bufrData.terrain = 0.01 * abs(header1->terrain);

      free(header1);

      //Read header 2 data
      Header_2* header2 = new Header_2;
      ufbint_f90(FORTRAN_FILE_UNIT, (void**)&header2, HEADER_2_SIZE, 1, &result, HEADER_2_MNEMONIC);

      bufrData.lza = header2->lza;
      bufrData.sza = header2->sza;
      bufrData.sat_aziang = header2->sat_aziang;
      bufrData.sol_aziang = header2->sol_aziang;

      free(header2);

      //Read bufr data
      double* tmbr_data = new double[num_channels];
      ufbrep_f90(FORTRAN_FILE_UNIT, (void**)&tmbr_data, 1, num_channels, &result, "TMBR");
      bufrData.bufr_data.reset(tmbr_data);

      //Store the result
      bufrDataList.push_back(bufrData);
    }
  } 

  closbf_f90(FORTRAN_FILE_UNIT);
  close_f90(FORTRAN_FILE_UNIT);

  return bufrDataList;
}

void printBufrData(BufrDataList bufrDataList, int maxLinesToPrint)
{
  int idx = 0;
  cout.precision(5);
  for (auto bufrData : bufrDataList)
  {
    for (int channel_idx = 0; channel_idx<bufrData.nchanl; channel_idx++)
    {
      cout << setw(7)  << bufrData.bufr_data.get()[channel_idx] << " ";
    }

    cout << endl;

    if (maxLinesToPrint > 0 && ++idx >= maxLinesToPrint) break;
  }
}

int main(int argc, const char** argv) 
{
  clock_t tStart = clock();

  const string filepath = "/Users/rmclaren/Work/sample-bufr-data/gdas/gdas.20200704/12/gdas.t12z.1bmhs.tm00.bufr_d";

  int num_channels = 15;

  // int num_reports = count_messages(filepath);
  auto bufrDataList = read_bufrdata(filepath, num_channels);
  printBufrData(bufrDataList, 10);

  printf("Time taken: %.2fs\n", (double)(clock() - tStart)/CLOCKS_PER_SEC);
  return 0;
}
