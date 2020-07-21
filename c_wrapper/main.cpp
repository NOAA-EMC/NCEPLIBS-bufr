#include <math.h>
#include <iostream>
#include <vector>
#include "bufrlib_c_wrapper.h"

using namespace std;

static const int FORTRAN_FILE_UNIT = 10;
static const char* HEADER_1_MNEMONIC = "SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS";
static const char* HEADER_2_MNEMONIC = "SAZA SOZA BEARAZ SOLAZI";

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
  open_fortran_file(FORTRAN_FILE_UNIT, filepath.c_str());
  open_bufr(FORTRAN_FILE_UNIT, "IN", FORTRAN_FILE_UNIT);

  int num_msgs = 0;
  int num_reports = 0;

  char* subset;
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

  return num_reports;
}

BufrDataList read_bufrdata(const string filepath, int num_channels, int num_reps)
{
  BufrDataList bufrDataList;
  bufrDataList.reserve(num_reps);

  open_fortran_file(FORTRAN_FILE_UNIT, filepath.c_str());
  open_bufr(FORTRAN_FILE_UNIT, "IN", FORTRAN_FILE_UNIT);

  char* subset;
  int iddate;
  int result = 0;
  int hdr1b_size = 13;
  int hdr2b_size = 4;
  int dim_2 = 1;

  while (read_next_msg(FORTRAN_FILE_UNIT, subset, &iddate) == 0)
  {
    while (read_next_subset(FORTRAN_FILE_UNIT) == 0)
    {
      BufrData bufrData;

      double* header_data_1 = new double[hdr1b_size];
      ufbint(FORTRAN_FILE_UNIT, &header_data_1, &hdr1b_size, &dim_2, &result, HEADER_1_MNEMONIC);

      bufrData.nchanl = num_channels;
      bufrData.satid = header_data_1[0];
      bufrData.ifov = header_data_1[1];

      bufrData.dtime[0] = header_data_1[2]; //year
      bufrData.dtime[1] = header_data_1[3]; //month
      bufrData.dtime[2] = header_data_1[4]; //day
      bufrData.dtime[3] = header_data_1[5]; //hour
      bufrData.dtime[4] = header_data_1[6]; //minute
      bufrData.dtime[5] = header_data_1[7]; //second

      double lat;
      double lon;
      if (abs(header_data_1[10]) <= 90 && abs(header_data_1[11]) <= 360)
      {
        lat = header_data_1[10];
        lon = header_data_1[11];
      }
      else if (abs(header_data_1[8]) <= 90 && abs(header_data_1[9]) <= 360)
      {
        lat = header_data_1[8];
        lon = header_data_1[9];
      }

      if (lon < 0) lon = lon + 360;
      if (lon >= 360) lon = lon - 360;

      bufrData.olat = lat;
      bufrData.olon = lon;

      bufrData.terrain = 0.01 * abs(header_data_1[12]);

      free(header_data_1);

      double* header_data_2 = new double[hdr2b_size];
      ufbint(FORTRAN_FILE_UNIT, &header_data_2, &hdr2b_size, &dim_2, &result, HEADER_2_MNEMONIC);

      bufrData.lza = header_data_2[0];
      bufrData.sza = header_data_2[1];
      bufrData.sat_aziang = header_data_2[2];
      bufrData.sol_aziang = header_data_2[3];

      free(header_data_2);

      bufrData.bufr_data = make_shared<double>(num_channels);

      int dim_1 = 1;
      // double* tmbr_data = bufrData.bufr_data.get();

      double* tmbr_data = new double[num_channels];
      ufbrep(FORTRAN_FILE_UNIT, &tmbr_data, &dim_1, &num_channels, &result, "TMBR");
      bufrData.bufr_data.reset(tmbr_data);

      bufrDataList.push_back(bufrData);
    }
  } 

  close_bufr(FORTRAN_FILE_UNIT);
  close_fortran_file(FORTRAN_FILE_UNIT);

  return bufrDataList;
}

int main(int argc, const char** argv) 
{
  const string filepath = "/Users/rmclaren/Work/sample-bufr-data/gdas/gdas.20200704/12/gdas.t12z.1bhrs4.tm00.bufr_d";
  int num_reports = count_messages(filepath);
  auto bufrDataList = read_bufrdata(filepath, 15, num_reports);

  int idx = 0;
  cout.precision(5);
  for (auto bufrData : bufrDataList)
  {
    for (int channel_idx = 0; channel_idx<15; channel_idx++)
    {
      cout << bufrData.bufr_data.get()[channel_idx] << " ";
    }

    cout << endl;

    idx++;

    if (idx >= 10)
    {
      break;
    }
  }


  return 0;
}
