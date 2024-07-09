![Status](https://github.com/NOAA-EMC/NCEPLIBS-bufr/workflows/Build%20and%20Test/badge.svg)

## NCEPLIBS-bufr library

The NCEPLIBS-bufr library contains routines and utilites for working
with the [WMO
BUFR](https://library.wmo.int/index.php?lvl=notice_display&id=10684#.Y70OSNLMJH7)
format. It is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For full documentation of the library, see https://noaa-emc.github.io/NCEPLIBS-bufr/.

NCEPLIBS-bufr is used by numerous other projects including:
- [gfs-utils](https://github.com/NOAA-EMC/gfs-utils) from NOAA's global
  workflow.
- NCAR's [Gridpoint Statistical Interpolation
  (GSI)](https://ral.ucar.edu/solutions/products/gridpoint-statistical-interpolation-gsi).
- [obsproc](https://github.com/NOAA-EMC/obsproc) in the [NOAA
  Operational Model Archive and Distribution System (NOMADS)](https://nomads.ncep.noaa.gov/).
- [prepobs](https://github.com/NOAA-EMC/prepobs) from the [The NCEP Production Suite](https://www.nco.ncep.noaa.gov/pmb/prod_overview/).
- [bufr-dump](https://github.com/NOAA-EMC/bufr-dump) which is run by
  all of the NOAA model data assimilation systems when it's time to
  collect data for use in the analyses.
- the [Global Ensemble Forecast
  System(GEFS)](https://www.ncei.noaa.gov/products/weather-climate-models/global-ensemble-forecast).
- the [High-Resolution Rapid Refresh model
  (HRRR)](https://rapidrefresh.noaa.gov/hrrr/).
- NOAA's [Rapid Refresh (RAP)](https://rapidrefresh.noaa.gov/) assimilation/modeling system.

To submit bug reports, feature requests, or other code-related issues including installation and usage questions, please create a [GitHub issue](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues). For general NCEPLIBS inquiries, contact [Edward Hartnett](mailto:edward.hartnett@noaa.gov) (secondary point of contact [Alex Richert](mailto:alexander.richert@noaa.gov)).

## Authors

Jack Woollen, Jeff Ator, Dennis Keyser, Stacey Bender, Diane Stokes, Edward Hartnett,
Jeff Whitaker, Rahul Mahajan, Alex Richert, Ron McLaren, and Dom Heinzeller.

Code manager: [Jeff Ator](mailto:jeff.ator@noaa.gov)

## How to Build and Install

Download tarball from
[Releases](https://github.com/NOAA-EMC/NCEPLIBS-bufr/releases) and
unpack.

<pre>
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=path1 -DMASTER_TABLE_DIR=path2 ..
make -j4
ctest
make install
</pre>

Both `path1` and `path2` may be full or relative pathnames
on the system, up to a maximum of 240 characters each.

Installation of the library and utilities will be under `path1`.
Installation of the master BUFR tables will be under `path2`, or
under `path1` if `-DMASTER_TABLE_DIR=path2` is omitted
from the above cmake command.

If Python interoperability is desired, then `-DENABLE_PYTHON=ON` can also
be added to the above cmake command.  However, version 3 of Python
must be installed and available on the system.

When building the library, automated CI testing is included by default.
If this is not desired, then it can be disabled by adding `-DBUILD_TESTING=OFF`
to the above cmake command.

When building the library, associated utilities are included by default.
If these are not desired, then they can be disabled by adding `-DBUILD_UTILS=OFF`
to the above cmake command.

## References

Hartnett, E., Ator, J, Lei, H., Richert, A., Woollen, J., King, A.,
Hartnett, A., [NCEPLIBS GRIB and BUFR Libraries: Maintaining and
Modernizing NOAA's Libraries for WMO Data
Formats](https://www.researchgate.net/publication/376390180_NCEPLIBS_GRIB_and_BUFR_Libraries_Maintaining_and_Modernizing_NOAA's_Libraries_for_WMO_Data_Formats),
American Geophysical Union (AGU) 2023. (See also
[poster](https://www.researchgate.net/publication/376582005_Poster_-_IN51B-0416_NCEPLIBS_GRIB_and_BUFR_Libraries_Maintaining_and_Modernizing_NOAA's_Libraries_for_WMO_Data_Formats)).

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.
