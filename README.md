![Status](https://github.com/NOAA-EMC/NCEPLIBS-bufr/workflows/Build%20and%20Test/badge.svg)

## NCEPLIBS-bufr library

The NCEPLIBS-bufr library contains routines and utilites for working with the WMO BUFR format.
It is part of the [NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For full documentation of the library, see https://noaa-emc.github.io/NCEPLIBS-bufr/.

## Authors

NCEP/EMC developers.

Code manager: Jeff Ator

## Supported Compilers

- GNU
- Intel
- Clang

## How to Build and Install

<pre>
git clone https://github.com/noaa-emc/nceplibs-bufr
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=path1 -DMASTER_TABLE_DIR=path2 ../nceplibs-bufr
make -j4
ctest
make install
</pre>

Both `path1` and `path2` may be full or relative pathnames
on the system, up to a maximum of 90 characters each.

Installation of the library and utilities will be under `path1`.
Installation of the master BUFR tables will be under `path2`, or
under `path1` if `-DMASTER_TABLE_DIR=path2` is omitted
from the above cmake command.

For Intel and GNU compilers, the library is built with three different
type/size combinations:

- "4" = 4-byte integers and 4-byte reals
- "8" = 8-byte integers and 8-byte reals
- "d" = 4-byte integers and 8-byte reals

All type/size combinations are built and installed.

If Python interoperability is desired, `-DENABLE_PYTHON=ON` can also
be added to the above cmake command.  However, version 3 of Python
must be installed and available on the system.

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
